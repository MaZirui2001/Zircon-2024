import chisel3._
import chisel3.util._
import ZirconConfig.Fetch._
import ZirconConfig.Decode._
import ZirconUtil._

class FrontendDispatchIO extends Bundle {
    val instPkg = Vec(ndcd, Decoupled(new FrontendPackage))
}

class FrontendCommitIO extends Bundle {
    val rnm = new RenameCommitIO
    val npc = new NPCCommitIO
    val fq  = new FetchQueueCommitIO
    val pr  = new PredictCommitIO
}


class FrontendMemoryIO extends Bundle {
    val l2 = Flipped(new L2ICacheIO)
}

class FrontendDBGIO extends Bundle {
    val ic  = new ICacheDBG
    val fq  = new FetchQueueDBGIO
    val rnm = new RenameDBGIO
}

class FrontendIO extends Bundle {
    val bke = Flipped(new BackendFrontendIO)
    val dsp = new FrontendDispatchIO
    val mem = new FrontendMemoryIO
    val cmt = new FrontendCommitIO
    val dbg = new FrontendDBGIO
}

class Frontend extends Module {
    val io = IO(new FrontendIO)

    val npc = Module(new NPC)
    val pr  = Module(new Predict)
    val ic  = Module(new ICache)
    val pd  = Module(new PreDecoders)
    val fq  = Module(new FetchQueue)
    val rnm = Module(new Rename)
    val dcd = VecInit.fill(ndcd)(Module(new Decoder).io)
    val pc  = RegInit(0x7FFFFFF0L.U(32.W))
    
    /* Previous Fetch Stage */
    val instPkgPF = WireDefault(VecInit.fill(nfch)(0.U.asTypeOf(new FrontendPackage)))

    // npc
    npc.io.cmt       := io.cmt.npc
    npc.io.pd        := pd.io.npc
    npc.io.ic.miss   := ic.io.pp.miss
    npc.io.fq.ready  := fq.io.enq(0).ready
    npc.io.pc.pc     := pc
    npc.io.pr        := pr.io.npc
    pc               := npc.io.pc.npc

    // icache visit
    ic.io.pp.rreq    := fq.io.enq(0).ready || io.cmt.fq.flush || pd.io.npc.flush
    ic.io.pp.vaddr   := npc.io.pc.npc

    // TODO: add predictor
    val instValid = MuxLookup((npc.io.pc.npc >> 2).take(log2Ceil(nfch)), 0.U(4.W))(
        (0 until nfch).map(i =>
           ((nfch-1-i).U, ((1<<(i+1))-1).U)
        )
    ).asBools
    instPkgPF.zip(instValid)foreach{ case(pkg, valid) => 
        pkg.predInfo.jumpEn := false.B
        pkg.predInfo.offset := 4.U
        pkg.predInfo.vld := false.B
        pkg.valid := valid
    }

    /* Fetch Stage */
    val instPkgFC = WireDefault(ShiftRegister(
        instPkgPF, 
        1, 
        0.U.asTypeOf(Vec(nfch, new FrontendPackage)), 
        (fq.io.enq(0).ready || pd.io.npc.flush) && !npc.io.ic.miss || io.cmt.npc.flush
    ))
    // predict
    val instPkgPDIn = WireDefault(instPkgFC)
    pr.io.fc.gs.pc   := pc
    pr.io.fc.btbM.pc := pc
    pr.io.pd         <> pd.io.pr
    pr.io.cmt        <> io.cmt.pr

    instPkgPDIn.zipWithIndex.foreach{ case(pkg, i) =>
        pkg.predInfo.jumpEn := pr.io.fc.gs.jumpEnPredict(i)
        pkg.predInfo.offset := SE(pr.io.fc.btbM.rData(i).imm << 2)
        pkg.predInfo.vld    := instPkgFC(i).valid && pr.io.fc.btbM.rValid(i)
        pkg.valid           := instPkgFC(i).valid && pr.io.fc.validMask(i)
    }

    // icache mmu TODO: add mmu
    ic.io.mmu.paddr   := pc
    ic.io.mmu.uncache := false.B
    ic.io.pp.stall    := !fq.io.enq(0).ready
    ic.io.pp.flush    := io.cmt.npc.flush || pd.io.npc.flush
    io.mem.l2         <> ic.io.l2
    instPkgPDIn.zipWithIndex.foreach{ case (pkg, i) => pkg.pc := BLevelPAdder32(pc, (i * 4).U, 0.U).io.res }
    pr.io.fc.pc.zip(instPkgPDIn).foreach{ case (pc, pkg) => pc := pkg.pc }

    /* Previous Decode Stage */
    val instPkgPD = WireDefault(ShiftRegister(
        Mux(io.cmt.fq.flush || pd.io.npc.flush, 0.U.asTypeOf(Vec(nfch, new FrontendPackage)), instPkgPDIn), 
        1, 
        0.U.asTypeOf(Vec(nfch, new FrontendPackage)), 
        fq.io.enq(0).ready && !npc.io.ic.miss || io.cmt.npc.flush
    ))
    instPkgPD.zip(ic.io.pp.rdata).foreach{ case (pkg, inst) => pkg.inst := inst}
   
    // previous decoder
    pd.io.instPkg := instPkgPD
    pd.io.rinfo.foreach{ rinfo => rinfo.ready := DontCare}
    pd.io.praData := io.bke.rf.praData
    instPkgPD.zip(pd.io.rinfo).foreach{ case (pkg, rinfo) => pkg.rinfo := rinfo.bits }
    instPkgPD.zip(pd.io.predOffset).foreach{ case (pkg, predOffset) => pkg.predOffset := predOffset }
    
    // fetch queue
    fq.io.enq.zipWithIndex.foreach{ case (enq, i) => 
        enq.valid   := pd.io.rinfo(i).valid && !ic.io.pp.miss
        enq.bits    := instPkgPD(i)
    }
    fq.io.cmt   := io.cmt.fq
    val instPkgDCD = WireDefault(VecInit(fq.io.deq.map(_.bits)))
    instPkgDCD.zip(fq.io.deq).foreach{ case (pkg, deq) => pkg.valid := deq.valid }
    
    /* Decode and Rename Stage */
    rnm.io.fte.rinfo.zip(fq.io.deq).foreach{ case (rinfo, deq) => 
        rinfo.bits  := deq.bits.rinfo 
        rinfo.valid := deq.valid && io.dsp.instPkg(0).ready
        deq.ready   := rinfo.ready && io.dsp.instPkg(0).ready
    }
    rnm.io.cmt <> io.cmt.rnm

    instPkgDCD.zip(rnm.io.fte.pinfo).foreach{ case (pkg, pinfo) => pkg.pinfo := pinfo }
    dcd.zip(instPkgDCD).foreach{ case (dcd, pkg) => 
        dcd.inst    := pkg.inst
        dcd.rinfo   := pkg.rinfo
        pkg.op      := dcd.op
        pkg.imm     := dcd.imm
        pkg.func    := dcd.func
    }
    io.bke.rf.pra := rnm.io.fte.pra
    val instPkgDSP = WireDefault(ShiftRegister(
        Mux(io.cmt.rnm.fList.flush || !rnm.io.fte.rinfo(0).ready, 0.U.asTypeOf(Vec(ndcd, new FrontendPackage)), instPkgDCD), 
        1, 
        0.U.asTypeOf(Vec(ndcd, new FrontendPackage)), 
        io.dsp.instPkg(0).ready || io.cmt.fq.flush
    ))
    io.dsp.instPkg.zip(instPkgDSP).foreach{ case (dsp, pkg) => 
        dsp.bits    := pkg 
        dsp.valid   := pkg.valid
    }
    io.dbg.ic  := ic.io.dbg
    io.dbg.fq  := fq.io.dbg
    io.dbg.rnm := rnm.io.dbg
}