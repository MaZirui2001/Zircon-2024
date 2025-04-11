import chisel3._
import chisel3.util._
import ZirconConfig.Fetch._
import ZirconConfig.Decode._

class FrontendDispatchIO extends Bundle {
    val instPkg = Vec(ndcd, Decoupled(new FrontendPackage))
}
class FrontendCommitIO extends Bundle {
    val rnm     = new RenameCommitIO
    val npc     = new NPCCommitIO
    val fq      = new FetchQueueCommitIO
    
}
class FrontendMemoryIO extends Bundle {
    val l2      = Flipped(new L2ICacheIO)
}

class FrontendIO extends Bundle {
    val dsp     = new FrontendDispatchIO
    val mem     = new FrontendMemoryIO
    val cmt     = new FrontendCommitIO
}

class Frontend extends Module {
    val io = IO(new FrontendIO)

    val npc = Module(new NPC)
    val ic  = Module(new ICache)
    val pd  = Module(new PreDecoders)
    val fq  = Module(new FetchQueue)
    val rnm = Module(new Rename)
    val dcd = VecInit.fill(ndcd)(Module(new Decoder).io)
    val pc  = RegInit(0x7FFFFFF0L.U(32.W))
    
    /* Previous Fetch Stage */
    val instPkgPf = WireDefault(VecInit.fill(nfch)(0.U.asTypeOf(new FrontendPackage)))

    // npc
    npc.io.cmt              := io.cmt.npc
    npc.io.pd               := pd.io.npc
    npc.io.ic.miss          := ic.io.pp.miss
    npc.io.fq.ready         := fq.io.enq(0).ready
    npc.io.pc.pc            := pc
    pc                      := npc.io.pc.npc

    // icache visit
    ic.io.pp.rreq           := fq.io.enq(0).ready || io.cmt.fq.flush || pd.io.npc.flush
    ic.io.pp.vaddr          := npc.io.pc.npc

    // TODO: add predictor
    instPkgPf.foreach{ pkg => 
        pkg.predInfo.jumpEn := false.B
        pkg.predInfo.offset := 4.U
        pkg.predInfo.vld := false.B
        pkg.valid := true.B
    }

    /* Fetch Stage */
    val instPkgFc = WireDefault(ShiftRegister(
        instPkgPf, 
        1, 
        0.U.asTypeOf(Vec(nfch, new FrontendPackage)), 
        (fq.io.enq(0).ready || pd.io.npc.flush) && !npc.io.ic.miss || io.cmt.npc.flush
    ))

    // icache mmu TODO: add mmu
    ic.io.mmu.paddr        := pc
    ic.io.mmu.uncache      := false.B
    ic.io.pp.stall         := !fq.io.enq(0).ready
    ic.io.pp.flush         := io.cmt.npc.flush || pd.io.npc.flush
    io.mem.l2              <> ic.io.l2
    instPkgFc.zipWithIndex.foreach{ case (pkg, i) => pkg.pc := pc + (i * 4).U }

    /* Previous Decode Stage */
    val instPkgPd = WireDefault(ShiftRegister(
        Mux(io.cmt.fq.flush || pd.io.npc.flush, 0.U.asTypeOf(Vec(nfch, new FrontendPackage)), instPkgFc), 
        1, 
        0.U.asTypeOf(Vec(nfch, new FrontendPackage)), 
        fq.io.enq(0).ready && !npc.io.ic.miss || io.cmt.npc.flush
    ))
    instPkgPd.zip(ic.io.pp.rdata).foreach{ case (pkg, inst) => pkg.inst := inst}
   
    // previous decoder
    pd.io.instPkg := instPkgPd
    pd.io.rinfo.foreach{ rinfo => rinfo.ready := DontCare}
    instPkgPd.zip(pd.io.rinfo).foreach{ case (pkg, rinfo) => pkg.rinfo := rinfo.bits }
    instPkgPd.zip(pd.io.predOffset).foreach{ case (pkg, predOffset) => pkg.predOffset := predOffset }
    
    // fetch queue
    fq.io.enq.zipWithIndex.foreach{ case (enq, i) => 
        enq.valid   := pd.io.rinfo(i).valid && !ic.io.pp.miss
        enq.bits    := instPkgPd(i)
    }
    fq.io.cmt   := io.cmt.fq
    val instPkgDcd = WireDefault(VecInit(fq.io.deq.map(_.bits)))
    instPkgDcd.zip(fq.io.deq).foreach{ case (pkg, deq) => pkg.valid := deq.valid }
    
    /* Decode and Rename Stage */
    rnm.io.fte.rinfo.zip(fq.io.deq).foreach{ case (rinfo, deq) => 
        rinfo.bits  := deq.bits.rinfo 
        rinfo.valid := deq.valid && io.dsp.instPkg(0).ready
        deq.ready   := rinfo.ready && io.dsp.instPkg(0).ready
    }
    rnm.io.cmt <> io.cmt.rnm
    instPkgDcd.zip(rnm.io.fte.pinfo).foreach{ case (pkg, pinfo) => pkg.pinfo := pinfo }
    dcd.zip(instPkgDcd).foreach{ case (dcd, pkg) => 
        dcd.inst    := pkg.inst
        dcd.rinfo   := pkg.rinfo
        pkg.op      := dcd.op
        pkg.imm     := dcd.imm
        pkg.func    := dcd.func
    }
    val instPkgDsp = WireDefault(ShiftRegister(
        Mux(io.cmt.rnm.flst.flush || !rnm.io.fte.rinfo(0).ready, 0.U.asTypeOf(Vec(ndcd, new FrontendPackage)), instPkgDcd), 
        1, 
        0.U.asTypeOf(Vec(ndcd, new FrontendPackage)), 
        io.dsp.instPkg(0).ready || io.cmt.fq.flush
    ))
    io.dsp.instPkg.zip(instPkgDsp).foreach{ case (dsp, pkg) => 
        dsp.bits    := pkg 
        dsp.valid   := pkg.valid
    }
}