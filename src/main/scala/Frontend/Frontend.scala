import chisel3._
import chisel3.util._
import Zircon_Config.Fetch._
import Zircon_Config.Decode._

class Frontend_Dispatch_IO extends Bundle {
    val inst_pkg = Vec(ndecode, Decoupled(new Frontend_Package))
}
class Frontend_Commit_IO extends Bundle {
    val rnm     = new Rename_Commit_IO
    val npc     = new NPC_Commit_IO
    val fq      = new Fetch_Queue_Commit_IO
    
}
class Frontend_Memory_IO extends Bundle {
    val l2      = Flipped(new L2_ICache_IO)
}

class Frontend_IO extends Bundle {
    val dsp     = new Frontend_Dispatch_IO
    val mem     = new Frontend_Memory_IO
    val cmt     = new Frontend_Commit_IO
}

class Frontend extends Module {
    val io = IO(new Frontend_IO)

    val npc = Module(new NPC)
    val ic  = Module(new ICache)
    val pd  = Module(new Pre_Decoders)
    val fq  = Module(new Fetch_Queue)
    val rnm = Module(new Rename)
    val dcd = VecInit.fill(ndecode)(Module(new Decoder).io)
    val pc  = RegInit(0x7FFFFFF0L.U(32.W))
    
    /* Previous Fetch Stage */
    val inst_pkg_pf = WireDefault(VecInit.fill(nfetch)(0.U.asTypeOf(new Frontend_Package)))

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
    inst_pkg_pf.foreach{ pkg => 
        pkg.pred_info.jump_en := false.B
        pkg.pred_info.offset := 4.U
        pkg.pred_info.vld := false.B
        pkg.valid := true.B
    }

    /* Fetch Stage */
    val inst_pkg_fc = WireDefault(ShiftRegister(
        inst_pkg_pf, 
        1, 
        0.U.asTypeOf(Vec(nfetch, new Frontend_Package)), 
        (fq.io.enq(0).ready || pd.io.npc.flush) && !npc.io.ic.miss && io.dsp.inst_pkg(0).ready || io.cmt.npc.flush
    ))

    // icache mmu TODO: add mmu
    ic.io.mmu.paddr        := pc
    ic.io.mmu.uncache      := false.B
    ic.io.pp.stall         := !fq.io.enq(0).ready
    ic.io.pp.flush         := io.cmt.npc.flush || pd.io.npc.flush
    io.mem.l2              <> ic.io.l2
    inst_pkg_fc.zipWithIndex.foreach{ case (pkg, i) => pkg.pc := pc + (i * 4).U }

    /* Previous Decode Stage */
    val inst_pkg_pd = WireDefault(ShiftRegister(
        Mux(io.cmt.fq.flush || pd.io.npc.flush, 0.U.asTypeOf(Vec(nfetch, new Frontend_Package)), inst_pkg_fc), 
        1, 
        0.U.asTypeOf(Vec(nfetch, new Frontend_Package)), 
        (fq.io.enq(0).ready || pd.io.npc.flush) && !npc.io.ic.miss && io.dsp.inst_pkg(0).ready || io.cmt.npc.flush
    ))
    inst_pkg_pd.zip(ic.io.pp.rdata).foreach{ case (pkg, inst) => pkg.inst := inst}
   
    // previous decoder
    pd.io.inst_pkg := inst_pkg_pd
    pd.io.rinfo.foreach{ rinfo => rinfo.ready := DontCare}
    inst_pkg_pd.zip(pd.io.rinfo).foreach{ case (pkg, rinfo) => pkg.rinfo := rinfo.bits }
    inst_pkg_pd.zip(pd.io.pred_offset).foreach{ case (pkg, pred_offset) => pkg.pred_offset := pred_offset }
    
    // fetch queue
    fq.io.enq.zipWithIndex.foreach{ case (enq, i) => 
        enq.valid   := pd.io.rinfo(i).valid && !ic.io.pp.miss
        enq.bits    := inst_pkg_pd(i)
    }
    fq.io.cmt   := io.cmt.fq
    val inst_pkg_dcd = WireDefault(VecInit(fq.io.deq.map(_.bits)))
    inst_pkg_dcd.zip(fq.io.deq).foreach{ case (pkg, deq) => pkg.valid := deq.valid }
    
    /* Decode and Rename Stage */
    rnm.io.fte.rinfo.zip(fq.io.deq).foreach{ case (rinfo, deq) => 
        rinfo.bits  := deq.bits.rinfo 
        rinfo.valid := deq.valid && io.dsp.inst_pkg(0).ready
        deq.ready   := rinfo.ready && io.dsp.inst_pkg(0).ready
    }
    rnm.io.cmt <> io.cmt.rnm
    inst_pkg_dcd.zip(rnm.io.fte.pinfo).foreach{ case (pkg, pinfo) => pkg.pinfo := pinfo }
    dcd.zip(inst_pkg_dcd).foreach{ case (dcd, pkg) => 
        dcd.inst    := pkg.inst
        dcd.rinfo   := pkg.rinfo
        pkg.op      := dcd.op
        pkg.imm     := dcd.imm
        pkg.func    := dcd.func
    }
    val inst_pkg_dsp = WireDefault(ShiftRegister(
        Mux(io.cmt.rnm.flst.flush || !rnm.io.fte.rinfo(0).ready, 0.U.asTypeOf(Vec(ndecode, new Frontend_Package)), inst_pkg_dcd), 
        1, 
        0.U.asTypeOf(Vec(ndecode, new Frontend_Package)), 
        io.dsp.inst_pkg(0).ready || io.cmt.fq.flush
    ))
    io.dsp.inst_pkg.zip(inst_pkg_dsp).foreach{ case (dsp, pkg) => 
        dsp.bits    := pkg 
        dsp.valid   := pkg.valid
    }
}