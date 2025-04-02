import chisel3._
import chisel3.util._
import CPU_Config.Decode._
import CPU_Config.Issue._
import Zircon_Util._
import CPU_Config.Commit._
import Adder.BLevel_PAdder32

class LS_Commit_IO extends Pipeline_Commit_IO {
    val dc      = new D_Commit_IO
}
class LS_IQ_IO extends Pipeline_IQ_IO 

// class LS_Regfile_IO extends Pipeline_Regfile_IO

class LS_Wakeup_IO extends Bundle {
    val wake_rf    = Output(new Wakeup_Bus_Pkg)
    val rply_in    = Input(new Replay_Bus_Pkg)
    val rply_out   = Output(new Replay_Bus_Pkg)
}
class LS_Forward_IO extends Bundle {
    val inst_pkg_wb  = Output(new Backend_Package)
}

class LS_Memory_IO extends Bundle {
    val l2         = Flipped(new L2_DCache_IO)
}

class LS_Pipeline_IO extends Bundle {
    val iq         = new LS_IQ_IO
    val rf         = Flipped(new Regfile_Single_IO)
    val cmt        = new LS_Commit_IO
    val fwd        = new LS_Forward_IO
    val wk         = new LS_Wakeup_IO
    val mem        = new LS_Memory_IO
}

class LS_Pipeline extends Module {
    val io = IO(new LS_Pipeline_IO)
    
    val agu = Module(new BLevel_PAdder32)
    val dc  = Module(new DCache)

    /* Issue Stage */
    val inst_pkg_is = WireDefault(io.iq.inst_pkg.bits)
    io.iq.inst_pkg.ready := !(dc.io.pp.miss || dc.io.pp.sb_full)
    
    def seg_flush(inst_pkg: Backend_Package): Bool = {
        io.cmt.flush || io.wk.rply_in.replay && (inst_pkg.prj_lpv | inst_pkg.prk_lpv).orR
    }
    inst_pkg_is.prj_lpv := io.iq.inst_pkg.bits.prj_lpv << 1
    inst_pkg_is.prk_lpv := io.iq.inst_pkg.bits.prk_lpv << 1
    
    /* Regfile Stage */
    val inst_pkg_rf = WireDefault(ShiftRegister(
        Mux(seg_flush(inst_pkg_is), 0.U.asTypeOf(new Backend_Package), inst_pkg_is), 
        1, 
        0.U.asTypeOf(new Backend_Package), 
        !(dc.io.pp.miss || dc.io.pp.sb_full) || io.cmt.flush || io.wk.rply_in.replay
    ))
    // regfile read
    io.rf.rd.prj           := inst_pkg_rf.prj
    io.rf.rd.prk           := inst_pkg_rf.prk
    // agu
    agu.io.src1         := io.rf.rd.prj_data
    agu.io.src2         := inst_pkg_rf.imm
    agu.io.cin          := 0.U
    inst_pkg_rf.src1    := agu.io.res
    inst_pkg_rf.src2    := io.rf.rd.prk_data
    // wakeup
    io.wk.wake_rf := (new Wakeup_Bus_Pkg)(inst_pkg_rf, true)

    dc.io.pp.rreq       := inst_pkg_rf.op(5)
    dc.io.pp.mtype      := inst_pkg_rf.op(2, 0)
    dc.io.pp.is_latest  := inst_pkg_rf.is_latest
    dc.io.pp.wreq       := inst_pkg_rf.op(6)
    dc.io.pp.wdata      := inst_pkg_rf.src2
    dc.io.pp.vaddr      := inst_pkg_rf.src1

    /* DCache Stage 1 */
    val inst_pkg_d1 = WireDefault(ShiftRegister(
        Mux(io.cmt.flush, 0.U.asTypeOf(new Backend_Package), inst_pkg_rf), 
        1, 
        0.U.asTypeOf(new Backend_Package), 
        !(dc.io.pp.miss || dc.io.pp.sb_full) || io.cmt.flush
    ))
    // dcache
    dc.io.cmt           := io.cmt.dc

    /* DCache Stage 2 */
    val inst_pkg_d2 = WireDefault(ShiftRegister(
        Mux(seg_flush(inst_pkg_d1), 0.U.asTypeOf(new Backend_Package), inst_pkg_d1), 
        1, 
        0.U.asTypeOf(new Backend_Package), 
        !(dc.io.pp.miss || dc.io.pp.sb_full) || io.cmt.flush
    ))
    // dcache
    dc.io.mmu.paddr     := inst_pkg_d2.src1
    // TODO: add mmu
    dc.io.mmu.uncache   := false.B
    dc.io.mmu.exception := 0.U(8.W)

    dc.io.l2            <> io.mem.l2
    inst_pkg_d2.nxt_cmt_en := inst_pkg_d2.op(6)

    // replay
    io.wk.rply_out.prd      := inst_pkg_d2.prd
    io.wk.rply_out.replay   := dc.io.pp.miss || dc.io.pp.sb_full

    /* Write Back Stage */
    val inst_pkg_wb = WireDefault(ShiftRegister(
        Mux(io.cmt.flush || dc.io.pp.miss || dc.io.pp.sb_full, 0.U.asTypeOf(new Backend_Package), inst_pkg_d2), 
        1, 
        0.U.asTypeOf(new Backend_Package), 
        true.B
    ))
    inst_pkg_wb.rf_wdata := dc.io.pp.rdata
    // rob
    io.cmt.widx.offset  := UIntToOH(inst_pkg_wb.rob_idx.offset)
    io.cmt.widx.qidx    := UIntToOH(inst_pkg_wb.rob_idx.qidx)
    io.cmt.widx.high    := DontCare
    io.cmt.wen          := inst_pkg_wb.valid
    io.cmt.wdata        := (new ROB_Backend_Entry)(inst_pkg_wb) 
    // regfile
    io.rf.wr.prd        := inst_pkg_wb.prd
    io.rf.wr.prd_vld    := inst_pkg_wb.rd_vld
    io.rf.wr.prd_data   := inst_pkg_wb.rf_wdata
    // forward
    io.fwd.inst_pkg_wb  := inst_pkg_wb
}
