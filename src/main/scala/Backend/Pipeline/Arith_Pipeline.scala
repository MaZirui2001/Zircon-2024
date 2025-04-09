import chisel3._
import chisel3.util._
import Zircon_Config.Issue._
import Zircon_Config.Commit._
import Zircon_Config.Decode._
import Zircon_Config.RegisterFile._

abstract class Pipeline_Commit_IO extends Bundle {
    val flush   = Input(Bool())
    val widx    = Output(new Cluster_Entry(nrob_q, ndcd))
    val wen     = Output(Bool())
    val wdata   = Output(new ROB_Backend_Entry)
}
abstract class Pipeline_IQ_IO extends Bundle {
    val inst_pkg = Flipped(Decoupled(new Backend_Package))
}


class Arith_Commit_IO extends Pipeline_Commit_IO

class Arith_IQ_IO extends Pipeline_IQ_IO


class Arith_Forward_IO extends Bundle {
    val inst_pkg_wb  = Output(new Backend_Package)
    val inst_pkg_ex  = Output(new Backend_Package)
    val src1_fwd     = Flipped(Decoupled(UInt(32.W)))
    val src2_fwd     = Flipped(Decoupled(UInt(32.W)))
}

class Arith_Wakeup_IO extends Bundle {
    val wake_issue = Output(new Wakeup_Bus_Pkg)
    val wake_rf    = Output(new Wakeup_Bus_Pkg)
    val rply_in    = Input(new Replay_Bus_Pkg)
}

class Arith_Pipeline_IO extends Bundle {
    val iq  = new Arith_IQ_IO
    val rf  = Flipped(new Regfile_Single_IO)
    val cmt = new Arith_Commit_IO
    val fwd = new Arith_Forward_IO
    val wk  = new Arith_Wakeup_IO
}

class Arith_Pipeline extends Module {
    val io = IO(new Arith_Pipeline_IO)

    val alu     = Module(new ALU)
    val branch  = Module(new Branch)

    /* Issue Stage */
    val inst_pkg_is         = WireDefault(io.iq.inst_pkg.bits)
    io.iq.inst_pkg.ready    := true.B

    def seg_flush(inst_pkg: Backend_Package): Bool = {
        io.cmt.flush || io.wk.rply_in.replay && (inst_pkg.prj_lpv | inst_pkg.prk_lpv).orR
    }
    inst_pkg_is.prj_lpv := io.iq.inst_pkg.bits.prj_lpv << 1
    inst_pkg_is.prk_lpv := io.iq.inst_pkg.bits.prk_lpv << 1

    // wakeup 
    io.wk.wake_issue := (new Wakeup_Bus_Pkg)(io.iq.inst_pkg.bits, io.wk.rply_in)
    
    /* Regfile Stage */
    val inst_pkg_rf = WireDefault(ShiftRegister(
        Mux(seg_flush(io.iq.inst_pkg.bits), 0.U.asTypeOf(new Backend_Package), inst_pkg_is), 
        1, 
        0.U.asTypeOf(new Backend_Package), 
        true.B
    ))
    io.rf.rd.prj           := inst_pkg_rf.prj
    io.rf.rd.prk           := inst_pkg_rf.prk
    inst_pkg_rf.src1       := io.rf.rd.prj_data
    inst_pkg_rf.src2       := io.rf.rd.prk_data

    // wakeup
    io.wk.wake_rf := (new Wakeup_Bus_Pkg)(inst_pkg_rf, io.wk.rply_in)
    
    /* Execute Stage */
    val inst_pkg_ex = WireDefault(ShiftRegister(
        Mux(seg_flush(inst_pkg_rf), 0.U.asTypeOf(new Backend_Package), inst_pkg_rf), 
        1, 
        0.U.asTypeOf(new Backend_Package), 
        true.B
    ))

    // alu
    alu.io.op               := inst_pkg_ex.op(4, 0)
    alu.io.src1             := Mux(inst_pkg_ex.op(6), inst_pkg_ex.pc,  Mux(io.fwd.src1_fwd.valid, io.fwd.src1_fwd.bits, inst_pkg_ex.src1))
    alu.io.src2             := Mux(inst_pkg_ex.op(5), Mux(io.fwd.src2_fwd.valid, io.fwd.src2_fwd.bits, inst_pkg_ex.src2), inst_pkg_ex.imm)

    // branch
    branch.io.op            := inst_pkg_ex.op(4, 0)
    branch.io.src1          := Mux(io.fwd.src1_fwd.valid, io.fwd.src1_fwd.bits, inst_pkg_ex.src1)
    branch.io.src2          := Mux(io.fwd.src2_fwd.valid, io.fwd.src2_fwd.bits, inst_pkg_ex.src2)
    branch.io.pc            := inst_pkg_ex.pc
    branch.io.imm           := inst_pkg_ex.imm
    branch.io.pred_offset   := inst_pkg_ex.pred_offset

    inst_pkg_ex.rf_wdata    := alu.io.res
    inst_pkg_ex.result      := branch.io.jump_tgt
    inst_pkg_ex.jump_en     := branch.io.real_jp
    inst_pkg_ex.pred_fail   := branch.io.pred_fail
    inst_pkg_ex.nxt_cmt_en  := !inst_pkg_ex.op(4)

    // forward
    io.fwd.inst_pkg_ex      := inst_pkg_ex
    io.fwd.src1_fwd.ready   := DontCare
    io.fwd.src2_fwd.ready   := DontCare 

    /* Write Back Stage */
    val inst_pkg_wb = WireDefault(ShiftRegister(
        Mux(io.cmt.flush, 0.U.asTypeOf(new Backend_Package), inst_pkg_ex), 
        1, 
        0.U.asTypeOf(new Backend_Package), 
        true.B
    ))
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