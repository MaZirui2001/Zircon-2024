import chisel3._
import chisel3.util._
import Zircon_Config.Issue._
import Zircon_Config.Commit._
// import Multiply._

class MulDiv_Commit_IO extends Pipeline_Commit_IO

class MulDiv_IQ_IO extends Pipeline_IQ_IO

// class MulDiv_Regfile_IO extends Pipeline_Regfile_IO

class MulDiv_Forward_IO extends Bundle {
    val inst_pkg_wb  = Output(new Backend_Package)
    val inst_pkg_ex  = Output(new Backend_Package)
    val src1_fwd     = Flipped(Decoupled(UInt(32.W)))
    val src2_fwd     = Flipped(Decoupled(UInt(32.W)))
}
class MulDiv_Wakeup_IO extends Bundle {
    val wake_ex3 = Output(new Wakeup_Bus_Pkg)
    val rply_in  = Input(new Replay_Bus_Pkg)
}

class MulDiv_Pipeline_IO extends Bundle {
    val iq  = new MulDiv_IQ_IO
    val rf  = Flipped(new Regfile_Single_IO)
    val cmt = new MulDiv_Commit_IO
    val fwd = new MulDiv_Forward_IO
    val wk  = new MulDiv_Wakeup_IO
}

class MulDiv_Pipeline extends Module {
    val io = IO(new MulDiv_Pipeline_IO)

    val mul = Module(new Mul_Booth2_Wallce)
    val div = Module(new SRT2)

    /* Issue Stage */
    val inst_pkg_is = WireDefault(io.iq.inst_pkg.bits)
    io.iq.inst_pkg.ready := !div.io.busy

    def seg_flush(inst_pkg: Backend_Package): Bool = {
        io.cmt.flush || io.wk.rply_in.replay && (inst_pkg.prj_lpv | inst_pkg.prk_lpv).orR
    }
    inst_pkg_is.prj_lpv := io.iq.inst_pkg.bits.prj_lpv 
    inst_pkg_is.prk_lpv := io.iq.inst_pkg.bits.prk_lpv

    /* Regfile Stage */
    val inst_pkg_rf = WireDefault(ShiftRegister(
        Mux(seg_flush(inst_pkg_is), 0.U.asTypeOf(new Backend_Package), inst_pkg_is), 
        1, 
        0.U.asTypeOf(new Backend_Package), 
        io.cmt.flush || io.wk.rply_in.replay || !div.io.busy
    ))
    io.rf.rd.prj := inst_pkg_rf.prj
    io.rf.rd.prk := inst_pkg_rf.prk
    inst_pkg_rf.src1 := io.rf.rd.prj_data
    inst_pkg_rf.src2 := io.rf.rd.prk_data

    /* Execute Stage 1 */
    val inst_pkg_ex1 = WireDefault(ShiftRegister(
        Mux(seg_flush(inst_pkg_rf), 0.U.asTypeOf(new Backend_Package), inst_pkg_rf), 
        1, 
        0.U.asTypeOf(new Backend_Package), 
        io.cmt.flush || io.wk.rply_in.replay || !div.io.busy
    ))

    // multiply
    mul.io.src1 := Mux(io.fwd.src1_fwd.valid, io.fwd.src1_fwd.bits, inst_pkg_ex1.src1)
    mul.io.src2 := Mux(io.fwd.src2_fwd.valid, io.fwd.src2_fwd.bits, inst_pkg_ex1.src2)
    mul.io.op   := inst_pkg_ex1.op(3, 0)
    mul.io.div_busy := div.io.busy

    // divide
    div.io.src1 := Mux(io.fwd.src1_fwd.valid, io.fwd.src1_fwd.bits, inst_pkg_ex1.src1)
    div.io.src2 := Mux(io.fwd.src2_fwd.valid, io.fwd.src2_fwd.bits, inst_pkg_ex1.src2)
    div.io.op   := inst_pkg_ex1.op(3, 0)

    // forward
    io.fwd.inst_pkg_ex := inst_pkg_ex1
    io.fwd.src1_fwd.ready := DontCare
    io.fwd.src2_fwd.ready := DontCare

    /* Execute Stage 2 */
    val inst_pkg_ex2 = WireDefault(ShiftRegister(
        Mux(io.cmt.flush, 0.U.asTypeOf(new Backend_Package), inst_pkg_ex1), 
        1, 
        0.U.asTypeOf(new Backend_Package), 
        io.cmt.flush || !div.io.busy
    ))

    /* Execute Stage 3 */
    val inst_pkg_ex3 = WireDefault(ShiftRegister(
        Mux(io.cmt.flush || div.io.busy, 0.U.asTypeOf(new Backend_Package), inst_pkg_ex2), 
        1, 
        0.U.asTypeOf(new Backend_Package), 
        true.B
    ))
    inst_pkg_ex3.rf_wdata := Mux(inst_pkg_ex3.op(2), div.io.res, mul.io.res)
    io.wk.wake_ex3 := (new Wakeup_Bus_Pkg)(inst_pkg_ex3, 0.U.asTypeOf(new Replay_Bus_Pkg))

    /* Write Back Stage */
    val inst_pkg_wb = WireDefault(ShiftRegister(
        Mux(io.cmt.flush, 0.U.asTypeOf(new Backend_Package), inst_pkg_ex3), 
        1, 
        0.U.asTypeOf(new Backend_Package), 
        true.B
    ))
    // rob
    io.cmt.widx.offset := UIntToOH(inst_pkg_wb.rob_idx.offset)
    io.cmt.widx.qidx   := UIntToOH(inst_pkg_wb.rob_idx.qidx)
    io.cmt.widx.high   := DontCare
    io.cmt.wen         := inst_pkg_wb.valid
    io.cmt.wdata       := (new ROB_Backend_Entry)(inst_pkg_wb)
    // regfile
    io.rf.wr.prd        := inst_pkg_wb.prd
    io.rf.wr.prd_vld    := inst_pkg_wb.rd_vld
    io.rf.wr.prd_data   := inst_pkg_wb.rf_wdata
    // forward
    io.fwd.inst_pkg_wb := inst_pkg_wb

}
