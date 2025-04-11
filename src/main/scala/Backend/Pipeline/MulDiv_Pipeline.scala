import chisel3._
import chisel3.util._
import ZirconConfig.Issue._
import ZirconConfig.Commit._
// import Multiply._

class MulDivCommitIO extends PipelineCommitIO

class MulDivIQIO extends PipelineIQIO

// class MulDivRegfileIO extends PipelineRegfileIO

class MulDivForwardIO extends Bundle {
    val instPkgWb  = Output(new BackendPackage)
    val instPkgEx  = Output(new BackendPackage)
    val src1Fwd    = Flipped(Decoupled(UInt(32.W)))
    val src2Fwd    = Flipped(Decoupled(UInt(32.W)))
}
class MulDivWakeupIO extends Bundle {
    val wakeEx3 = Output(new WakeupBusPkg)
    val rplyIn  = Input(new ReplayBusPkg)
}

class MulDivPipelineIO extends Bundle {
    val iq  = new MulDivIQIO
    val rf  = Flipped(new RegfileSingleIO)
    val cmt = new MulDivCommitIO
    val fwd = new MulDivForwardIO
    val wk  = new MulDivWakeupIO
}

class MulDivPipeline extends Module {
    val io = IO(new MulDivPipelineIO)

    val mul = Module(new MulBooth2Wallce)
    val div = Module(new SRT2)

    /* Issue Stage */
    val instPkgIs = WireDefault(io.iq.instPkg.bits)
    io.iq.instPkg.ready := !div.io.busy

    def segFlush(instPkg: BackendPackage): Bool = {
        io.cmt.flush || io.wk.rplyIn.replay && (instPkg.prjLpv | instPkg.prkLpv).orR
    }
    instPkgIs.prjLpv := io.iq.instPkg.bits.prjLpv 
    instPkgIs.prkLpv := io.iq.instPkg.bits.prkLpv

    /* Regfile Stage */
    val instPkgRf = WireDefault(ShiftRegister(
        Mux(segFlush(instPkgIs), 0.U.asTypeOf(new BackendPackage), instPkgIs), 
        1, 
        0.U.asTypeOf(new BackendPackage), 
        io.cmt.flush || io.wk.rplyIn.replay || !div.io.busy
    ))
    io.rf.rd.prj := instPkgRf.prj
    io.rf.rd.prk := instPkgRf.prk
    instPkgRf.src1 := io.rf.rd.prjData
    instPkgRf.src2 := io.rf.rd.prkData

    /* Execute Stage 1 */
    val instPkgEx1 = WireDefault(ShiftRegister(
        Mux(segFlush(instPkgRf), 0.U.asTypeOf(new BackendPackage), instPkgRf), 
        1, 
        0.U.asTypeOf(new BackendPackage), 
        io.cmt.flush || io.wk.rplyIn.replay || !div.io.busy
    ))

    // multiply
    mul.io.src1 := Mux(io.fwd.src1Fwd.valid, io.fwd.src1Fwd.bits, instPkgEx1.src1)
    mul.io.src2 := Mux(io.fwd.src2Fwd.valid, io.fwd.src2Fwd.bits, instPkgEx1.src2)
    mul.io.op   := instPkgEx1.op(3, 0)
    mul.io.divBusy := div.io.busy

    // divide
    div.io.src1 := Mux(io.fwd.src1Fwd.valid, io.fwd.src1Fwd.bits, instPkgEx1.src1)
    div.io.src2 := Mux(io.fwd.src2Fwd.valid, io.fwd.src2Fwd.bits, instPkgEx1.src2)
    div.io.op   := instPkgEx1.op(3, 0)

    // forward
    io.fwd.instPkgEx := instPkgEx1
    io.fwd.src1Fwd.ready := DontCare
    io.fwd.src2Fwd.ready := DontCare

    /* Execute Stage 2 */
    val instPkgEx2 = WireDefault(ShiftRegister(
        Mux(io.cmt.flush, 0.U.asTypeOf(new BackendPackage), instPkgEx1), 
        1, 
        0.U.asTypeOf(new BackendPackage), 
        io.cmt.flush || !div.io.busy
    ))

    /* Execute Stage 3 */
    val instPkgEx3 = WireDefault(ShiftRegister(
        Mux(io.cmt.flush || div.io.busy, 0.U.asTypeOf(new BackendPackage), instPkgEx2), 
        1, 
        0.U.asTypeOf(new BackendPackage), 
        true.B
    ))
    instPkgEx3.rfWdata := Mux(instPkgEx3.op(2), div.io.res, mul.io.res)
    io.wk.wakeEx3 := (new WakeupBusPkg)(instPkgEx3, 0.U.asTypeOf(new ReplayBusPkg))

    /* Write Back Stage */
    val instPkgWb = WireDefault(ShiftRegister(
        Mux(io.cmt.flush, 0.U.asTypeOf(new BackendPackage), instPkgEx3), 
        1, 
        0.U.asTypeOf(new BackendPackage), 
        true.B
    ))
    // rob
    io.cmt.widx.offset := UIntToOH(instPkgWb.robIdx.offset)
    io.cmt.widx.qidx   := UIntToOH(instPkgWb.robIdx.qidx)
    io.cmt.widx.high   := DontCare
    io.cmt.wen         := instPkgWb.valid
    io.cmt.wdata       := (new ROBBackendEntry)(instPkgWb)
    // regfile
    io.rf.wr.prd       := instPkgWb.prd
    io.rf.wr.prdVld    := instPkgWb.rdVld
    io.rf.wr.prdData   := instPkgWb.rfWdata
    // forward
    io.fwd.instPkgWb   := instPkgWb

}
