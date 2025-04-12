import chisel3._
import chisel3.util._
import ZirconConfig.Issue._
import ZirconConfig.Commit._
import ZirconConfig.Decode._
import ZirconConfig.RegisterFile._

abstract class PipelineCommitIO extends Bundle {
    val flush   = Input(Bool())
    val widx    = Output(new ClusterEntry(nrobQ, ndcd))
    val wen     = Output(Bool())
    val wdata   = Output(new ROBBackendEntry)
}
abstract class PipelineIQIO extends Bundle {
    val instPkg = Flipped(Decoupled(new BackendPackage))
}


class ArithCommitIO extends PipelineCommitIO

class ArithIQIO extends PipelineIQIO


class ArithForwardIO extends Bundle {
    val instPkgWb  = Output(new BackendPackage)
    val instPkgEx  = Output(new BackendPackage)
    val src1Fwd     = Flipped(Decoupled(UInt(32.W)))
    val src2Fwd     = Flipped(Decoupled(UInt(32.W)))
}

class ArithWakeupIO extends Bundle {
    val wakeIssue = Output(new WakeupBusPkg)
    val wakeRf    = Output(new WakeupBusPkg)
    val rplyIn    = Input(new ReplayBusPkg)
}

class ArithPipelineIO extends Bundle {
    val iq  = new ArithIQIO
    val rf  = Flipped(new RegfileSingleIO)
    val cmt = new ArithCommitIO
    val fwd = new ArithForwardIO
    val wk  = new ArithWakeupIO
}

class ArithPipeline extends Module {
    val io = IO(new ArithPipelineIO)

    val alu     = Module(new ALU)
    val branch  = Module(new Branch)

    /* Issue Stage */
    val instPkgIs         = WireDefault(io.iq.instPkg.bits)
    io.iq.instPkg.ready    := true.B

    def segFlush(instPkg: BackendPackage): Bool = {
        io.cmt.flush || io.wk.rplyIn.replay && (instPkg.prjLpv | instPkg.prkLpv).orR
    }
    instPkgIs.prjLpv := io.iq.instPkg.bits.prjLpv << 1
    instPkgIs.prkLpv := io.iq.instPkg.bits.prkLpv << 1

    // wakeup 
    io.wk.wakeIssue := (new WakeupBusPkg)(io.iq.instPkg.bits, io.wk.rplyIn)
    
    /* Regfile Stage */
    val instPkgRf = WireDefault(ShiftRegister(
        Mux(segFlush(io.iq.instPkg.bits), 0.U.asTypeOf(new BackendPackage), instPkgIs), 
        1, 
        0.U.asTypeOf(new BackendPackage), 
        true.B
    ))
    io.rf.rd.prj         := instPkgRf.prj
    io.rf.rd.prk         := instPkgRf.prk
    instPkgRf.src1       := io.rf.rd.prjData
    instPkgRf.src2       := io.rf.rd.prkData

    // wakeup
    io.wk.wakeRf := (new WakeupBusPkg)(instPkgRf, io.wk.rplyIn)
    
    /* Execute Stage */
    val instPkgEx = WireDefault(ShiftRegister(
        Mux(segFlush(instPkgRf), 0.U.asTypeOf(new BackendPackage), instPkgRf), 
        1, 
        0.U.asTypeOf(new BackendPackage), 
        true.B
    ))

    // alu
    alu.io.op               := instPkgEx.op(4, 0)
    alu.io.src1             := Mux(instPkgEx.op(6), instPkgEx.pc,  Mux(io.fwd.src1Fwd.valid, io.fwd.src1Fwd.bits, instPkgEx.src1))
    alu.io.src2             := Mux(instPkgEx.op(5), Mux(io.fwd.src2Fwd.valid, io.fwd.src2Fwd.bits, instPkgEx.src2), instPkgEx.imm)

    // branch
    branch.io.op            := instPkgEx.op(4, 0)
    branch.io.src1          := Mux(io.fwd.src1Fwd.valid, io.fwd.src1Fwd.bits, instPkgEx.src1)
    branch.io.src2          := Mux(io.fwd.src2Fwd.valid, io.fwd.src2Fwd.bits, instPkgEx.src2)
    branch.io.pc            := instPkgEx.pc
    branch.io.imm           := instPkgEx.imm
    branch.io.predOffset   := instPkgEx.predOffset

    instPkgEx.rfWdata    := alu.io.res
    instPkgEx.result      := branch.io.jumpTgt
    instPkgEx.jumpEn     := branch.io.realJp
    instPkgEx.predFail   := branch.io.predFail
    instPkgEx.nxtCmtEn  := !instPkgEx.op(4)

    // forward
    io.fwd.instPkgEx      := instPkgEx
    io.fwd.src1Fwd.ready   := DontCare
    io.fwd.src2Fwd.ready   := DontCare 

    /* Write Back Stage */
    val instPkgWb = WireDefault(ShiftRegister(
        Mux(io.cmt.flush, 0.U.asTypeOf(new BackendPackage), instPkgEx), 
        1, 
        0.U.asTypeOf(new BackendPackage), 
        true.B
    ))
    // rob
    io.cmt.widx.offset  := UIntToOH(instPkgWb.robIdx.offset)
    io.cmt.widx.qidx    := UIntToOH(instPkgWb.robIdx.qidx)
    io.cmt.widx.high    := DontCare
    io.cmt.wen          := instPkgWb.valid
    io.cmt.wdata        := (new ROBBackendEntry)(instPkgWb)
    // regfile
    io.rf.wr.prd        := instPkgWb.prd
    io.rf.wr.prdVld    := instPkgWb.rdVld
    io.rf.wr.prdData   := instPkgWb.rfWdata
    // forward
    io.fwd.instPkgWb  := instPkgWb
}