import chisel3._
import chisel3.util._
import ZirconConfig.Issue._
import ZirconConfig.Commit._
import ZirconConfig.Decode._
import ZirconConfig.RegisterFile._

abstract class PipelineROBIO extends Bundle {
    val widx    = Output(new ClusterEntry(nrobQ, ndcd))
    val wen     = Output(Bool())
    val wdata   = Output(new ROBEntry)
}

abstract class PipelineBDBIO extends Bundle {
    val widx    = Output(new ClusterEntry(nbdbQ, ndcd))
    val wen     = Output(Bool())
    val wdata   = Output(new BDBEntry)
}

abstract class PipelineIQIO extends Bundle {
    val instPkg = Flipped(Decoupled(new BackendPackage))
}

abstract class PipelineDBGIO extends Bundle {
}

class ArithROBIO extends PipelineROBIO {
    val ridx  = Output(new ClusterEntry(nrobQ, ndcd))
    val rdata = Input(new ROBEntry)
}

class ArithBDBIO extends PipelineBDBIO {
    val ridx  = Output(new ClusterEntry(nbdbQ, ndcd))
    val rdata = Input(new BDBEntry)
}

class ArithCommitIO extends Bundle {
    val rob   = new ArithROBIO
    val bdb   = new ArithBDBIO
    val flush = Input(Bool())
}

class ArithIQIO extends PipelineIQIO


class ArithForwardIO extends Bundle {
    val instPkgWB = Output(new BackendPackage)
    val instPkgEX = Output(new BackendPackage)
    val src1Fwd   = Flipped(Decoupled(UInt(32.W)))
    val src2Fwd   = Flipped(Decoupled(UInt(32.W)))
}

class ArithWakeupIO extends Bundle {
    val wakeIssue = Output(new WakeupBusPkg)
    val wakeRF    = Output(new WakeupBusPkg)
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
    val instPkgIs       = WireDefault(io.iq.instPkg.bits)
    io.iq.instPkg.ready := true.B

    def segFlush(instPkg: BackendPackage): Bool = {
        io.cmt.flush || io.wk.rplyIn.replay && (instPkg.prjLpv | instPkg.prkLpv).orR
    }
    instPkgIs.prjLpv := io.iq.instPkg.bits.prjLpv << 1
    instPkgIs.prkLpv := io.iq.instPkg.bits.prkLpv << 1

    // wakeup 
    io.wk.wakeIssue := (new WakeupBusPkg)(io.iq.instPkg.bits, io.wk.rplyIn)
    
    /* Regfile Stage */
    val instPkgRF = WireDefault(ShiftRegister(
        Mux(segFlush(io.iq.instPkg.bits), 0.U.asTypeOf(new BackendPackage), instPkgIs), 
        1, 
        0.U.asTypeOf(new BackendPackage), 
        true.B
    ))
    io.rf.rd.prj   := instPkgRF.prj
    io.rf.rd.prk   := instPkgRF.prk
    instPkgRF.src1 := io.rf.rd.prjData
    instPkgRF.src2 := io.rf.rd.prkData

    io.cmt.rob.ridx.offset := UIntToOH(instPkgRF.robIdx.offset)
    io.cmt.rob.ridx.qidx   := UIntToOH(instPkgRF.robIdx.qidx)
    io.cmt.rob.ridx.high   := DontCare

    io.cmt.bdb.ridx.offset := UIntToOH(instPkgRF.bdbIdx.offset)
    io.cmt.bdb.ridx.qidx   := UIntToOH(instPkgRF.bdbIdx.qidx)
    io.cmt.bdb.ridx.high   := DontCare

    instPkgRF.pc := io.cmt.rob.rdata.pc
    instPkgRF.predOffset := io.cmt.bdb.rdata.offset

    // wakeup
    io.wk.wakeRF := (new WakeupBusPkg)(instPkgRF, io.wk.rplyIn)
    
    /* Execute Stage */
    val instPkgEX = WireDefault(ShiftRegister(
        Mux(segFlush(instPkgRF), 0.U.asTypeOf(new BackendPackage), instPkgRF), 
        1, 
        0.U.asTypeOf(new BackendPackage), 
        true.B
    ))

    // alu
    alu.io.op            := instPkgEX.op(4, 0)
    alu.io.src1          := Mux(instPkgEX.op(6), instPkgEX.pc,  Mux(io.fwd.src1Fwd.valid, io.fwd.src1Fwd.bits, instPkgEX.src1))
    alu.io.src2          := Mux(instPkgEX.op(5), Mux(io.fwd.src2Fwd.valid, io.fwd.src2Fwd.bits, instPkgEX.src2), instPkgEX.imm)

    // branch
    branch.io.op         := instPkgEX.op(4, 0)
    branch.io.src1       := Mux(io.fwd.src1Fwd.valid, io.fwd.src1Fwd.bits, instPkgEX.src1)
    branch.io.src2       := Mux(io.fwd.src2Fwd.valid, io.fwd.src2Fwd.bits, instPkgEX.src2)
    branch.io.pc         := instPkgEX.pc
    branch.io.imm        := instPkgEX.imm
    branch.io.predOffset := instPkgEX.predOffset

    instPkgEX.rfWdata    := alu.io.res
    instPkgEX.result     := branch.io.jumpTgt
    instPkgEX.jumpEn     := branch.io.realJp
    instPkgEX.predFail   := branch.io.predFail
    instPkgEX.nxtCmtEn   := !instPkgEX.op(4)

    // forward
    io.fwd.instPkgEX     := instPkgEX
    io.fwd.src1Fwd.ready := DontCare
    io.fwd.src2Fwd.ready := DontCare 

    /* Write Back Stage */
    val instPkgWB = WireDefault(ShiftRegister(
        Mux(io.cmt.flush, 0.U.asTypeOf(new BackendPackage), instPkgEX), 
        1, 
        0.U.asTypeOf(new BackendPackage), 
        true.B
    ))
    // rob
    io.cmt.rob.widx.offset := UIntToOH(instPkgWB.robIdx.offset)
    io.cmt.rob.widx.qidx   := UIntToOH(instPkgWB.robIdx.qidx)
    io.cmt.rob.widx.high   := DontCare
    io.cmt.rob.wen         := instPkgWB.valid
    io.cmt.rob.wdata       := (new ROBEntry)(instPkgWB)

    // bdb
    io.cmt.bdb.widx.offset := UIntToOH(instPkgWB.bdbIdx.offset)
    io.cmt.bdb.widx.qidx   := UIntToOH(instPkgWB.bdbIdx.qidx)
    io.cmt.bdb.widx.high   := DontCare
    io.cmt.bdb.wen         := instPkgWB.valid && instPkgWB.op(4)
    io.cmt.bdb.wdata       := (new BDBEntry)(instPkgWB)

    // regfile
    io.rf.wr.prd       := instPkgWB.prd
    io.rf.wr.prdVld    := instPkgWB.rdVld
    io.rf.wr.prdData   := instPkgWB.rfWdata
    // forward
    io.fwd.instPkgWB   := instPkgWB
}