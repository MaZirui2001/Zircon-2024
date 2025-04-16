import chisel3._
import chisel3.util._
import ZirconConfig.Fetch._
import ZirconConfig.Decode._

class FetchQueueCommitIO extends Bundle {
    val flush = Input(Bool())
}

class FetchQueueDBGIO extends Bundle {
    val fullCycle  = Output(UInt(64.W))
    val emptyCycle = Output(UInt(64.W))
}

class FetchQueueIO extends Bundle {
    val enq = Vec(nfch, Flipped(Decoupled(new FrontendPackage)))
    val deq = Vec(ndcd, Decoupled(new FrontendPackage))
    val cmt = new FetchQueueCommitIO
    val dbg = new FetchQueueDBGIO
}

class FetchQueue extends Module {
    val io = IO(new FetchQueueIO)

    val q = Module(new ClusterIndexFIFO(new FrontendPackage, nfq, nfch, ndcd, 0, 0))

    q.io.enq   <> io.enq
    q.io.deq   <> io.deq
    q.io.flush := io.cmt.flush

    val fullCycleReg = RegInit(0.U(64.W))
    fullCycleReg     := fullCycleReg + !io.enq.map(_.ready).reduce(_ && _)
    io.dbg.fullCycle := fullCycleReg

    val emptyCycleReg = RegInit(0.U(64.W))
    emptyCycleReg     := emptyCycleReg + !io.deq.map(_.valid).reduce(_ || _)
    io.dbg.emptyCycle := emptyCycleReg
}