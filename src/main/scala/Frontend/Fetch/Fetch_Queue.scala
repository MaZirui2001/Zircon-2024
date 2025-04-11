import chisel3._
import chisel3.util._
import ZirconConfig.Fetch._
import ZirconConfig.Decode._

class FetchQueueCommitIO extends Bundle {
    val flush   = Input(Bool())
}

class FetchQueueIO extends Bundle {
    val enq     = Vec(nfch, Flipped(Decoupled(new FrontendPackage)))
    val deq     = Vec(ndcd, Decoupled(new FrontendPackage))
    val cmt     = new FetchQueueCommitIO
}

class FetchQueue extends Module {
    val io = IO(new FetchQueueIO)

    val q = Module(new ClusterIndexFIFO(new FrontendPackage, nfq, nfch, ndcd, 0, 0))

    q.io.enq <> io.enq
    q.io.deq <> io.deq
    q.io.flush := io.cmt.flush
}