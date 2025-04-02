import chisel3._
import chisel3.util._
import Zircon_Config.Fetch._
import Zircon_Config.Decode._

class Fetch_Queue_Commit_IO extends Bundle {
    val flush   = Input(Bool())
}

class Fetch_Queue_IO extends Bundle {
    val enq     = Vec(nfetch, Flipped(Decoupled(new Frontend_Package)))
    val deq     = Vec(ndecode, Decoupled(new Frontend_Package))
    val cmt     = new Fetch_Queue_Commit_IO
}

class Fetch_Queue extends Module {
    val io = IO(new Fetch_Queue_IO)

    val q = Module(new Cluster_Index_FIFO(new Frontend_Package, nfetch_q, nfetch, ndecode, 0, 0))

    q.io.enq <> io.enq
    q.io.deq <> io.deq
    q.io.flush := io.cmt.flush
}