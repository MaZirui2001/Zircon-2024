import chisel3._
import chisel3.util._

class Cluster_FIFO_IO[T <: Data](gen: T, n: Int, k: Int, is_flst: Boolean) extends Bundle {
    val enq 	= Vec(k, Flipped(Decoupled(gen)))
    val deq 	= Vec(k, Decoupled(gen))
    val flush 	= Input(Bool())
    val hptr    = if(is_flst) Some(Input(Vec(k, UInt((n/k).W)))) else None
}

class Cluster_FIFO[T <: Data](gen: T, n: Int, k: Int, is_flst: Boolean) extends Module {
    val io = IO(new Cluster_FIFO_IO(gen, n, k, is_flst))

    val fifos = VecInit.tabulate(k)(i => Module(new FIFO(gen, n/k, is_flst)).io)

    val enq_ptr = RegInit(VecInit.tabulate(k)(i => (1 << i).U(k.W)))
    fifos.zipWithIndex.foreach({ case (fifo, i) => 
        fifo.enq.valid := Mux1H(enq_ptr(i), io.enq.map(_.valid))
        fifo.enq.bits := Mux1H(enq_ptr(i), io.enq.map(_.bits))
        io.enq(i).ready := Mux1H(enq_ptr(i), fifos.map(_.enq.ready))
    })

    val deq_ptr = RegInit(VecInit.tabulate(k)(i => (1 << i).U(k.W)))
    fifos.zipWithIndex.foreach({ case (fifo, i) => 
        fifo.deq.ready := Mux1H(deq_ptr(i), io.deq.map(_.ready))
        io.deq(i).valid := Mux1H(deq_ptr(i), fifos.map(_.deq.valid))
        io.deq(i).bits := Mux1H(deq_ptr(i), fifos.map(_.deq.bits))
    })

    // update enq_ptr
    import FIFO_Utils._
    when(io.flush){
        enq_ptr.zipWithIndex.foreach{ case (ptr, i) => ptr := (1 << i).U(k.W) }
    }.elsewhen(VecInit(io.enq.map(_.ready)).reduceTree(_ && _)){
        val counter = PopCount(io.enq.map(_.valid)).take(log2Ceil(k))
        enq_ptr.foreach{ ptr => ptr := VecInit.tabulate(k)(i => shift_add_n(ptr, i))(counter)}
    }

    // update deq_ptr
    when(io.flush){
        deq_ptr.zipWithIndex.foreach{ case (ptr, i) => ptr := (1 << i).U(k.W) }
    }.elsewhen(VecInit(io.deq.map(_.valid)).reduceTree(_ && _)){
        val counter = PopCount(io.deq.map(_.ready)).take(log2Ceil(k))
        deq_ptr.foreach{ ptr => ptr := VecInit.tabulate(k)(i => shift_add_n(ptr, i))(counter)}
    }

    fifos.zipWithIndex.foreach{ case (fifo, i) => 
        fifo.flush := io.flush
        if(is_flst){ fifo.hptr.get := io.hptr.get(i) }
    }

}

