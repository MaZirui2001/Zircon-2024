import chisel3._
import chisel3.util._

class Cluster_FIFO_IO[T <: Data](gen: T, n: Int, ew: Int, dw: Int, is_flst: Boolean) extends Bundle {
    val enq 	= Vec(ew, Flipped(Decoupled(gen)))
    val deq 	= Vec(dw, Decoupled(gen))
    val flush 	= Input(Bool())
    val hptr    = if(is_flst) Some(Input(Vec(ew, UInt((n/ew).W)))) else None
}

class Cluster_FIFO[T <: Data](gen: T, n: Int, ew: Int, dw: Int, is_flst: Boolean) extends Module {
    assert(ew >= dw, "enq width must be greater than or equal to deq width")
    val io = IO(new Cluster_FIFO_IO(gen, n, ew, dw, is_flst))

    // FIFO has ew entries, each entry has n/ew elements
    val fifos = VecInit.tabulate(ew)(i => Module(new FIFO(gen, n/ew, is_flst)).io)

    val all_enq_ready = fifos.map(_.enq.ready).reduce(_ && _)

    val enq_ptr = RegInit(VecInit.tabulate(ew)(i => (1 << i).U(ew.W)))
    fifos.zipWithIndex.foreach{ case (fifo, i) => 
        fifo.enq.valid := Mux1H(enq_ptr(i), io.enq.map(_.valid)) && all_enq_ready 
        fifo.enq.bits := Mux1H(enq_ptr(i), io.enq.map(_.bits))
    }
    // the enq ready is 1 only when all the fifos are ready
    io.enq.foreach{_.ready := all_enq_ready}

    val deq_ptr = RegInit(VecInit.tabulate(dw)(i => (1 << i).U(ew.W)))
    // Transform the deq_ptr matrix
    val deq_ptr_trn = VecInit.tabulate(ew)(i => VecInit.tabulate(dw)(j => deq_ptr(j)(i)).asUInt)
    io.deq.zipWithIndex.foreach{ case (deq, i) => 
        deq.valid := Mux1H(deq_ptr(i), fifos.map(_.deq.valid))
        deq.bits := Mux1H(deq_ptr(i), fifos.map(_.deq.bits))
    }

    fifos.zipWithIndex.foreach{ case (fifo, i) => 
        fifo.deq.ready := deq_ptr_trn(i).orR && Mux1H(deq_ptr_trn(i), io.deq.map(_.ready))
    }

    // update enq_ptr
    import FIFO_Utils._
    when(io.flush){
        enq_ptr.zipWithIndex.foreach{ case (ptr, i) => ptr := (1 << i).U(ew.W) }
    }.otherwise{
        // val counter = PopCount(VecInit(io.enq.map(_.valid)).asUInt & VecInit(io.enq.map(_.ready)).asUInt).take(log2Ceil(ew))
        val counter = PopCount(io.enq.map(_.valid).zip(io.enq.map(_.ready)).map{ case (v, r) => v && r}).take(log2Ceil(ew))
        enq_ptr.foreach{ ptr => ptr := VecInit.tabulate(ew)(i => shift_sub_n(ptr, i))(counter)}
    }

    // update deq_ptr
    when(io.flush){
        deq_ptr.zipWithIndex.foreach{ case (ptr, i) => ptr := (1 << i).U(ew.W) }
    }.otherwise{
        // val counter = PopCount(VecInit(io.deq.map(_.valid)).asUInt & VecInit(io.deq.map(_.ready)).asUInt).take(log2Ceil(ew))
        val counter = PopCount(io.deq.map(_.valid).zip(io.deq.map(_.ready)).map{ case (v, r) => v && r}).take(log2Ceil(ew))
        deq_ptr.foreach{ ptr => ptr := VecInit.tabulate(ew)(i => shift_add_n(ptr, i))(counter)}
    }

    fifos.zipWithIndex.foreach{ case (fifo, i) => 
        fifo.flush := io.flush
        if(is_flst){ fifo.hptr.get := io.hptr.get(i) }
    }

}

