import chisel3._
import chisel3.util._
import Zircon_Util._

class Cluster_FIFO_IO[T <: Data](gen: T, num: Int, ew: Int, dw: Int, is_flst: Boolean) extends Bundle {
    val n       = if(dw > ew) dw else ew
    val enq 	= Vec(ew, Flipped(Decoupled(gen)))
    val deq 	= Vec(dw, Decoupled(gen))
    val flush 	= Input(Bool())
    val hptr    = if(is_flst) Some(Input(Vec(n, UInt((num/n).W)))) else None
}

class Cluster_FIFO[T <: Data](gen: T, num: Int, ew: Int, dw: Int, is_flst: Boolean) extends Module {
    // assert(ew >= dw, "enq width must be greater than or equal to deq width")
    val io = IO(new Cluster_FIFO_IO(gen, num, ew, dw, is_flst))
    val n = if(dw > ew) dw else ew
    val len = num / n
    val fifos = VecInit.tabulate(n)(i => Module(new FIFO(gen, len, is_flst, false, (i << log2Ceil(len)))).io)

    val all_enq_ready = fifos.map(_.enq.ready).reduce(_ && _)

    val enq_ptr = RegInit(VecInit.tabulate(ew)(i => (1 << i).U(n.W)))
    val enq_ptr_trn = VecInit.tabulate(n)(i => VecInit.tabulate(ew)(j => enq_ptr(j)(i)).asUInt)
    fifos.zipWithIndex.foreach{ case (fifo, i) => 
        fifo.enq.valid := (Mux1H(enq_ptr(i), io.enq.map(_.valid)) 
                       && all_enq_ready
                       && (if(dw > ew) enq_ptr_trn(i).orR else true.B))
        fifo.enq.bits := Mux1H(enq_ptr(i), io.enq.map(_.bits))
    }
    // the enq ready is 1 only when all the fifos are ready
    io.enq.foreach{_.ready := all_enq_ready}

    val deq_ptr = RegInit(VecInit.tabulate(dw)(i => (1 << i).U(n.W)))
    // Transform the deq_ptr matrix
    val deq_ptr_trn = VecInit.tabulate(ew)(i => VecInit.tabulate(dw)(j => deq_ptr(j)(i)).asUInt)
    io.deq.zipWithIndex.foreach{ case (deq, i) => 
        deq.valid := Mux1H(deq_ptr(i), fifos.map(_.deq.valid))
        deq.bits := Mux1H(deq_ptr(i), fifos.map(_.deq.bits))
    }

    fifos.zipWithIndex.foreach{ case (fifo, i) => 
        fifo.deq.ready := Mux1H(deq_ptr_trn(i), io.deq.map(_.ready)) && (if(dw > ew) true.B else deq_ptr_trn(i).orR)
    }

    // update enq_ptr
    when(io.flush){
        enq_ptr.zipWithIndex.foreach{ case (ptr, i) => ptr := (1 << i).U(n.W) }
    }.otherwise{
        val counter = PopCount(io.enq.map(_.valid).zip(io.enq.map(_.ready)).map{ case (v, r) => v && r}).take(log2Ceil(n))
        enq_ptr.foreach{ ptr => ptr := VecInit.tabulate(n)(i => shift_sub_n(ptr, i))(counter)}
    }

    // update deq_ptr
    when(io.flush){
        deq_ptr.zipWithIndex.foreach{ case (ptr, i) => ptr := (1 << i).U(n.W) }
    }.otherwise{
        val counter = PopCount(io.deq.map(_.valid).zip(io.deq.map(_.ready)).map{ case (v, r) => v && r}).take(log2Ceil(n))
        deq_ptr.foreach{ ptr => ptr := VecInit.tabulate(n)(i => shift_add_n(ptr, i))(counter)}
    }

    fifos.zipWithIndex.foreach{ case (fifo, i) => 
        fifo.flush := io.flush
        // if(is_flst){ fifo.hptr.get := io.hptr.get(i) }
    }

}

object Cluster_FIFO{
    def apply[T <: Data](gen: T, n: Int, ew: Int, dw: Int, is_flst: Boolean): Cluster_FIFO[T] = {
        val cluster_fifo = Module(new Cluster_FIFO(gen, n, ew, dw, is_flst))
        cluster_fifo
    }
}