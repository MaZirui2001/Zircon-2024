import chisel3._
import chisel3.util._
import Zircon_Util._

class Cluster_Index_FIFO_IO[T <: Data](gen: T, n: Int, len: Int, ew: Int, dw: Int, rw: Int, ww: Int) extends Bundle {
    val enq 	= Vec(ew, Flipped(Decoupled(gen)))
    val enq_idx = Output(Vec(ew, new Cluster_Entry(len, n)))
    val deq 	= Vec(dw, Decoupled(gen))
    // read port
    val ridx    = Input(Vec(rw, new Cluster_Entry(len, n)))
    val rdata   = Output(Vec(rw, gen))
    // write port
    val widx    = Input(Vec(ww, new Cluster_Entry(len, n)))
    val wen     = Input(Vec(ww, Bool()))
    val wdata   = Input(Vec(ww, gen))

    val flush 	= Input(Bool())
}

class Cluster_Index_FIFO[T <: Data](gen: T, num: Int, ew: Int, dw: Int, rw: Int, ww: Int) extends Module {
    val n = if(dw > ew) dw else ew
    val len = num / n
    val io = IO(new Cluster_Index_FIFO_IO(gen, n, len, ew, dw, rw, ww))

    val fifos = VecInit.tabulate(n)(i => Module(new Index_FIFO(gen, len, rw, ww)).io)

    val all_enq_ready = fifos.map(_.enq.ready).reduce(_ && _)
    val enq_ptr = RegInit(VecInit.tabulate(ew)(i => (1 << i).U(n.W)))
    val enq_ptr_trn = VecInit.tabulate(n)(i => VecInit.tabulate(ew)(j => enq_ptr(j)(i)).asUInt)
    fifos.zipWithIndex.foreach{ case (fifo, i) => 
        fifo.enq.valid := (Mux1H(enq_ptr_trn(i), io.enq.map(_.valid)) 
                       && all_enq_ready
                       && (if(dw > ew) enq_ptr_trn(i).orR else true.B))
        fifo.enq.bits := Mux1H(enq_ptr_trn(i), io.enq.map(_.bits))
    }
    // the enq ready is 1 only when all the fifos are ready
    io.enq.foreach{_.ready := all_enq_ready}
    io.enq_idx.zipWithIndex.foreach{ case(idx, i) => 
        idx.qidx := enq_ptr(i)
        idx.offset := Mux1H(enq_ptr(i), fifos.map(_.enq_idx))
    }

    val deq_ptr = RegInit(VecInit.tabulate(dw)(i => (1 << i).U(n.W)))
    val deq_ptr_trn = VecInit.tabulate(n)(i => VecInit.tabulate(dw)(j => deq_ptr(j)(i)).asUInt)
    io.deq.zipWithIndex.foreach{ case (deq, i) => 
        deq.valid := Mux1H(deq_ptr(i), fifos.map(_.deq.valid))
        deq.bits := Mux1H(deq_ptr(i), fifos.map(_.deq.bits))
    }

    fifos.zipWithIndex.foreach{ case (fifo, i) => 
        fifo.deq.ready := (Mux1H(deq_ptr_trn(i), io.deq.map(_.ready)) 
                       && (if(dw > ew) true.B else deq_ptr_trn(i).orR))
    }

    // update enq_ptr
    when(io.flush){
        enq_ptr.zipWithIndex.foreach{ case (ptr, i) => ptr := (1 << i).U(n.W) }
    }.otherwise{
        val counter = PopCount(io.enq.map(_.valid).zip(io.enq.map(_.ready)).map{ case (v, r) => v && r}).take(log2Ceil(n))
        enq_ptr.foreach{ ptr => ptr := VecInit.tabulate(n)(i => shift_add_n(ptr, i))(counter)}
    }

    // update deq_ptr
    when(io.flush){
        deq_ptr.zipWithIndex.foreach{ case (ptr, i) => ptr := (1 << i).U(n.W) }
    }.otherwise{
        val counter = PopCount(io.deq.map(_.valid).zip(io.deq.map(_.ready)).map{ case (v, r) => v && r}).take(log2Ceil(n))
        deq_ptr.foreach{ ptr => ptr := VecInit.tabulate(n)(i => shift_add_n(ptr, i))(counter)}
    }

    fifos.foreach{_.flush := io.flush}

    // random read logic
    fifos.map(_.ridx := io.ridx.map(_.offset))
    io.rdata.zipWithIndex.foreach{ case (rdata, i) => 
        rdata := Mux1H(io.ridx(i).qidx, fifos.map(_.rdata(i)))
    }

    // random write logic
    fifos.map(_.widx := io.widx.map(_.offset))
    fifos.map(_.wdata := io.wdata)
    fifos.zipWithIndex.foreach{case(fifo, i) =>
        fifo.wen.zipWithIndex.foreach{ case(wen, j) =>
            wen := io.widx(j).qidx(i) && io.wen(j)
        }
    }

}