import chisel3._
import chisel3.util._
import Zircon_Util._

class Cluster_Entry(w_offset: Int, w_qidx: Int) extends Bundle {
    val offset = UInt(w_offset.W)
    val qidx   = UInt(w_qidx.W)
    val high   = UInt(1.W)
    def get_age: UInt = {
        high ## offset ## qidx
    }
    def apply(offset: UInt, qidx: UInt, high: UInt): Cluster_Entry = {
        val entry = Wire(new Cluster_Entry(w_offset, w_qidx))
        entry.offset := offset
        entry.qidx   := qidx
        entry.high   := high
        entry
    }
}

class Cluster_Index_FIFO_IO[T <: Data](gen: T, n: Int, len: Int, ew: Int, dw: Int, rw: Int, ww: Int, is_flst: Boolean) extends Bundle {
    val enq 	= Vec(ew, Flipped(Decoupled(gen)))
    val enq_idx = Output(Vec(ew, new Cluster_Entry(len, n)))
    val deq 	= Vec(dw, Decoupled(gen))
    val deq_idx = Output(Vec(dw, new Cluster_Entry(len, n)))
    // read port
    val ridx    = Input(Vec(rw, new Cluster_Entry(len, n)))
    val rdata   = Output(Vec(rw, gen))
    // write port
    val widx    = Input(Vec(ww, new Cluster_Entry(len, n)))
    val wen     = Input(Vec(ww, Bool()))
    val wdata   = Input(Vec(ww, gen))

    val flush 	= Input(Bool())
    val dbg_FIFO = Output(Vec(n*len, gen))
}

class Cluster_Index_FIFO[T <: Data](gen: T, num: Int, ew: Int, dw: Int, rw: Int, ww: Int, is_flst: Boolean = false, rst_val: Option[Seq[T]] = None) extends Module {
    val n = if(dw > ew) dw else ew
    val len = num / n
    assert(num % n == 0, "cluster fifo num must be divisible by n")
    // println(s"n: $n, len: $len, num: $num, ew: $ew, dw: $dw")
    val io = IO(new Cluster_Index_FIFO_IO(gen, n, len, ew, dw, rw, ww, is_flst))

    val fifos = VecInit.tabulate(n)(i => Module(new Index_FIFO(gen, len, rw, ww, is_flst, 
        if (rst_val.isDefined) Some(rst_val.get.slice(i*len, (i+1)*len)) else None)).io)

    // enq
    val all_enq_ready = fifos.map(_.enq.ready).reduce(_ && _)
    val enq_ptr = RegInit(VecInit.tabulate(ew)(i => (1 << i).U(n.W)))
    val enq_ptr_trans = Transpose(enq_ptr)
    fifos.zipWithIndex.foreach{ case (fifo, i) => 
        fifo.enq.valid := (Mux1H(enq_ptr_trans(i), io.enq.map(_.valid)) 
                       && all_enq_ready
                       && (if(dw > ew) enq_ptr_trans(i).orR else true.B))
        fifo.enq.bits := Mux1H(enq_ptr_trans(i), io.enq.map(_.bits))
    }
    // the enq ready is 1 only when all the fifos are ready
    io.enq.foreach{_.ready := all_enq_ready}
    io.enq_idx.zipWithIndex.foreach{ case(idx, i) => 
        idx.qidx := enq_ptr(i)
        idx.offset := Mux1H(enq_ptr(i), fifos.map(_.enq_idx))
        idx.high := Mux1H(enq_ptr(i), fifos.map(_.enq_high))
    }
    
    // deq
    val all_deq_valid = if(is_flst) fifos.map(_.deq.valid).reduce(_ && _) else true.B
    val deq_ptr = RegInit(VecInit.tabulate(dw)(i => (1 << i).U(n.W)))
    val deq_ptr_trans = Transpose(deq_ptr)
    io.deq.zipWithIndex.foreach{ case (deq, i) => 
        deq.valid := Mux1H(deq_ptr(i), fifos.map(_.deq.valid))
        deq.bits := Mux1H(deq_ptr(i), fifos.map(_.deq.bits))
    }
    io.deq_idx.zipWithIndex.foreach{ case(idx, i) => 
        idx.qidx    := deq_ptr(i)
        idx.offset  := Mux1H(deq_ptr(i), fifos.map(_.deq_idx))
        idx.high    := Mux1H(deq_ptr(i), fifos.map(_.deq_high))

    }

    fifos.zipWithIndex.foreach{ case (fifo, i) => 
        fifo.deq.ready := (Mux1H(deq_ptr_trans(i), io.deq.map(_.ready)) 
                        && all_deq_valid
                        && (if(dw > ew) true.B else deq_ptr_trans(i).orR))
    }

    // commit
    val commit_ptr = RegInit(VecInit.tabulate(dw)(i => (1 << i).U(n.W)))

    // update enq_ptr
    if(is_flst){
        // val counter = BitAlign(PopCount(io.enq.map(_.valid).zip(io.enq.map(_.ready)).map{ case (v, r) => v && r}), log2Ceil(n))
        val counter = BitAlign(Log2(VecInit(io.enq.map(_.valid).zip(io.enq.map(_.ready)).map{ case (v, r) => v && r}).asUInt << 1), log2Ceil(n))
        enq_ptr.foreach{ ptr => ptr := VecInit.tabulate(n)(i => ShiftAddN(ptr, i))(counter)}
    }else{
        when(io.flush){
            enq_ptr.zipWithIndex.foreach{ case (ptr, i) => ptr := (1 << i).U(n.W) }
        }.otherwise{
            // val counter = BitAlign(PopCount(io.enq.map(_.valid).zip(io.enq.map(_.ready)).map{ case (v, r) => v && r}), log2Ceil(n))
            val counter = BitAlign(Log2(VecInit(io.enq.map(_.valid).zip(io.enq.map(_.ready)).map{ case (v, r) => v && r}).asUInt << 1), log2Ceil(n))
            enq_ptr.foreach{ ptr => ptr := Mux(all_enq_ready, VecInit.tabulate(n)(i => ShiftAddN(ptr, i))(counter), ptr)}
        }
    }

    // update deq_ptr
    when(io.flush){
        if(is_flst){
            deq_ptr := commit_ptr
        }else{
            deq_ptr.zipWithIndex.foreach{ case (ptr, i) => ptr := (1 << i).U(n.W) }
        }
    }.otherwise{
        // val counter = BitAlign(PopCount(io.deq.map(_.valid).zip(io.deq.map(_.ready)).map{ case (v, r) => v && r}), log2Ceil(n))
        val counter = BitAlign(Log2(VecInit(io.deq.map(_.valid).zip(io.deq.map(_.ready)).map{ case (v, r) => v && r}).asUInt << 1), log2Ceil(n))
        deq_ptr.foreach{ ptr => ptr := Mux(all_deq_valid, VecInit.tabulate(n)(i => ShiftAddN(ptr, i))(counter), ptr)}
    }
    
    // update commit_ptr
    if(is_flst){
        // val counter = BitAlign(PopCount(io.enq.map(_.valid).zip(io.enq.map(_.ready)).map{ case (v, r) => v && r}), log2Ceil(n))
        val counter = BitAlign(Log2(VecInit(io.enq.map(_.valid).zip(io.enq.map(_.ready)).map{ case (v, r) => v && r}).asUInt << 1), log2Ceil(n))
        commit_ptr.foreach{ ptr => ptr := VecInit.tabulate(n)(i => ShiftAddN(ptr, i))(counter)}
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

    for(i <- 0 until len){
        for(j <- 0 until n){
            io.dbg_FIFO(i*n + j) := fifos(j).dbg_FIFO(i)
        }
    }

}

object Cluster_Index_FIFO{
    def apply[T <: Data](gen: T, num: Int, ew: Int, dw: Int, rw: Int, ww: Int): Cluster_Index_FIFO[T] = {
        new Cluster_Index_FIFO(gen, num, ew, dw, rw, ww)
    }
}