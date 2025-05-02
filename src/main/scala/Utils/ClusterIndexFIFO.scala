import chisel3._
import chisel3.util._
import ZirconUtil._
import scala.reflect.runtime.universe._
import scala.reflect.ClassTag

class ClusterEntry(wOffset: Int, wQidx: Int) extends Bundle {
    val offset = UInt(wOffset.W)
    val qidx   = UInt(wQidx.W)
    val high   = UInt(1.W)
    def getAge: UInt = {
        high ## offset ## qidx
    }
    def apply(offset: UInt, qidx: UInt, high: UInt): ClusterEntry = {
        val entry = Wire(new ClusterEntry(wOffset, wQidx))
        entry.offset := offset
        entry.qidx   := qidx
        entry.high   := high
        entry
    }
}

class ClusterIndexFIFOIO[T <: Data](gen: T, n: Int, len: Int, ew: Int, dw: Int, rw: Int, ww: Int, isFlst: Boolean) extends Bundle {
    val enq 	= Vec(ew, Flipped(Decoupled(gen)))
    val enqIdx  = Output(Vec(ew, new ClusterEntry(len, n)))
    val deq 	= Vec(dw, Decoupled(gen))
    val deqIdx  = Output(Vec(dw, new ClusterEntry(len, n)))
    // read port
    val ridx    = Input(Vec(rw, new ClusterEntry(len, n)))
    val rdata   = Output(Vec(rw, gen))
    // write port
    val widx    = Input(Vec(ww, new ClusterEntry(len, n)))
    val wen     = Input(Vec(ww, Bool()))
    val wdata   = Input(Vec(ww, gen))

    val flush 	= Input(Bool())
    val dbgFIFO = Output(Vec(n*len, gen))
}

class ClusterIndexFIFO[T <: Data : TypeTag : ClassTag](gen: T, num: Int, ew: Int, dw: Int, rw: Int, ww: Int, isFlst: Boolean = false, rstVal: Option[Seq[T]] = None) extends Module {
    val n = if(dw > ew) dw else ew
    val len = num / n
    assert(num % n == 0, "cluster fifo num must be divisible by n")
    // println(s"n: $n, len: $len, num: $num, ew: $ew, dw: $dw")
    val io = IO(new ClusterIndexFIFOIO(gen, n, len, ew, dw, rw, ww, isFlst))

    val fifos = VecInit.tabulate(n)(i => Module(new IndexFIFO(gen, len, rw, ww, isFlst, 
        if (rstVal.isDefined) Some(rstVal.get.slice(i*len, (i+1)*len)) else None)).io)

    // enq
    val allEnqReady = fifos.map(_.enq.ready).reduce(_ && _)
    val enqPtr = RegInit(VecInit.tabulate(ew)(i => (1 << i).U(n.W)))
    val enqPtrTrans = Transpose(enqPtr)
    fifos.zipWithIndex.foreach{ case (fifo, i) => 
        fifo.enq.valid := (Mux1H(enqPtrTrans(i), io.enq.map(_.valid)) 
                       && allEnqReady
                       && (if(dw > ew) enqPtrTrans(i).orR else true.B))
        fifo.enq.bits := Mux1H(enqPtrTrans(i), io.enq.map(_.bits))
    }
    // the enq ready is 1 only when all the fifos are ready
    io.enq.foreach{_.ready := allEnqReady}
    io.enqIdx.zipWithIndex.foreach{ case(idx, i) => 
        idx.qidx := enqPtr(i)
        idx.offset := Mux1H(enqPtr(i), fifos.map(_.enqIdx))
        idx.high := Mux1H(enqPtr(i), fifos.map(_.enqHigh))
    }
    
    // deq
    val allDeqValid = if(isFlst) fifos.map(_.deq.valid).reduce(_ && _) else true.B
    val deqPtr = RegInit(VecInit.tabulate(dw)(i => (1 << i).U(n.W)))
    val deqPtrTrans = Transpose(deqPtr)
    io.deq.zipWithIndex.foreach{ case (deq, i) => 
        deq.valid := Mux1H(deqPtr(i), fifos.map(_.deq.valid)) && allDeqValid
        deq.bits  := Mux1H(deqPtr(i), fifos.map(_.deq.bits))
    }
    io.deqIdx.zipWithIndex.foreach{ case(idx, i) => 
        idx.qidx    := deqPtr(i)
        idx.offset  := Mux1H(deqPtr(i), fifos.map(_.deqIdx))
        idx.high    := Mux1H(deqPtr(i), fifos.map(_.deqHigh))

    }

    fifos.zipWithIndex.foreach{ case (fifo, i) => 
        fifo.deq.ready := (Mux1H(deqPtrTrans(i), io.deq.map(_.ready)) 
                        && allDeqValid
                        && (if(dw > ew) true.B else deqPtrTrans(i).orR))
    }

    // commit
    val commitPtr = RegInit(VecInit.tabulate(dw)(i => (1 << i).U(n.W)))

    // update enqPtr
    if(isFlst){
        // val counter = BitAlign(PopCount(io.enq.map(_.valid).zip(io.enq.map(_.ready)).map{ case (v, r) => v && r}), log2Ceil(n))
        val counter = BitAlign(Log2(VecInit(io.enq.map(_.valid).zip(io.enq.map(_.ready)).map{ case (v, r) => v && r}).asUInt << 1), log2Ceil(n))
        enqPtr.foreach{ ptr => ptr := VecInit.tabulate(n)(i => ShiftAddN(ptr, i))(counter)}
    }else{
        when(io.flush){
            enqPtr.zipWithIndex.foreach{ case (ptr, i) => ptr := (1 << i).U(n.W) }
        }.otherwise{
            // val counter = BitAlign(PopCount(io.enq.map(_.valid).zip(io.enq.map(_.ready)).map{ case (v, r) => v && r}), log2Ceil(n))
            val counter = BitAlign(Log2(VecInit(io.enq.map(_.valid).zip(io.enq.map(_.ready)).map{ case (v, r) => v && r}).asUInt << 1), log2Ceil(n))
            enqPtr.foreach{ ptr => ptr := Mux(allEnqReady, VecInit.tabulate(n)(i => ShiftAddN(ptr, i))(counter), ptr)}
        }
    }

    // update deqPtr
    when(io.flush){
        if(isFlst){
            deqPtr := commitPtr
        }else{
            deqPtr.zipWithIndex.foreach{ case (ptr, i) => ptr := (1 << i).U(n.W) }
        }
    }.otherwise{
        // val counter = BitAlign(PopCount(io.deq.map(_.valid).zip(io.deq.map(_.ready)).map{ case (v, r) => v && r}), log2Ceil(n))
        val counter = BitAlign(Log2(VecInit(io.deq.map(_.valid).zip(io.deq.map(_.ready)).map{ case (v, r) => v && r}).asUInt << 1), log2Ceil(n))
        deqPtr.foreach{ ptr => ptr := Mux(allDeqValid, VecInit.tabulate(n)(i => ShiftAddN(ptr, i))(counter), ptr)}
    }
    
    // update commitPtr
    if(isFlst){
        // val counter = BitAlign(PopCount(io.enq.map(_.valid).zip(io.enq.map(_.ready)).map{ case (v, r) => v && r}), log2Ceil(n))
        val counter = BitAlign(Log2(VecInit(io.enq.map(_.valid).zip(io.enq.map(_.ready)).map{ case (v, r) => v && r}).asUInt << 1), log2Ceil(n))
        commitPtr.foreach{ ptr => ptr := VecInit.tabulate(n)(i => ShiftAddN(ptr, i))(counter)}
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
            io.dbgFIFO(i*n+j) := fifos(j).dbgFIFO(i)
        }
    }

}

object ClusterIndexFIFO{
    def apply[T <: Data : TypeTag : ClassTag](gen: T, num: Int, ew: Int, dw: Int, rw: Int, ww: Int): ClusterIndexFIFO[T] = {
        new ClusterIndexFIFO(gen, num, ew, dw, rw, ww)
    }
}