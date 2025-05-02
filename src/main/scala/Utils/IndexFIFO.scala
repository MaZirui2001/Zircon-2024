import chisel3._
import chisel3.util._
import ZirconUtil._
import scala.reflect.runtime.universe._
import scala.reflect.ClassTag

// isFlst: The FIFO is a free list for reg rename
class IndexFIFOIO[T <: Data](gen: T, n: Int, rw: Int, ww: Int, isFlst: Boolean) extends Bundle {
    val enq 	= Flipped(Decoupled(gen))
	val enqIdx  = Output(UInt(n.W))
	val enqHigh = Output(Bool())
    val deq 	= Decoupled(gen)
	val deqIdx  = Output(UInt(n.W))
	val deqHigh = Output(Bool())
	// read port
	val ridx    = Input(Vec(rw, UInt(n.W)))
	val rdata   = Output(Vec(rw, gen))
	// write port
	val widx    = Input(Vec(ww, UInt(n.W)))
	val wen     = Input(Vec(ww, Bool()))
	val wdata   = Input(Vec(ww, gen))

	val flush 	= Input(Bool())
	val dbgFIFO = Output(Vec(n, gen))
}

class IndexFIFO[T <: Data : TypeTag : ClassTag](gen: T, n: Int, rw: Int, ww: Int, isFlst: Boolean = false, rstVal: Option[Seq[T]] = None) extends Module {
	val io = IO(new IndexFIFOIO(gen, n, rw, ww, isFlst))

	def hasFunc(funcName: String): Boolean = {
		try {
			val mirror = runtimeMirror(getClass.getClassLoader)
			val instanceMirror = mirror.reflect(gen)
			val methodSymbol = typeOf[T].member(TermName(funcName)).asMethod
			val methodMirror = instanceMirror.reflectMethod(methodSymbol)
			true
		} catch {
			case _: Exception => false
		}
	}

	val hasEnqueueFunc = hasFunc("enqueue")
	val hasWriteFunc = hasFunc("write")

	val q = RegInit(
		if(isFlst && rstVal.isDefined) VecInit(rstVal.get)
		else VecInit.fill(n)(0.U.asTypeOf(gen))
	)

	// full and empty flags
	val fulln = RegInit(true.B)
	val eptyn = RegInit(if(isFlst) true.B else false.B)

	// pointers
	val hptr = RegInit(1.U(n.W))
	val tptr = RegInit(1.U(n.W))
	val hptrHigh = RegInit(0.U(1.W))
	val tptrHigh = RegInit(0.U(1.W))

	// pointer update logic
	val hptrNxt = Mux(io.deq.ready && eptyn, ShiftAdd1(hptr), hptr)
	val tptrNxt = Mux(io.enq.valid && fulln, ShiftAdd1(tptr), tptr)
	hptr := Mux(io.flush, if(isFlst) tptrNxt else hptrNxt, hptrNxt)
	tptr := Mux(io.flush, if(isFlst) tptrNxt else hptrNxt, tptrNxt)


	val hptrHighNxt = Mux(hptrNxt(0) && hptr(n-1), ~hptrHigh, hptrHigh)
	val tptrHighNxt = Mux(tptrNxt(0) && tptr(n-1), ~tptrHigh, tptrHigh)
	hptrHigh := Mux(io.flush, if(isFlst) tptrHighNxt else hptrHighNxt, hptrHighNxt)
	tptrHigh := Mux(io.flush, if(isFlst) tptrHighNxt else hptrHighNxt, tptrHighNxt)

	// full and empty flag update logic
	if(!isFlst){
		when(io.flush){ fulln := true.B }
		.elsewhen(io.enq.valid) { fulln := !(hptrNxt & tptrNxt) }
		.elsewhen(io.deq.ready) { fulln := true.B }
	}

	when(io.flush){ eptyn := (if(isFlst) true.B else false.B) }
	.elsewhen(io.deq.ready) { eptyn := !(hptrNxt & tptrNxt) }
	.elsewhen(io.enq.valid) { eptyn := true.B }

	// write logic
	q.zipWithIndex.foreach{ case (qq, i) => 
		when(tptr(i) && io.enq.valid && fulln) { 
			if(hasEnqueueFunc) {
				qq.asInstanceOf[{ def enqueue(data: T): Unit }].enqueue(io.enq.bits)
			} else {
				qq := io.enq.bits 
			}
		}
	}
	io.enqIdx  := tptr
	io.enqHigh := tptrHigh
	io.deqIdx  := hptr
	io.deqHigh := hptrHigh
	// random access logic
	for(i <- 0 until rw){
		io.rdata(i) := Mux1H(io.ridx(i), q)
	}
	for(i <- 0 until ww){
		when(io.wen(i)){
			q.zipWithIndex.foreach{ case (qq, j) => 
				when(io.widx(i)(j)){ 
					if(hasWriteFunc) {
						qq.asInstanceOf[{ def write(data: T): Unit }].write(io.wdata(i))
					} else {
						qq := io.wdata(i)
					}
				}
			}
		}
	}
	// q.zipWithIndex.foreach{ case (qq, i) =>
	// 	if(qq.isInstanceOf[ROBEntry]){
	// 		when(io.flush){
	// 			qq.asInstanceOf[ROBEntry].bke.complete := false.B
	// 		}
	// 	}
	// }

	// read logic 
	io.deq.bits  := Mux1H(hptr, q)

	io.enq.ready := fulln
	io.deq.valid := eptyn
	
	io.dbgFIFO   := q
}