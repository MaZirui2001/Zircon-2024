import chisel3._
import chisel3.util._
import ZirconUtil._

// isFlst: The FIFO is a free list for reg rename
class FIFOIO[T <: Data](gen: T, n: Int, preg: Boolean) extends Bundle {
    val enq 	= Flipped(Decoupled(gen))
    val deq 	= Decoupled(gen)
	val flush 	= Input(Bool())
	val hptr 	= if(preg) Some(Input(UInt(n.W))) else None
}

class FIFO[T <: Data](gen: T, n: Int, preg: Boolean, iq: Boolean, startNum: Int = 0) extends Module {
	val io = IO(new FIFOIO(gen, n, preg))

	val q = RegInit(
		if(!preg && !iq) VecInit.fill(n)(0.U.asTypeOf(gen))
		else VecInit.tabulate(n)(i => (startNum+i).U.asTypeOf(gen))
	)
	// full and empty flags
	val fulln = RegInit(true.B)
	val eptyn = RegInit(if(preg || iq) true.B else false.B)

	// pointers
	val hptr = RegInit(1.U(n.W))
	val tptr = RegInit(1.U(n.W))

	// pointer update logic
	val hptrNxt = Mux(io.deq.ready && eptyn, ShiftAdd1(hptr), hptr)
	val tptrNxt = Mux(io.enq.valid && fulln, ShiftAdd1(tptr), tptr)

	hptr := Mux(io.flush, if(preg) io.hptr.get else 1.U, hptrNxt)
	tptr := Mux(io.flush, if(preg) tptrNxt else 1.U, tptrNxt)

	// full and empty flag update logic
	if(!preg && !iq){
		when(io.flush){ fulln := true.B }
		.elsewhen(io.enq.valid) { fulln := !(hptrNxt & tptrNxt) }
		.elsewhen(io.deq.ready) { fulln := true.B }
	} 

	when(io.flush){ eptyn := (if(preg || iq) true.B else false.B) }
	.elsewhen(io.deq.ready) { eptyn := !(hptrNxt & tptrNxt) }
	.elsewhen(io.enq.valid) { eptyn := true.B }

	// write logic
	q.zipWithIndex.foreach{ case (qq, i) => 
		when(tptr(i) && io.enq.valid && fulln) { qq := io.enq.bits }
	}

	if(iq){
		when(io.flush){ q.zipWithIndex.foreach{ case (qq, i) => qq := (startNum+i).U.asTypeOf(gen) } }
	}

	// read logic 
	io.deq.bits := Mux1H(hptr, q)

	io.enq.ready := fulln
	io.deq.valid := eptyn

}