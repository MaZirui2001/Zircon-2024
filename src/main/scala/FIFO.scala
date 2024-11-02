import chisel3._
import chisel3.util._
import Zircon_Util._

// is_flst: The FIFO is a free list for reg rename
class FIFO_IO[T <: Data](gen: T, n: Int, preg: Boolean) extends Bundle {
    val enq 	= Flipped(Decoupled(gen))
    val deq 	= Decoupled(gen)
	val flush 	= Input(Bool())
	val hptr 	= if(preg) Some(Input(UInt(n.W))) else None
}

class FIFO[T <: Data](gen: T, n: Int, preg: Boolean, iq: Boolean, start_num: Int = 0) extends Module {
	val io = IO(new FIFO_IO(gen, n, preg))

	val q = RegInit(
		if(!preg && !iq) VecInit.fill(n)(0.U.asTypeOf(gen))
		else VecInit.tabulate(n)(i => (start_num+i).U.asTypeOf(gen))
	)
	// full and empty flags
	val fulln = RegInit(if(preg || iq) false.B else true.B)
	val eptyn = RegInit(if(preg || iq) true.B else false.B)

	// pointers
	val hptr = RegInit(1.U(n.W))
	val tptr = RegInit(1.U(n.W))

	// pointer update logic
	val hptr_nxt = Mux(io.deq.ready && eptyn, shift_add_1(hptr), hptr)
	val tptr_nxt = Mux(io.enq.valid && fulln, shift_add_1(tptr), tptr)

	hptr := Mux(io.flush, if(preg) io.hptr.get else 1.U, hptr_nxt)
	tptr := Mux(io.flush, if(preg) tptr_nxt else 1.U, tptr_nxt)

	// full and empty flag update logic
	if(!preg && !iq){
		when(io.flush){ fulln := true.B }
		.elsewhen(io.enq.valid) { fulln := !(hptr_nxt & tptr_nxt) }
		.elsewhen(io.deq.ready) { fulln := true.B }
	} 

	when(io.flush){ eptyn := (if(preg || iq) true.B else false.B) }
	.elsewhen(io.deq.ready) { eptyn := !(hptr_nxt & tptr_nxt) }
	.elsewhen(io.enq.valid) { eptyn := true.B }

	if(iq){
		when(io.flush){ q.zipWithIndex.foreach{ case (qq, i) => qq := (start_num+i).U.asTypeOf(gen) } }
	}

	// write logic
	q.zipWithIndex.foreach{ case (qq, i) => 
		when(tptr(i) && io.enq.valid && fulln) { qq := io.enq.bits }
	}

	// read logic 
	io.deq.bits := Mux1H(hptr, q)

	io.enq.ready := fulln
	io.deq.valid := eptyn

}