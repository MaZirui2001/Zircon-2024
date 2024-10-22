import chisel3._
import chisel3.util._

object FIFO_Utils{
	def shift_add_1(x: UInt): UInt = {
		val n = x.getWidth
		x(n-2, 0) ## x(n-1)
	}
	def shift_add_n(x: UInt, k: Int): UInt = {
		val n = x.getWidth
		if (k == 0) x
		else x(n-k-1, 0) ## x(n-1, n-k)
	}
}
// is_flst: The FIFO is a free list for reg rename
class FIFO_IO[T <: Data](gen: T, n: Int, is_flst: Boolean) extends Bundle {
    val enq 	= Flipped(Decoupled(gen))
    val deq 	= Decoupled(gen)
	val flush 	= Input(Bool())
	val hptr 	= if(is_flst) Some(Input(UInt(n.W))) else None
}

class FIFO[T <: Data](gen: T, n: Int, is_flst: Boolean) extends Module {
	val io = IO(new FIFO_IO(gen, n, is_flst))

	val q = RegInit(
		if(is_flst) VecInit.fill(n)(0.U.asTypeOf(gen))
		else VecInit.tabulate(n)(i => (i+1).U.asTypeOf(gen))
	)

	// full and empty flags
	val fulln = RegInit(if(is_flst) false.B else true.B)
	val eptyn = RegInit(if(is_flst) true.B else false.B)

	// pointers
	val hptr = RegInit(1.U(n.W))
	val tptr = RegInit(1.U(n.W))

	// pointer update logic
	import FIFO_Utils._
	val hptr_nxt = Mux(io.deq.ready && eptyn, shift_add_1(hptr), hptr)
	val tptr_nxt = Mux(io.enq.valid && fulln, shift_add_1(tptr), tptr)

	hptr := Mux(io.flush, if(is_flst) io.hptr.get else hptr, hptr_nxt)
	tptr := Mux(io.flush, if(is_flst) tptr_nxt else hptr, tptr_nxt)

	// full and empty flag update logic
	when(io.flush){ fulln := (if(is_flst) false.B else true.B) }
	.elsewhen(io.enq.valid) { fulln := !(hptr_nxt & tptr_nxt) }
	.elsewhen(io.deq.ready) { fulln := true.B }

	when(io.flush){ eptyn := (if(is_flst) true.B else false.B) }
	.elsewhen(io.deq.ready) { eptyn := !(hptr_nxt & tptr_nxt) }
	.elsewhen(io.enq.valid) { eptyn := true.B }

	// write logic
	q.zipWithIndex.foreach{ case (qq, i) => 
		when(tptr(i) && io.enq.valid && fulln) { qq := io.enq.bits }
	}

	// read logic 
	io.deq.bits := Mux1H(hptr, q)

	io.enq.ready := fulln
	io.deq.valid := eptyn

}