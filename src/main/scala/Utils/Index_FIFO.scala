import chisel3._
import chisel3.util._
import Zircon_Util._

// is_flst: The FIFO is a free list for reg rename
class Index_FIFO_IO[T <: Data](gen: T, n: Int, rw: Int, ww: Int) extends Bundle {
    val enq 	= Flipped(Decoupled(gen))
	val enq_idx = Output(UInt(n.W))
    val deq 	= Decoupled(gen)
	// read port
	val ridx    = Input(Vec(rw, UInt(n.W)))
	val rdata   = Output(Vec(rw, gen))
	// write port
	val widx    = Input(Vec(ww, UInt(n.W)))
	val wen     = Input(Vec(ww, Bool()))
	val wdata   = Input(Vec(ww, gen))

	val flush 	= Input(Bool())
}

class Index_FIFO[T <: Data](gen: T, n: Int, rw: Int, ww: Int) extends Module {
	val io = IO(new Index_FIFO_IO(gen, n, rw, ww))

	val q = RegInit(VecInit.fill(n)(0.U.asTypeOf(gen)))

	// full and empty flags
	val fulln = RegInit(true.B)
	val eptyn = RegInit(false.B)

	// pointers
	val hptr = RegInit(1.U(n.W))
	val tptr = RegInit(1.U(n.W))

	// pointer update logic
	val hptr_nxt = Mux(io.deq.ready && eptyn, shift_add_1(hptr), hptr)
	val tptr_nxt = Mux(io.enq.valid && fulln, shift_add_1(tptr), tptr)

	hptr := Mux(io.flush, 1.U, hptr_nxt)
	tptr := Mux(io.flush, 1.U, tptr_nxt)

	// full and empty flag update logic
	when(io.flush){ fulln := true.B }
	.elsewhen(io.enq.valid) { fulln := !(hptr_nxt & tptr_nxt) }
	.elsewhen(io.deq.ready) { fulln := true.B }

	when(io.flush){ eptyn := false.B }
	.elsewhen(io.deq.ready) { eptyn := !(hptr_nxt & tptr_nxt) }
	.elsewhen(io.enq.valid) { eptyn := true.B }

	// write logic
	q.zipWithIndex.foreach{ case (qq, i) => 
		when(tptr(i) && io.enq.valid && fulln) { qq := io.enq.bits }
	}
	io.enq_idx := tptr

	// random access logic
	for(i <- 0 until rw){
		io.rdata(i) := MuxOH(io.ridx(i), q)
	}
	for(i <- 0 until ww){
		when(io.wen(i)){
			q.zipWithIndex.foreach{ case (qq, j) => 
				when(io.widx(i)(j)){ qq := io.wdata(i) }
			}
		}
	}

	// read logic 
	io.deq.bits := MuxOH(hptr, q)

	io.enq.ready := fulln
	io.deq.valid := eptyn

}