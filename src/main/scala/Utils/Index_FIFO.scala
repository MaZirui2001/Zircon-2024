import chisel3._
import chisel3.util._
import Zircon_Util._

// is_flst: The FIFO is a free list for reg rename
class Index_FIFO_IO[T <: Data](gen: T, n: Int, rw: Int, ww: Int, is_flst: Boolean) extends Bundle {
    val enq 	 = Flipped(Decoupled(gen))
	val enq_idx  = Output(UInt(n.W))
	val enq_high = Output(Bool())
    val deq 	 = Decoupled(gen)
	val deq_idx  = Output(UInt(n.W))
	val deq_high = Output(Bool())
	// read port
	val ridx    = Input(Vec(rw, UInt(n.W)))
	val rdata   = Output(Vec(rw, gen))
	// write port
	val widx    = Input(Vec(ww, UInt(n.W)))
	val wen     = Input(Vec(ww, Bool()))
	val wdata   = Input(Vec(ww, gen))

	val flush 	= Input(Bool())
	val dbg_FIFO = Output(Vec(n, gen))
}

class Index_FIFO[T <: Data](gen: T, n: Int, rw: Int, ww: Int, is_flst: Boolean = false, rst_val: Option[Seq[T]] = None) extends Module {
	val io = IO(new Index_FIFO_IO(gen, n, rw, ww, is_flst))

	val q = RegInit(
		if(is_flst && rst_val.isDefined) VecInit(rst_val.get)
		else VecInit.fill(n)(0.U.asTypeOf(gen))
	)

	// full and empty flags
	val fulln = RegInit(true.B)
	val eptyn = RegInit(if(is_flst) true.B else false.B)

	// pointers
	val hptr = RegInit(1.U(n.W))
	val tptr = RegInit(1.U(n.W))
	val hptr_high = RegInit(0.U(1.W))
	val tptr_high = RegInit(0.U(1.W))

	// pointer update logic
	val hptr_nxt = Mux(io.deq.ready && eptyn, ShiftAdd1(hptr), hptr)
	val tptr_nxt = Mux(io.enq.valid && fulln, ShiftAdd1(tptr), tptr)
	hptr := Mux(io.flush, if(is_flst) tptr_nxt else 1.U, hptr_nxt)
	tptr := Mux(io.flush, if(is_flst) tptr_nxt else 1.U, tptr_nxt)


	val hptr_high_nxt = Mux(hptr_nxt(0) && hptr(n-1), ~hptr_high, hptr_high)
	val tptr_high_nxt = Mux(tptr_nxt(0) && tptr(n-1), ~tptr_high, tptr_high)
	hptr_high := Mux(io.flush, if(is_flst) tptr_high_nxt else 0.U, hptr_high_nxt)
	tptr_high := Mux(io.flush, if(is_flst) tptr_high_nxt else 0.U, tptr_high_nxt)

	// full and empty flag update logic
	if(!is_flst){
		when(io.flush){ fulln := true.B }
		.elsewhen(io.enq.valid) { fulln := !(hptr_nxt & tptr_nxt) }
		.elsewhen(io.deq.ready) { fulln := true.B }
	}

	when(io.flush){ eptyn := (if(is_flst) true.B else false.B) }
	.elsewhen(io.deq.ready) { eptyn := !(hptr_nxt & tptr_nxt) }
	.elsewhen(io.enq.valid) { eptyn := true.B }

	// write logic
	q.zipWithIndex.foreach{ case (qq, i) => 
		when(tptr(i) && io.enq.valid && fulln) { 
			if(qq.isInstanceOf[ROB_Entry]){
				qq.asInstanceOf[ROB_Entry].fte := io.enq.bits.asInstanceOf[ROB_Entry].fte
				qq.asInstanceOf[ROB_Entry].bke.complete := false.B
			}else{
				qq := io.enq.bits 
			}
		}
		if(qq.isInstanceOf[ROB_Entry]){
			when(io.flush){
				qq.asInstanceOf[ROB_Entry].bke.complete := false.B
			}
		}
	}
	io.enq_idx := tptr
	io.enq_high := tptr_high
	io.deq_idx := hptr
	io.deq_high := hptr_high
	// random access logic
	for(i <- 0 until rw){
		io.rdata(i) := Mux1H(io.ridx(i), q)
	}
	for(i <- 0 until ww){
		when(io.wen(i)){
			q.zipWithIndex.foreach{ case (qq, j) => 
				when(io.widx(i)(j)){ 
					if(qq.isInstanceOf[ROB_Entry]){
						qq.asInstanceOf[ROB_Entry].bke := io.wdata(i).asInstanceOf[ROB_Entry].bke
					}else{
						qq := io.wdata(i) 
					}
				}
			}
		}
	}

	// read logic 
	io.deq.bits := Mux1H(hptr, q)

	io.enq.ready := fulln
	io.deq.valid := eptyn
	
	io.dbg_FIFO := q
}