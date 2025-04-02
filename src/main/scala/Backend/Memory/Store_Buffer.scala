import chisel3._
import chisel3.util._
import CPU_Config.StoreBuffer._
import CPU_Config.ReserveQueue._
import Zircon_Util._

class sb_entry extends Bundle{
    val paddr = UInt(32.W)
    val wdata = UInt(32.W)
    val wstrb = UInt(4.W)
    val uncache = Bool()

    def apply(paddr: UInt, wdata: UInt, mtype: UInt, uncache: Bool): sb_entry = {
        val entry = Wire(new sb_entry)
        entry.paddr := paddr
        entry.wdata := (wdata << (paddr(1, 0) << 3.U))(31, 0)
        entry.wstrb := (mtype_decode(mtype(1, 0)) << paddr(1, 0))(3, 0)
        entry.uncache := uncache
        entry
    }
}

class Store_Buffer_IO extends Bundle {
    // first time: store write itself into sb
    val enq             = Flipped(Decoupled(new sb_entry))
    val enq_idx         = Output(UInt(wsb.W))

    // second time: store commit from sb
    val deq             = Decoupled(new sb_entry)
    val deq_idx         = Output(UInt(wsb.W))
    val st_cmt          = Input(Bool())
    val st_finish       = Input(Bool())
    
    // load read
    val ld_sb_hit       = Output(UInt(4.W))
    val ld_hit_data     = Output(UInt(32.W))

	val flush 	        = Input(Bool()) // only when all entries have been committed
    val lock            = Input(Bool()) // stop the sb from committing to the dcache
    val clear           = Output(Bool())
}
class Store_Buffer extends Module {
    val io = IO(new Store_Buffer_IO)

    val q = RegInit(VecInit.fill(nsb)(0.U.asTypeOf(new sb_entry)))

    // full and empty flags
    val fulln = RegInit(true.B)
    val eptyn = RegInit(false.B) // ready to commit but not issue to dcache
    val all_clear = RegInit(true.B) // all entries have been committed

    // pointers
    val hptr = RegInit(1.U(nsb.W))
    val rptr = RegInit(1.U(nsb.W)) // ready ptr: points to the latest entry to be committed
    val tptr = RegInit(1.U(nsb.W))
    val cptr = RegInit(1.U(nsb.W)) // commit ptr: points to the latest entry has been committed

    val hptr_nxt = Mux(io.deq.ready && eptyn && !io.lock, shift_sub_1(hptr), hptr)
    val rptr_nxt = Mux(io.st_cmt, shift_sub_1(rptr), rptr)
    val tptr_nxt = Mux(io.enq.valid && fulln, shift_sub_1(tptr), tptr)
    val cptr_nxt = Mux(io.st_finish, shift_sub_1(cptr), cptr)

    hptr := hptr_nxt
    rptr := rptr_nxt
    tptr := Mux(io.flush, rptr_nxt, tptr_nxt)
    cptr := cptr_nxt

    // full and empty flags update logic
    when(io.flush){ fulln := Mux(io.st_finish || !(rptr_nxt & tptr_nxt), true.B, fulln) }
    .elsewhen(io.enq.valid) { fulln := !(cptr_nxt & tptr_nxt) }
    .elsewhen(io.st_finish) { fulln := true.B }

    when(io.lock){ eptyn := eptyn }
    .elsewhen(io.deq.ready){ eptyn := !(hptr_nxt & rptr_nxt) }
    .elsewhen(io.st_cmt) { eptyn := true.B }

    when(io.flush){ all_clear := Mux(io.st_finish || !(tptr_nxt & rptr_nxt), (rptr_nxt & cptr_nxt).orR, all_clear) }
    .elsewhen(io.st_finish){ all_clear := (tptr_nxt & cptr_nxt).orR }
    .elsewhen(io.enq.valid){ all_clear := false.B }
    
    // write logic
    q.zipWithIndex.foreach{ case (qq, i) => 
        when(io.flush){ qq.wstrb := 0.U(4.W) }
		.elsewhen(tptr(i) && io.enq.valid && fulln) { qq := io.enq.bits }
	}
    io.enq_idx := OHToUInt(tptr)

    // read logic
    io.deq.bits  := Mux1H(hptr, q)
    io.deq_idx   := OHToUInt(hptr)
    io.enq.ready := fulln
    io.deq.valid := eptyn && !io.lock
    io.clear     := all_clear

    // load read: read for each byte
    val load_bytes = WireDefault(VecInit.fill(4)(0.U(8.W)))
    val load_hit = WireDefault(VecInit.fill(4)(false.B))
    // 1. match the address(31, 2) with store buffer
    val sb_word_addr_match = VecInit.tabulate(nsb){i => 
        Mux(q(i).paddr(31, 2) === io.enq.bits.paddr(31, 2), 1.U(1.W), 0.U(1.W))
    }.asUInt
    // 2. for each byte in the word, check each wstrb, get the match item
    for(i <- 0 until 4){
        val byte_hit = Log2OHRev(rotateRightOH(sb_word_addr_match & VecInit(q.map(_.wstrb(i))).asUInt, shift_add_1(tptr)))
        load_hit(i) := byte_hit.orR
        load_bytes(i) := Mux1H(rotateLeftOH((byte_hit), shift_add_1(tptr)), q.map(_.wdata(i*8+7, i*8)))
    }
    // 3. shift the result
    io.ld_sb_hit := load_hit.asUInt >> io.enq.bits.paddr(1, 0)
    io.ld_hit_data := load_bytes.asUInt >> (io.enq.bits.paddr(1, 0) << 3.U)
    
}