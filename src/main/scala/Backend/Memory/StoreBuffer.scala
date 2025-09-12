import chisel3._
import chisel3.util._
import ZirconConfig.StoreBuffer._
import ZirconUtil._

class SBEntry extends Bundle{
    val paddr   = UInt(32.W)
    val wdata   = UInt(32.W)
    val wstrb   = UInt(4.W)
    val uncache = Bool()
    val commit  = Bool()

    def apply(paddr: UInt, wdata: UInt, mtype: UInt, uncache: Bool): SBEntry = {
        val entry = Wire(new SBEntry)
        entry.paddr   := paddr
        entry.wdata   := (wdata << (paddr(1, 0) << 3.U))(31, 0)
        entry.wstrb   := (MTypeDecode(mtype(1, 0)) << paddr(1, 0))(3, 0)
        entry.uncache := uncache
        entry.commit  := false.B
        entry
    }
}

class StoreBufferIO extends Bundle {
    // first time: store write itself into sb
    val enq       = Flipped(Decoupled(new SBEntry))
    val enqIdx    = Output(UInt(wsb.W))

    // second time: store commit from sb
    val deq       = Decoupled(new SBEntry)
    val deqIdx    = Output(UInt(wsb.W))
    val stCmt     = Input(Bool())
    val stFinish  = Input(Bool())
    
    // load read
    val ldSBHit   = Output(UInt(4.W))
    val ldHitData = Output(UInt(32.W))

	val flush 	  = Input(Bool()) // only when all entries have been committed
    val lock      = Input(Bool()) // stop the sb from committing to the dcache
    val clear     = Output(Bool())
}
class StoreBuffer extends Module {
    val io = IO(new StoreBufferIO)

    val q = RegInit(VecInit.fill(nsb)(0.U.asTypeOf(new SBEntry)))

    // full and empty flags
    val fulln = RegInit(true.B)
    val eptyn = RegInit(false.B) // ready to commit but not issue to dcache
    val allClear = RegInit(true.B) // all entries have been committed

    // pointers
    val hptr = RegInit(1.U(nsb.W))
    val rptr = RegInit(1.U(nsb.W)) // ready ptr: points to the latest entry to be committed
    val tptr = RegInit(1.U(nsb.W))
    val cptr = RegInit(1.U(nsb.W)) // commit ptr: points to the latest entry has been committed

    val hptrNxt = Mux(io.deq.ready && eptyn && !io.lock, ShiftSub1(hptr), hptr)
    val rptrNxt = Mux(io.stCmt, ShiftSub1(rptr), rptr)
    val tptrNxt = Mux(io.enq.valid && fulln, ShiftSub1(tptr), tptr)
    val cptrNxt = Mux(io.stFinish, ShiftSub1(cptr), cptr)

    hptr := hptrNxt
    rptr := rptrNxt
    tptr := Mux(io.flush, rptrNxt, tptrNxt)
    cptr := cptrNxt

    // full and empty flags update logic
    when(io.flush){ fulln := Mux(io.stFinish , true.B, !q.map(_.commit).reduce(_ && _)) }
    .elsewhen(io.enq.valid) { fulln := !(cptrNxt & tptrNxt) }
    .elsewhen(io.stFinish) { fulln := true.B }

    when(io.deq.ready){ eptyn := !(hptrNxt & rptrNxt) }
    .elsewhen(io.stCmt) { eptyn := true.B }

    when(io.flush){ allClear := Mux(io.stFinish, (rptrNxt & cptrNxt).orR, !q.map(_.commit).reduce(_ || _)) }
    .elsewhen(io.stFinish){ allClear := (tptrNxt & cptrNxt).orR }
    .elsewhen(io.enq.valid){ allClear := false.B }
    
    // write logic
    q.zipWithIndex.foreach{ case (qq, i) => 
        when(rptr(i) && io.stCmt){ qq.commit := true.B }
        .elsewhen(io.flush){ qq.wstrb := Mux(qq.commit, qq.wstrb, 0.U(4.W)) }
		.elsewhen(tptr(i) && io.enq.valid && fulln) { qq := io.enq.bits }
        
	}
    io.enqIdx := OHToUInt(tptr)

    // read logic
    io.deq.bits  := Mux1H(hptr, q)
    io.deqIdx    := OHToUInt(hptr)
    io.enq.ready := fulln
    io.deq.valid := eptyn && !io.lock
    io.clear     := allClear
    q.zipWithIndex.foreach{ case(qq, i) => 
        when(cptr(i) && io.stFinish){
            qq.commit := false.B
        }
    }

    // load read: read for each byte
    val loadBytes = WireDefault(VecInit.fill(4)(0.U(8.W)))
    val loadHit   = WireDefault(VecInit.fill(4)(false.B))
    // 1. match the address(31, 2) with store buffer
    val sbWordAddrMatch = VecInit.tabulate(nsb){i => 
        Mux(q(i).paddr(31, 2) === io.enq.bits.paddr(31, 2), 1.U(1.W), 0.U(1.W))
    }.asUInt
    // 2. for each byte in the word, check each wstrb, get the match item
    for(i <- 0 until 4){
        val byteHit   = Log2OHRev(RotateRightOH(sbWordAddrMatch & VecInit(q.map(_.wstrb(i))).asUInt, ShiftAdd1(tptr)))
        loadHit(i)   := byteHit.orR
        loadBytes(i) := Mux1H(RotateLeftOH((byteHit), ShiftAdd1(tptr)), q.map(_.wdata(i*8+7, i*8)))
    }
    // 3. shift the result
    io.ldSBHit := loadHit.asUInt >> io.enq.bits.paddr(1, 0)
    io.ldHitData := loadBytes.asUInt >> (io.enq.bits.paddr(1, 0) << 3.U)
    
}