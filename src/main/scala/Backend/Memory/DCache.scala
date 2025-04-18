import chisel3._
import chisel3.util._
import ZirconConfig.Cache._
import ZirconConfig.StoreBuffer._
import ZirconUtil._

class DChannel1Stage1Signal extends Bundle {
    val rreq     = Bool()
    val vaddr    = UInt(32.W)
    val mtype    = UInt(3.W)
    val isLatest = Bool()
    val wreq     = Bool()
    val wdata    = UInt(32.W)

    def apply(pp: DPipelineIO): DChannel1Stage1Signal = {
        val c = Wire(new DChannel1Stage1Signal)
        InheritFields(c, pp)
        c
    }
}

class DChannel1Stage2Signal extends DChannel1Stage1Signal {
    val rtag    = Vec(l1Way, UInt(l1Tag.W))
    val rdata   = Vec(l1Way, UInt(l1LineBits.W))
    val hit     = UInt(l1Way.W)
    val lru     = UInt(2.W)
    val paddr   = UInt(32.W)
    val uncache = Bool()

    def apply(C: DChannel1Stage1Signal, M: DMMUIO, rtag: Vec[UInt], rdata: Vec[UInt], hit: UInt, lru: UInt): DChannel1Stage2Signal = {
        val c = Wire(new DChannel1Stage2Signal)
        InheritFields(c, C)
        InheritFields(c, M)
        c.rtag  := rtag
        c.rdata := rdata
        c.hit   := hit
        c.lru   := lru
        c
    }
}

class DChannel1Stage3Signal extends DChannel1Stage2Signal {
    val sbHitData = UInt(32.W)
    val memData   = UInt(32.W)
    val sbHit     = UInt(4.W)

    def apply(C: DChannel1Stage2Signal, sbHitData: UInt, memData: UInt, sbHit: UInt): DChannel1Stage3Signal = {
        val c = Wire(new DChannel1Stage3Signal)
        InheritFields(c, C)
        c.sbHitData := sbHitData
        c.memData   := memData
        c.sbHit     := sbHit
        c
    }
}

class DChannel2Stage1Signal extends Bundle {
    val wreq    = Bool()
    val wdata   = UInt(32.W)
    val paddr   = UInt(32.W)
    val mtype   = UInt(3.W)
    val uncache = Bool()
    val sbIdx   = UInt(wsb.W)

    def apply(SBEntry: SBEntry, sbValid: Bool, sbIdx: UInt): DChannel2Stage1Signal = {
        val c = Wire(new DChannel2Stage1Signal)
        c.wreq    := sbValid
        c.wdata   := SBEntry.wdata >> (SBEntry.paddr(1, 0) << 3)
        c.paddr   := SBEntry.paddr
        c.mtype   := MTypeEncode(SBEntry.wstrb >> SBEntry.paddr(1, 0))
        c.uncache := SBEntry.uncache
        c.sbIdx   := sbIdx
        c
    }
}

class DChannel2Stage2Signal extends DChannel2Stage1Signal {
    val hit = UInt(l1Way.W)
        
    def apply(C: DChannel2Stage1Signal, hit: UInt): DChannel2Stage2Signal = {
        val c = Wire(new DChannel2Stage2Signal)
        InheritFields(c, C)
        c.hit := hit
        c
    }
}

class DPipelineIO extends Bundle {
    val rreq       = Input(Bool())
    val mtype      = Input(UInt(3.W))
    val isLatest   = Input(Bool()) // inform the rrequest is issued in order
    val wreq       = Input(Bool())
    val wdata      = Input(UInt(32.W))
    val vaddr      = Input(UInt(32.W))
    val rdata      = Output(UInt(32.W))
    val miss       = Output(Bool()) // not latest but uncache
    val rrsp       = Output(Bool())
    val loadReplay = Output(Bool())
    val sbFull     = Output(Bool())
    val wrsp       = Output(Bool())
}

class DCommitIO extends Bundle {
    val stCmt      = Input(Bool())
    val flush      = Input(Bool())
}

class DMMUIO extends Bundle {
    val paddr      = Input(UInt(32.W))
    val uncache    = Input(Bool())
    val exception  = Input(UInt(8.W)) // 1 cycle latency
}
class DCacheReadDBG extends CacheDBG {
    val missCycle   = UInt(64.W)
    val sbFullCycle = UInt(64.W)
}
class DCacheWriteDBG extends CacheDBG 

class DCacheIO extends Bundle {
    val mmu = new DMMUIO
    val pp  = new DPipelineIO
    val cmt = new DCommitIO
    val l2  = Flipped(new L2DCacheIO)
    val dbg = Output(MixedVec(new DCacheReadDBG, new DCacheWriteDBG))
}

class DCache extends Module {
    val io = IO(new DCacheIO)

    /* 
    DCache (write through and write unallocate) has two channels:
        1. read and first write
        2. commit write
        l1Way now is 2
    */
    // Memory arrays
    val tagTab  = VecInit.fill(l1Way)(Module(new XilinxTrueDualPortReadFirst1ClockRam(l1Tag, l1IndexNum)).io)
    val vldTab  = VecInit.fill(l1Way)(Module(new AsyncRegRam(Bool(), l1IndexNum, 1, 2, Some(false.B))).io)
    val dataTab = VecInit.fill(l1Way)(Module(new XilinxTrueDualPortReadFirstByteWrite1ClockRam(l1Line, 8, l1IndexNum)).io)
    val lruTab  = Module(new AsyncRegRam(UInt(2.W), l1IndexNum, 1, 1, Some(1.U(2.W)))).io

    // Utils
    def index(addr: UInt)    = addr(l1Index+l1Offset-1, l1Offset)
    def offset(addr: UInt)   = addr(l1Offset-1, 0)
    def tag(addr: UInt)      = addr(31, l1Index+l1Offset)
    def tagIndex(addr: UInt) = addr(31, l1Offset)

    // Control modules
    val fsm      = Module(new DCacheFSM)
    val sb       = Module(new StoreBuffer)
    val missC1   = RegInit(false.B)
    val hitC1    = RegInit(0.U(l1Way.W))
    val rbuf     = RegInit(VecInit.fill(l1Line)(0.U(8.W)))
    val rbufMask = RegInit(0.U(l1Line.W))

    // Channel 1 pipeline stages
    val c1s3Wreq = WireDefault(false.B)
    val sbFull   = !sb.io.enq.ready && c1s3Wreq

    // Stage 1: Request
    val c1s1     = (new DChannel1Stage1Signal)(io.pp)
    
    // Stage 2: MMU and hit check
    val c1s2     = ShiftRegister(Mux(io.cmt.flush, 0.U.asTypeOf(new DChannel1Stage1Signal), c1s1), 1, 0.U.asTypeOf(new DChannel1Stage1Signal), !(missC1 || sbFull) || io.cmt.flush)
    val vldC1s2  = vldTab.map(_.rdata(0))
    val tagC1s2  = tagTab.map(_.douta)
    val dataC1s2 = dataTab.map(_.douta)
    val hitC1s2  = VecInit(
        tagC1s2.zip(vldC1s2).map { case (t, v) => t === tag(io.mmu.paddr) && v }
    ).asUInt
    // assert(!c1s2.rreq || PopCount(hitC1s2) <= 1.U, "DCache: channel 1: multiple hits")

    val c1s3In    = (new DChannel1Stage2Signal)(c1s2, io.mmu, VecInit(tagC1s2), VecInit(dataC1s2), hitC1s2, lruTab.rdata(0))
    // miss check: when not latest but uncache, must not miss, for later
    val readMiss  = c1s2.rreq && (io.mmu.uncache || !hitC1s2.orR) && (c1s2.isLatest || !io.mmu.uncache)
    val writeMiss = false.B
    missC1 := Mux(fsm.io.cc.cmiss, false.B, Mux(missC1 || sbFull, missC1, (readMiss || writeMiss) && !io.cmt.flush))

    // stage 3
    val c1s3    = ShiftRegister(Mux(io.cmt.flush, 0.U.asTypeOf(new DChannel1Stage2Signal), c1s3In), 1, 0.U.asTypeOf(new DChannel1Stage2Signal), !missC1 && (!sbFull || io.cmt.flush))
    assert(!c1s3.rreq || PopCount(c1s3.hit) <= 1.U, "DCache: channel 1: multiple hits")
    c1s3Wreq    := c1s3.wreq
    val lruC1s3 = lruTab.rdata(0)
    // store buffer
    val sbEnq = (new SBEntry)(c1s3.paddr, c1s3.wdata, c1s3.mtype, c1s3.uncache)
    sb.io.enq.valid := c1s3.wreq && !missC1 && !io.cmt.flush
    sb.io.enq.bits  := sbEnq
    sb.io.stCmt     := io.cmt.stCmt
    sb.io.stFinish  := io.l2.wrsp
    sb.io.flush     := io.cmt.flush
    sb.io.lock      := fsm.io.cc.sbLock
    // fsm
    fsm.io.cc.rreq     := c1s3.rreq
    fsm.io.cc.wreq     := c1s3.wreq
    fsm.io.cc.uncache  := c1s3.uncache
    fsm.io.cc.hit      := c1s3.hit
    fsm.io.cc.isLatest := c1s3.isLatest
    fsm.io.cc.lru      := lruC1s3
    fsm.io.cc.sbClear  := sb.io.clear
    fsm.io.cc.flush    := io.cmt.flush
    fsm.io.l2.rrsp     := io.l2.rrsp
    fsm.io.l2.miss     := io.l2.miss
    fsm.io.cc.sbFull   := sbFull

    // lru
    lruTab.raddr(0) := index(c1s3.paddr)
    lruTab.wen(0)   := fsm.io.cc.lruUpd.orR
    lruTab.waddr(0) := index(c1s3.paddr)
    lruTab.wdata(0) := fsm.io.cc.lruUpd

    // tag and mem
    tagTab.zipWithIndex.foreach{ case (tagt, i) =>
        tagt.clka  := clock
        tagt.addra := Mux1H(fsm.io.cc.addrOH, VecInit(index(c1s1.vaddr), index(c1s2.vaddr), index(c1s3.vaddr)))
        tagt.ena   := Mux1H(fsm.io.cc.addrOH, VecInit(c1s1.rreq || c1s1.wreq, c1s2.rreq || c1s2.wreq, c1s3.rreq || c1s3.wreq))
        tagt.dina  := tag(c1s3.paddr)
        tagt.wea   := fsm.io.cc.tagvWe(i)
    }
    dataTab.zipWithIndex.foreach{ case (datat, i) =>
        datat.clka  := clock
        datat.addra := Mux1H(fsm.io.cc.addrOH, VecInit(index(c1s1.vaddr), index(c1s2.vaddr), index(c1s3.vaddr)))
        datat.ena   := Mux1H(fsm.io.cc.addrOH, VecInit(c1s1.rreq || c1s1.wreq, c1s2.rreq || c1s2.wreq, c1s3.rreq || c1s3.wreq))
        datat.dina  := rbuf.asUInt
        datat.wea   := Fill(l1Line, fsm.io.cc.memWe(i))
    }
    vldTab.zipWithIndex.foreach{ case (vldt, i) =>
        vldt.raddr(0) := index(c1s2.vaddr)
        vldt.wen(0)   := fsm.io.cc.tagvWe(i)
        vldt.waddr(0) := index(c1s3.vaddr)
        vldt.wdata(0) := true.B
    }
    io.pp.miss        := missC1
    io.pp.loadReplay  := c1s3.uncache && c1s3.rreq && !c1s3.isLatest
    io.pp.sbFull      := sbFull
    val memData = Mux(c1s3.uncache, rbuf.asUInt(31, 0), (Mux1H(fsm.io.cc.r1H, VecInit(Mux1H(c1s3.hit, c1s3.rdata), rbuf.asUInt)).asTypeOf(Vec(l1Line / 4, UInt(32.W))))(offset(c1s3.vaddr) >> 2) >> (offset(c1s3.vaddr)(1, 0) << 3))
    val c1s4In  = (new DChannel1Stage3Signal)(c1s3, sb.io.ldHitData, memData, sb.io.ldSBHit)
    val c1s4    = ShiftRegister(Mux(missC1 || sbFull || io.cmt.flush || io.pp.loadReplay, 0.U.asTypeOf(new DChannel1Stage3Signal), c1s4In), 1, 0.U.asTypeOf(new DChannel1Stage3Signal), true.B)
    val rdata   = VecInit.tabulate(4)(i => Mux(c1s4.sbHit(i) && !c1s4.uncache, c1s4.sbHitData(i*8+7, i*8), c1s4.memData(i*8+7, i*8))).asUInt
    io.pp.rdata := MuxLookup(c1s4.mtype(1, 0), 0.U(32.W))(Seq(
        0.U(2.W) -> Fill(24, Mux(c1s4.mtype(2), 0.U(1.W), rdata(7))) ## rdata(7, 0),
        1.U(2.W) -> Fill(16, Mux(c1s4.mtype(2), 0.U(1.W), rdata(15))) ## rdata(15, 0),
        2.U(2.W) -> rdata,
    ))
    io.pp.rrsp := c1s4.rreq 
    io.pp.wrsp := c1s4.wreq

    /* 
        channel 2: commit write
        stage1: SB commit request
        stage2: generate miss 
        stage3: write(hit: write through and write memory, miss: write through)
    */
    // stage 1

    val c2s1 = (new DChannel2Stage1Signal)(
        sb.io.deq.bits,
        sb.io.deq.valid,
        sb.io.deqIdx
    )
    sb.io.deq.ready := !io.l2.miss && !io.l2.rreq

    // stage 2
    val c2s2    = ShiftRegister(c2s1, 1, 0.U.asTypeOf(new DChannel2Stage1Signal), !io.l2.miss && !io.l2.rreq)
    val vldC2s2 = vldTab.map(_.rdata(1))
    val tagC2s2 = tagTab.map(_.doutb)
    val hitC2s2 = VecInit(tagC2s2.zip(vldC2s2).map { case (t, v) => t === tag(c2s2.paddr) && v }).asUInt

    // stage 3
    val c2s3In  = (new DChannel2Stage2Signal)(c2s2, hitC2s2)
    val c2s3    = ShiftRegister(c2s3In, 1, 0.U.asTypeOf(new DChannel2Stage2Signal), !io.l2.miss && !io.l2.rreq)
    assert(!c2s3.wreq || PopCount(c2s3.hit) <= 1.U, "DCache: channel 2: multiple hits")
    
    val wdataShift = c2s3.wdata << (offset(c2s3.paddr) << 3)
    val wstrbShift = MTypeDecode(c2s3.mtype) << offset(c2s3.paddr)
    fsm.io.cc.c2Wreq := c2s2.wreq || c2s3.wreq
    // tag and mem
    tagTab.zipWithIndex.foreach{ case (tagt, i) =>
        tagt.addrb := Mux(io.l2.miss || io.l2.rreq, index(c2s2.paddr), index(c2s1.paddr))
        tagt.enb   := Mux(io.l2.miss || io.l2.rreq, c2s2.wreq, c2s1.wreq)
        tagt.dinb  := DontCare
        tagt.web   := false.B
    }
    dataTab.zipWithIndex.foreach{ case (datat, i) =>
        datat.addrb := index(c2s3.paddr)
        datat.enb   := c2s3.wreq
        datat.dinb  := wdataShift
        datat.web   := Fill(l1Line, c2s3.wreq && c2s3.hit(i) && !c2s3.uncache) & wstrbShift
    }
    vldTab.zipWithIndex.foreach{ case (vldt, i) =>
        vldt.raddr(1) := index(c2s2.paddr)
    }

    // return buffer
    val rawEn = c2s3.wreq && c1s3.rreq && tagIndex(c2s3.paddr) === tagIndex(c1s3.paddr)
    when(fsm.io.cc.rbufClear){
        rbufMask := Mux(rawEn, wstrbShift, 0.U)
    }.elsewhen(rawEn){
        rbufMask := rbufMask | wstrbShift
    }
    val wMask = MTypeDecode(c2s3.mtype) << offset(c2s3.paddr)
    rbuf.zipWithIndex.foreach{ case (r, i) =>
        when(wstrbShift(i) && rawEn){
            r := wdataShift(i*8+7, i*8)
        }.elsewhen(rbufMask(i)){
            r := r
        }.elsewhen(io.l2.rrsp){
            r := io.l2.rline(i*8+7, i*8)
        }
    }
    // l2 cache
    // rreq: get the posedge of rreq
    io.l2.rreq     := fsm.io.l2.rreq && !ShiftRegister(Mux(io.l2.rrsp, false.B, fsm.io.l2.rreq), 1, false.B, !io.l2.miss || io.l2.rrsp)
    io.l2.wreq     := c2s3.wreq && !io.l2.rreq
    io.l2.paddr    := Mux(io.l2.rreq, c1s3.paddr, c2s3.paddr) // wreq first
    io.l2.uncache  := Mux(io.l2.rreq, c1s3.uncache, c2s3.uncache)
    io.l2.wdata    := c2s3.wdata
    io.l2.mtype    := c2s3.mtype

    val wVisitReg   = RegInit(0.U(64.W))
    val wHitReg     = RegInit(0.U(64.W))
    wVisitReg       := wVisitReg + (!io.l2.miss && !io.l2.rreq && !c2s3.uncache)
    wHitReg         := wHitReg + (!io.l2.miss && !io.l2.rreq && !c2s3.uncache && c2s3.hit.orR)
    io.dbg(0)       := fsm.io.dbg
    io.dbg(1).visit := wVisitReg
    io.dbg(1).hit   := wHitReg
}
