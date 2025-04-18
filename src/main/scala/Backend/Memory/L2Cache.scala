import chisel3._
import chisel3.util._
import ZirconConfig.Cache._
import ZirconUtil._

class L2Channel1Stage1Signal extends Bundle {
    val rreq        = Bool()
    val paddr       = UInt(32.W)
    val uncache     = Bool()

    def apply(rreq: Bool, paddr: UInt, uncache: Bool): L2Channel1Stage1Signal = {
        val c = Wire(new L2Channel1Stage1Signal)
        c.rreq      := rreq
        c.paddr     := paddr
        c.uncache   := uncache
        c
    }
}
class L2Channel1Stage2Signal extends L2Channel1Stage1Signal{
    val rtag        = Vec(l2Way, UInt(l2Tag.W))
    val rdata       = Vec(l2Way, UInt(l2LineBits.W))
    val hit         = UInt(l2Way.W)
    val lru         = UInt(2.W)

    def apply(C: L2Channel1Stage1Signal, rtag: Vec[UInt], rdata: Vec[UInt], hit: UInt, lru: UInt): L2Channel1Stage2Signal = {
        val c = Wire(new L2Channel1Stage2Signal)
        InheritFields(c, C)
        c.rtag := rtag
        c.rdata := rdata
        c.hit := hit
        c.lru := lru
        c
    }
}
class L2Channel2Stage1Signal extends Bundle {
    val rreq        = Bool()
    val wreq        = Bool()
    val uncache     = Bool()
    val paddr       = UInt(32.W)
    val mtype       = UInt(2.W)
    val wdata       = UInt(32.W)

    def apply(rreq: Bool, wreq: Bool, uncache: Bool, paddr: UInt, mtype: UInt, wdata: UInt): L2Channel2Stage1Signal = {
        val c = Wire(new L2Channel2Stage1Signal)
        c.rreq  := rreq
        c.wreq  := wreq
        c.uncache := uncache
        c.paddr := paddr
        c.mtype := mtype
        c.wdata := wdata
        c
    }
}
class L2Channel2Stage2Signal extends L2Channel2Stage1Signal{
    val rtag        = Vec(l2Way, UInt(l2Tag.W))
    val rdata       = Vec(l1Way, UInt(l2LineBits.W))
    val hit         = UInt(l2Way.W)
    val lru         = UInt(2.W)

    def apply(C: L2Channel2Stage1Signal, rtag: Vec[UInt], rdata: Vec[UInt], hit: UInt, lru: UInt): L2Channel2Stage2Signal = {
        val c = Wire(new L2Channel2Stage2Signal)
        InheritFields(c, C)
        c.rtag := rtag
        c.rdata := rdata
        c.hit := hit
        c.lru := lru
        c
    }
}
class WriteBuffer extends Bundle {
    val paddr  = UInt(32.W)
    val wdata  = UInt(l2LineBits.W)
}

class L2ICacheIO extends Bundle{
    val rreq        = Input(Bool())
    val rrsp        = Output(Bool())
    val paddr       = Input(UInt(32.W))
    val uncache     = Input(Bool())
    val rline       = Output(UInt(l1LineBits.W))
    val miss        = Output(Bool())
}
class L2DCacheIO extends Bundle {
    // replace port
    val rreq        = Input(Bool())
    val rrsp        = Output(Bool())
    val rline       = Output(UInt(l1LineBits.W))

    // write through port
    val wreq        = Input(Bool())
    val wrsp        = Output(Bool())

    // shared port
    val paddr       = Input(UInt(32.W))
    val uncache     = Input(Bool())
    val wdata       = Input(UInt(32.W))
    val mtype       = Input(UInt(2.W))
    val miss        = Output(Bool())
}
class MemIO(ic: Boolean) extends Bundle {
    val rreq        = Output(Bool())
    val rrsp        = Input(Bool())
    val rlast       = Input(Bool())
    val raddr       = Output(UInt(32.W))
    val rdata       = Input(UInt(32.W))
    val rlen        = Output(UInt(8.W))
    val rsize       = Output(UInt(2.W))

    val wreq        = if(ic) None else Some(Output(Bool()))
    val wrsp        = if(ic) None else Some(Input(Bool()))
    val wlast       = if(ic) None else Some(Output(Bool()))
    val waddr       = if(ic) None else Some(Output(UInt(32.W)))
    val wdata       = if(ic) None else Some(Output(UInt(32.W)))
    val wlen        = if(ic) None else Some(Output(UInt(8.W)))
    val wsize       = if(ic) None else Some(Output(UInt(2.W)))
    val wstrb       = if(ic) None else Some(Output(UInt(4.W)))
}

class L2CacheIO extends Bundle {
    val ic  = new L2ICacheIO
    val dc  = new L2DCacheIO
    val mem = new MixedVec(Seq(new MemIO(true), new MemIO(false)))
    val dbg = Output(Vec(2, new L2CacheDBG))
}

class L2Cache extends Module {
    val io = IO(new L2CacheIO)
    /* 
    This Cache has two channels:
        1. for dcache 
        2. for icache
    ICache: Read Operation can see all the way, but can only refill in way0-1
    Dcache: Read Operation can see all the way, but can only refill in way2-3
    */
    /* 
        memory, now the l2Way is 4 
        way0 and way1 are for icache
        way2 and way3 are for dcache
    */
    // TODO: L1Cache should give L2 the way to be replaced, in order to make sure the L2 includes the data in L1
    // tag
    val tagTab     = VecInit.fill(l2Way)(Module(new XilinxTrueDualPortReadFirst1ClockRam(l2Tag, l2IndexNum)).io)
    val vldTab     = VecInit.fill(l2Way)(Module(new AsyncRegRam(Bool(), l2IndexNum, 2, 2, Some(false.B))).io)
    // data
    val dataTab    = VecInit.fill(l2Way)(Module(new XilinxTrueDualPortReadFirstByteWrite1ClockRam(l2Line, 8, l2IndexNum)).io)
    // dirty table, dirty for dcache
    val dirtyTab   = VecInit.fill(l1Way)(Module(new AsyncRegRam(Bool(), l2IndexNum, 1, 1, Some(false.B))).io)
    // lru, 0 for icache, 1 for dcache, if the kth-bit is 1, the kth-way is the one to be replaced
    val lruTab     = VecInit.fill(2)(Module(new AsyncRegRam(UInt(2.W), l2IndexNum, 1, 1, Some(1.U(2.W)))).io)

    /* hazard */
    val icHazard   = WireDefault(false.B)
    val dcHazard   = WireDefault(false.B)

    /* utils */
    def index(paddr: UInt) = paddr(l2Index+l2Offset-1, l2Offset)
    def offset(paddr: UInt) = paddr(l2Offset-1, 0)
    def tag(paddr: UInt) = paddr(31, l2Index+l2Offset)

    /* 
    channel 1: icache visit
        stage 1: receive icache request
        stage 2: search the tag to determine which line to read
        stage 3: fsm
    */
    
    val fsmC1      = Module(new L2CacheFSM(true))
    val missC1     = RegInit(false.B)
    val rbufC1     = RegInit(0.U(l2LineBits.W))
    /* stage 1: receive the write through request */
    val c1s1       = (new L2Channel1Stage1Signal)(Mux(icHazard, false.B, io.ic.rreq), io.ic.paddr, io.ic.uncache)
    // Segreg1-2
    val c1s2       = ShiftRegister(c1s1, 1, 0.U.asTypeOf(new L2Channel1Stage1Signal), !missC1)
    /* stage 2: search the tag to determine which line to write */
    val vldC1s2    = vldTab.map(_.rdata(0))
    val hitC1s2    = VecInit(tagTab.zip(vldC1s2).map{ case (tagt, vld) => vld && tagt.douta === tag(c1s2.paddr)}).asUInt

    val c1s3In     = (new L2Channel1Stage2Signal)(c1s2, VecInit(tagTab.map(_.douta)), VecInit(dataTab.map(_.douta)), hitC1s2, lruTab(0).rdata(0))
    // miss update logic
    missC1         := Mux(fsmC1.io.cc.cmiss, false.B, Mux(missC1, missC1, c1s2.rreq && (c1s2.uncache || !hitC1s2.orR)))
    // Segreg2-3
    val c1s3       = ShiftRegister(c1s3In, 1, 0.U.asTypeOf(new L2Channel1Stage2Signal), !missC1)
    assert(!c1s3.rreq || PopCount(c1s3.hit) <= 1.U, "L2Cache: icache visit hit more than one line")
    val c1Lru      = lruTab(0).rdata(0)
    
    /* stage 3: fsm */
    // fsm
    fsmC1.io.cc.rreq    := c1s3.rreq
    fsmC1.io.cc.uncache := c1s3.uncache
    fsmC1.io.cc.hit     := c1s3.hit
    fsmC1.io.cc.lru     := c1Lru
    fsmC1.io.mem.rrsp   := io.mem(0).rrsp
    fsmC1.io.mem.rlast  := io.mem(0).rlast
    // rbuf
    when(io.mem(0).rreq && io.mem(0).rrsp){
        rbufC1 := io.mem(0).rdata ## rbufC1(l2LineBits-1, 32)
    }
    // lru
    lruTab(0).raddr(0)   := index(c1s3.paddr)
    lruTab(0).wen(0)     := fsmC1.io.cc.lruUpd.orR
    lruTab(0).waddr(0)   := index(c1s3.paddr)
    lruTab(0).wdata(0)   := fsmC1.io.cc.lruUpd
    // tag and mem 
    tagTab.zipWithIndex.foreach{ case (tagt, i) =>
        tagt.clka   := clock
        tagt.addra  := Mux1H(fsmC1.io.cc.addrOH, VecInit(index(c1s1.paddr), index(c1s2.paddr), index(c1s3.paddr)))
        tagt.ena    := Mux1H(fsmC1.io.cc.addrOH, VecInit(c1s1.rreq, c1s2.rreq, c1s3.rreq))
        tagt.dina   := tag(c1s3.paddr)
        tagt.wea    := (if(i < 2) fsmC1.io.cc.tagvWe(i) else false.B)
    }
    dataTab.zipWithIndex.foreach{ case (datat, i) =>
        datat.clka   := clock
        datat.addra  := Mux1H(fsmC1.io.cc.addrOH, VecInit(index(c1s1.paddr), index(c1s2.paddr), index(c1s3.paddr)))
        datat.ena    := Mux1H(fsmC1.io.cc.addrOH, VecInit(c1s1.rreq, c1s2.rreq, c1s3.rreq))
        datat.dina   := rbufC1
        datat.wea    := (if(i < 2) Fill(l2Line, fsmC1.io.cc.memWe(i)) else Fill(l2Line, false.B))
    }
    vldTab.zipWithIndex.foreach{ case (vldt, i) =>
        vldt.raddr(0)   := index(c1s2.paddr)
        vldt.wen(0)     := (if(i < 2) fsmC1.io.cc.tagvWe(i) else false.B)
        vldt.waddr(0)   := index(c1s3.paddr)
        vldt.wdata(0)   := true.B
    }

    io.ic.rrsp      := !missC1 && c1s3.rreq
    io.ic.rline     := Mux(c1s3.uncache, 0.U((l1LineBits-32).W) ## rbufC1(l2LineBits-1, l2LineBits-32), 
                                        (Mux1H(fsmC1.io.cc.r1H, VecInit(Mux1H(c1s3.hit, c1s3.rdata), rbufC1)).asTypeOf(Vec(l2LineBits / l1LineBits, UInt(l1LineBits.W))))(c1s3.paddr(l2Offset-1, l1Offset)))
    // io.ic.ucOut    := c1s3.uncache
    io.mem(0).rreq  := fsmC1.io.mem.rreq
    io.mem(0).raddr := tag(c1s3.paddr) ## index(c1s3.paddr) ## Mux(c1s3.uncache, offset(c1s3.paddr), 0.U(l2Offset.W))
    io.mem(0).rlen  := Mux(c1s3.uncache, 0.U, (l2LineBits / 32 - 1).U)
    io.mem(0).rsize := 2.U


    /*
    channel 2: dcache visit
        stage 1: receive the dcache request
        stage 2: search the tag to determine hit or miss, as well as read the data from the line
        stage 3: read the data from the line
    */

    val fsmC2      = Module(new L2CacheFSM(false))
    val missC2     = RegInit(false.B)
    val hitC2      = RegInit(0.U(l2Way.W))
    val wbufC2     = RegInit(0.U.asTypeOf(new WriteBuffer))
    val rbufC2     = RegInit(0.U(l2LineBits.W))
    /* stage 1: receive the request, and arbitrate the request */
    val c2s1       = (new L2Channel2Stage1Signal)(Mux(dcHazard, false.B, io.dc.rreq), Mux(dcHazard, false.B, io.dc.wreq), io.dc.uncache, io.dc.paddr, io.dc.mtype, io.dc.wdata)
    // Segreg1-2
    val c2s2       = ShiftRegister(c2s1, 1, 0.U.asTypeOf(new L2Channel2Stage1Signal), !missC2)
    /* stage 2: search the tag to determine hit or miss, as well as read the data from the line */
    val vldC2s2    = vldTab.map(_.rdata(1))
    val hitC2s2    = VecInit(tagTab.zip(vldC2s2).map{ case (tagt, vld) => vld && tagt.doutb === tag(c2s2.paddr)}).asUInt
    // miss update logic
    missC2         := Mux(fsmC2.io.cc.cmiss, false.B, Mux(missC2, missC2, (c2s2.rreq || c2s2.wreq) && (c2s2.uncache || !hitC2s2(3, 2).orR)))    
    val c2s3In     = (new L2Channel2Stage2Signal)(c2s2, VecInit(tagTab.map(_.doutb)), VecInit(dataTab.map(_.doutb).drop(2)), hitC2s2, lruTab(1).rdata(0))
    // Segreg2-3
    val c2s3       = ShiftRegister(c2s3In, 1, 0.U.asTypeOf(new L2Channel2Stage2Signal), !missC2)
    assert(!(c2s3.rreq || c2s3.wreq) || PopCount(c2s3.hit(3, 2)) <= 1.U, "L2Cache: dcache visit hit more than one line")
    /* stage 3: read the data from the line */
    val c2Lru      = lruTab(1).rdata(0)
    // fsm
    fsmC2.io.cc.rreq        := c2s3.rreq
    fsmC2.io.cc.wreq.get    := c2s3.wreq
    fsmC2.io.cc.uncache     := c2s3.uncache
    fsmC2.io.cc.hit         := c2s3.hit
    fsmC2.io.cc.lru         := c2Lru
    fsmC2.io.cc.drty.get    := dirtyTab.map(_.rdata(0))
    fsmC2.io.mem.rrsp       := io.mem(1).rrsp
    fsmC2.io.mem.rlast      := io.mem(1).rlast
    fsmC2.io.mem.wrsp.get   := io.mem(1).wrsp.get
    // wbuf
    when(fsmC2.io.cc.wbufWe){
        wbufC2.paddr := Mux(c2s3.uncache, c2s3.paddr, Mux1H(c2Lru, c2s3.rtag.drop(2)) ## index(c2s3.paddr) ## 0.U(l2Offset.W))
        wbufC2.wdata := Mux(c2s3.uncache, 0.U((l2LineBits-32).W) ## c2s3.wdata, Mux1H(c2Lru, c2s3.rdata))
    }.elsewhen(io.mem(1).wreq.get && io.mem(1).wrsp.get){
        wbufC2.wdata := wbufC2.wdata >> 32
    }
    // rbuf
    when(io.mem(1).rreq && io.mem(1).rrsp){
        rbufC2 := io.mem(1).rdata ## rbufC2(l2LineBits-1, 32)
    }
    val mtype       = MTypeDecode(c2s3.mtype) 
    val wmask       = Mux(!c2s3.wreq, 0.U, VecInit.tabulate(32)(i => mtype(i/8)).asUInt)
    val wmaskShift  = wmask << (offset(c2s3.paddr) << 3)
    val wdataShift  = c2s3.wdata << (offset(c2s3.paddr) << 3)
    val wdataRbuf   = rbufC2 & ~wmaskShift | wdataShift & wmaskShift
    val rdataMem    = Mux1H(c2s3.hit(3, 2), c2s3.rdata) & ~wmaskShift | wdataShift & wmaskShift
    // lru
    lruTab(1).raddr(0)  := index(c2s3.paddr)
    lruTab(1).wen(0)    := fsmC2.io.cc.lruUpd.orR
    lruTab(1).waddr(0)  := index(c2s3.paddr)
    lruTab(1).wdata(0)  := fsmC2.io.cc.lruUpd
    
    // dirty
    dirtyTab.zipWithIndex.foreach{ case (dirtyt, i) =>
        dirtyt.raddr(0) := index(c2s3.paddr)
        dirtyt.wen(0)   := fsmC2.io.cc.drtyWe.get(i)
        dirtyt.waddr(0) := index(c2s3.paddr)
        dirtyt.wdata(0) := fsmC2.io.cc.drtyD.get(i)
    }
    // tag and mem
    tagTab.zipWithIndex.foreach{ case (tagt, i) =>
        tagt.addrb  := Mux1H(fsmC2.io.cc.addrOH, VecInit(index(c2s1.paddr), index(c2s2.paddr), index(c2s3.paddr)))
        tagt.enb    := Mux1H(fsmC2.io.cc.addrOH, VecInit(c2s1.rreq || c2s1.wreq, c2s2.rreq || c2s2.wreq, c2s3.rreq || c2s3.wreq))
        tagt.dinb   := tag(c2s3.paddr)
        tagt.web    := (if(i >= 2) fsmC2.io.cc.tagvWe(i-2) else false.B)
    }
    dataTab.zipWithIndex.foreach{ case (datat, i) =>
        datat.addrb  := Mux1H(fsmC2.io.cc.addrOH, VecInit(index(c2s1.paddr), index(c2s2.paddr), index(c2s3.paddr)))
        datat.enb    := Mux1H(fsmC2.io.cc.addrOH, VecInit(c2s1.rreq || c2s1.wreq, c2s2.rreq || c2s2.wreq, c2s3.rreq || c2s3.wreq))
        datat.dinb   := Mux(!c2s3.hit(3, 2).orR, wdataRbuf, c2s3.wdata << (offset(c2s3.paddr) << 3))
        datat.web    := (if(i >= 2) Fill(l2Line, fsmC2.io.cc.memWe(i-2)) & Mux(!c2s3.hit(3, 2).orR, Fill(l2Line, fsmC2.io.cc.memWe(i-2)), mtype << offset(c2s3.paddr)) else Fill(l2Line, false.B))
    }
    vldTab.zipWithIndex.foreach{ case (vldt, i) =>
        vldt.raddr(1)   := index(c2s2.paddr)
        vldt.wen(1)     := (if(i >= 2) fsmC2.io.cc.tagvWe(i-2) else fsmC2.io.cc.vldInv.get(i))
        vldt.waddr(1)   := index(c2s3.paddr)
        vldt.wdata(1)   := (if(i < 2) Mux(fsmC2.io.cc.vldInv.get(i), false.B, true.B) else true.B)
    }

    io.dc.rrsp          := !missC2 && c2s3.rreq
    io.dc.rline         := Mux(c2s3.uncache, 0.U((l1LineBits-32).W) ## rbufC2(l2LineBits-1, l2LineBits-32), 
                                        (Mux1H(fsmC2.io.cc.r1H, VecInit(rdataMem, wdataRbuf)).asTypeOf(Vec(l2LineBits / l1LineBits, UInt(l1LineBits.W))))(c2s3.paddr(l2Offset-1, l1Offset)))
    io.dc.wrsp          := !missC2 && c2s3.wreq
    io.mem(1).rreq      := fsmC2.io.mem.rreq
    io.mem(1).raddr     := tag(c2s3.paddr) ## index(c2s3.paddr) ## Mux(c2s3.uncache, offset(c2s3.paddr), 0.U(l2Offset.W))
    io.mem(1).rlen      := Mux(c2s3.uncache, 0.U, (l2LineBits / 32 - 1).U)
    io.mem(1).rsize     := Mux(c2s3.uncache, c2s3.mtype, 2.U)
    io.mem(1).wreq.get  := fsmC2.io.mem.wreq.get
    io.mem(1).wlast.get := fsmC2.io.mem.wlast.get
    io.mem(1).waddr.get := wbufC2.paddr(31, 2) ## 0.U(2.W)
    io.mem(1).wdata.get := (wbufC2.wdata(31, 0) << (wbufC2.paddr(1, 0) << 3))
    io.mem(1).wlen.get  := Mux(c2s3.uncache, 0.U, (l2LineBits / 32 - 1).U)
    io.mem(1).wsize.get := 2.U
    io.mem(1).wstrb.get := Mux(c2s3.uncache, mtype << wbufC2.paddr(1, 0), 0xf.U)

    /* related check: */
    dcHazard := c2s3.wreq

    io.ic.miss := missC1 || icHazard
    io.dc.miss := missC2 || dcHazard

    io.dbg(0) := fsmC1.io.dbg
    io.dbg(1) := fsmC2.io.dbg
}