import chisel3._
import chisel3.util._
import ZirconConfig.Cache._
import ZirconConfig.Fetch._
import ZirconUtil._

class IStage1Signal extends Bundle {
    val rreq        = Bool()
    val vaddr       = UInt(32.W)

    def apply(pp: IPipelineIO): IStage1Signal = {
        val c = Wire(new IStage1Signal)
        InheritFields(c, pp)
        c
    }
}

class IStage2Signal extends IStage1Signal {
    val rtag        = Vec(l1Way, UInt(l1Tag.W))
    val rdata       = Vec(l1Way, UInt((32 * nfch).W))
    val hit         = UInt(l1Way.W)
    val lru         = UInt(2.W)
    val paddr       = UInt(32.W)
    val uncache     = Bool()

    def apply(C: IStage1Signal, Mmu: IMMUIO, Tag: Vec[UInt], Data: Vec[UInt], Hit: UInt, Lru: UInt): IStage2Signal = {
        val c = Wire(new IStage2Signal)
        InheritFields(c, C)
        InheritFields(c, Mmu)
        c.rtag    := Tag
        c.rdata   := Data
        c.hit     := Hit
        c.lru     := Lru
        c
    }
}

class IPipelineIO extends Bundle {
    val rreq        = Input(Bool())
    val vaddr       = Input(UInt(32.W))
    val rdata       = Output(Vec(nfch, UInt(32.W)))
    val miss        = Output(Bool())
    val rrsp        = Output(Bool())
    val stall       = Input(Bool())
    val flush       = Input(Bool())
}

class IMMUIO extends Bundle {
    val paddr       = Input(UInt(32.W))
    val uncache     = Input(Bool())
}

abstract class CacheDBG extends Bundle {
    val visit       = UInt(64.W)
    val hit         = UInt(64.W)
}

class ICacheDBG extends CacheDBG {
    val missCycle   = UInt(64.W)
}

class ICacheIO extends Bundle {
    val pp          = new IPipelineIO
    val mmu         = new IMMUIO
    val l2          = Flipped(new L2ICacheIO)
    val dbg         = Output(new ICacheDBG)
}

class ICache extends Module {
    val io = IO(new ICacheIO)
    /*
        ICache has one channels, l1Way now is 2
    */
    // Memory arrays
    val tagTab     = VecInit.fill(l1Way)(Module(new XilinxSinglePortRamReadFirst(l1Tag, l1IndexNum)).io)
    val vldTab     = VecInit.fill(l1Way)(Module(new AsyncRegRam(Bool(), l1IndexNum, 1, 1, Some(false.B))).io)
    val dataTab    = VecInit.fill(l1Way)(Module(new XilinxSinglePortRamReadFirst(icLineBits, l1IndexNum)).io)
    val lruTab     = Module(new AsyncRegRam(UInt(2.W), l1IndexNum, 1, 1, Some(1.U(2.W)))).io
    
    // Utils
    def index(addr: UInt)     = addr(l1Index+l1Offset-1, l1Offset)
    def offset(addr: UInt)    = addr(l1Offset-1, 0)
    def tag(addr: UInt)       = addr(31, l1Index+l1Offset)
    def tagIndex(addr: UInt)  = addr(31, l1Offset)

    // Control modules
    val fsm        = Module(new ICacheFSM)
    val missC1     = RegInit(false.B)
    val rbuf       = RegInit(0.U(l1LineBits.W))
    val dbuf       = RegInit(0.U((32 * nfch).W))
    // val plusReg    = RegInit(0.U(1.W)) // for the second request
    
    // Stage 1: Request
    val c1s1 = (new IStage1Signal)(io.pp)

    // Stage 2: MMU and hit check
    val c1s2 = ShiftRegister(c1s1, 1, 0.U.asTypeOf(new IStage1Signal), !(missC1 || io.pp.stall) || io.pp.flush)
    val vldC1s2    = vldTab.map(_.rdata(0))
    val tagC1s2    = tagTab.map(_.douta)
    val dataC1s2   = dataTab.map(_.douta)
    val hitC1s2    = VecInit(
        tagC1s2.zip(vldC1s2).map { case (t, v) => t === tag(io.mmu.paddr) && v }
    ).asUInt    
    missC1 := Mux(fsm.io.cc.cmiss, false.B, Mux(missC1 || io.pp.stall, missC1, c1s2.rreq && (io.mmu.uncache || !hitC1s2.orR)))
    val rdataC1s2 = dataC1s2.map{case d => (d >> (offset(c1s2.vaddr) << 3)).take(32 * nfch)}
    // val rdataC1s2 = Mux1H(hitC1s2, dataC1s2) >> (offset(c1s2.vaddr) << 3)
    val c1s3In = (new IStage2Signal)(c1s2, io.mmu, VecInit(tagC1s2), VecInit(rdataC1s2), hitC1s2, lruTab.rdata(0))
    // Stage 3: Data selection
    val c1s3 = ShiftRegister(c1s3In, 1, 0.U.asTypeOf(new IStage2Signal), !(missC1 || io.pp.stall))
    assert(!c1s3.rreq || PopCount(c1s3.hit) <= 1.U, "ICache: multiple hits")
    val lruC1s3    = lruTab.rdata(0)
    // fsm
    fsm.io.cc.rreq      := c1s3.rreq
    fsm.io.cc.uncache   := c1s3.uncache
    fsm.io.cc.hit       := c1s3.hit
    fsm.io.cc.lru       := lruC1s3
    fsm.io.cc.stall     := io.pp.stall
    fsm.io.l2.rrsp      := io.l2.rrsp // for two requests
    fsm.io.l2.miss      := io.l2.miss
    fsm.io.cc.flush     := io.pp.flush
    // lru
    lruTab.raddr(0) := index(c1s3.vaddr)
    lruTab.wen(0)   := fsm.io.cc.lruUpd.orR
    lruTab.waddr(0) := index(c1s3.vaddr)
    lruTab.wdata(0) := fsm.io.cc.lruUpd
    // tag and mem
    tagTab.zipWithIndex.foreach{ case (tagt, i) =>
        tagt.clka  := clock
        tagt.addra := MuxOH(fsm.io.cc.addrOH, VecInit(index(c1s1.vaddr), index(c1s2.vaddr), index(c1s3.vaddr)))
        tagt.ena   := MuxOH(fsm.io.cc.addrOH, VecInit(c1s1.rreq, c1s2.rreq, c1s3.rreq))
        tagt.dina  := tag(c1s3.paddr)
        tagt.wea   := fsm.io.cc.tagvWe(i)
    }
    dataTab.zipWithIndex.foreach{ case (datat, i) =>
        datat.clka  := clock
        datat.addra := MuxOH(fsm.io.cc.addrOH, VecInit(index(c1s1.vaddr), index(c1s2.vaddr), index(c1s3.vaddr)))
        datat.ena   := MuxOH(fsm.io.cc.addrOH, VecInit(c1s1.rreq, c1s2.rreq, c1s3.rreq))
        datat.dina  := rbuf
        datat.wea   := fsm.io.cc.memWe(i)
    }
    vldTab.zipWithIndex.foreach{ case (vldt, i) =>
        vldt.raddr(0) := index(c1s2.vaddr)
        vldt.wen(0)   := fsm.io.cc.tagvWe(i)
        vldt.waddr(0) := index(c1s3.vaddr)
        vldt.wdata(0) := true.B
    }
    // rbuf
    when(io.l2.rrsp){
        rbuf := io.l2.rline
        dbuf := Mux(c1s3.uncache, io.l2.rline, io.l2.rline >> (offset(c1s3.vaddr) << 3))
    }
    // plusReg
    // when(io.l2.rreq && !io.l2.miss){
    //     plusReg := plusReg ^ 1.U
    // }
    io.pp.rrsp         := c1s3.rreq && !missC1 && !io.pp.stall
    io.l2.rreq         := fsm.io.l2.rreq
    io.l2.paddr        := c1s3.paddr
    io.l2.uncache      := c1s3.uncache

    io.pp.miss         := missC1
    val rline          = MuxOH(fsm.io.cc.r1H, VecInit(MuxOH(c1s3.hit, c1s3.rdata), dbuf))
    io.pp.rdata.zipWithIndex.foreach{ case (rdata, i) => rdata := rline(i*32+31, i*32) }
    io.dbg := fsm.io.dbg

}