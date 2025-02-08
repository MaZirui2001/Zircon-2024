import chisel3._
import chisel3.util._
import CPU_Config.Cache._
import CPU_Config.StoreBuffer._
import Zircon_Util._

class D_Channel1_Stage1_Signal extends Bundle {
    val rreq        = Bool()
    val vaddr       = UInt(32.W)
    val mtype       = UInt(3.W)
    val is_latest   = Bool()
    val wreq        = Bool()
    val wdata       = UInt(32.W)

    def apply(pp: D_Pipeline_IO): D_Channel1_Stage1_Signal = {
        val c = Wire(new D_Channel1_Stage1_Signal)
        inheritFields(c, pp)
        c
    }
}

class D_Channel1_Stage2_Signal extends D_Channel1_Stage1_Signal {
    val rtag        = Vec(l1_way, UInt(l1_tag.W))
    val rdata       = Vec(l1_way, UInt(l1_line_bits.W))
    val hit         = UInt(l1_way.W)
    val lru         = UInt(2.W)
    val paddr       = UInt(32.W)
    val uncache     = Bool()

    def apply(_c: D_Channel1_Stage1_Signal, rtag: Vec[UInt], rdata: Vec[UInt], hit: UInt, lru: UInt, paddr: UInt, uncache: Bool): D_Channel1_Stage2_Signal = {
        val c = Wire(new D_Channel1_Stage2_Signal)
        inheritFields(c, _c)
        c.rtag := rtag
        c.rdata := rdata
        c.hit := hit
        c.lru := lru
        c.paddr := paddr
        c.uncache := uncache
        c
    }
}

class D_Channel1_Stage3_Signal extends D_Channel1_Stage2_Signal {
    val sb_hit_data = UInt(32.W)
    val mem_data    = UInt(32.W)
    val sb_hit      = UInt(4.W)

    def apply(_c: D_Channel1_Stage2_Signal, sb_hit_data: UInt, mem_data: UInt, sb_hit: UInt): D_Channel1_Stage3_Signal = {
        val c = Wire(new D_Channel1_Stage3_Signal)
        inheritFields(c, _c)
        c.sb_hit_data := sb_hit_data
        c.mem_data := mem_data
        c.sb_hit := sb_hit
        c
    }
}

class D_Channel2_Stage1_Signal extends Bundle {
    val wreq        = Bool()
    val wdata       = UInt(32.W)
    val paddr       = UInt(32.W)
    val mtype       = UInt(3.W)
    val uncache     = Bool()
    val sb_idx      = UInt(wsb.W)

    def apply(sb_entry: sb_entry, sb_valid: Bool, sb_idx: UInt): D_Channel2_Stage1_Signal = {
        val c = Wire(new D_Channel2_Stage1_Signal)
        c.wreq := sb_valid
        c.wdata := sb_entry.wdata >> (sb_entry.paddr(1, 0) << 3)
        c.paddr := sb_entry.paddr
        c.mtype := mtype_encode(sb_entry.wstrb >> sb_entry.paddr(1, 0))
        c.uncache := sb_entry.uncache
        c.sb_idx := sb_idx
        c
    }
}

class D_Channel2_Stage2_Signal extends D_Channel2_Stage1_Signal {
    val hit         = UInt(l1_way.W)
        
    def apply(_c: D_Channel2_Stage1_Signal, hit: UInt): D_Channel2_Stage2_Signal = {
        val c = Wire(new D_Channel2_Stage2_Signal)
        inheritFields(c, _c)
        c.hit := hit
        c
    }
}

class D_Pipeline_IO extends Bundle {
    val rreq        = Input(Bool())
    val mtype       = Input(UInt(3.W))
    val is_latest   = Input(Bool()) // inform the rrequest is issued in order
    val wreq        = Input(Bool())
    val wdata       = Input(UInt(32.W))
    val vaddr       = Input(UInt(32.W))
    val rdata       = Output(UInt(32.W))
    val miss        = Output(Bool()) // not latest but uncache
    val rrsp        = Output(Bool())
    val load_replay = Output(Bool())
    val sb_full     = Output(Bool())
    val wrsp        = Output(Bool())
}

class D_Commit_IO extends Bundle {
    val st_cmt      = Input(Bool())
    val flush       = Input(Bool())
}

class D_MMU_IO extends Bundle {
    val paddr       = Input(UInt(32.W))
    val uncache     = Input(Bool())
    val exception   = Input(UInt(8.W)) // 1 cycle latency
}

class DCache_IO extends Bundle {
    val mmu = new D_MMU_IO
    val pp = new D_Pipeline_IO
    val cmt = new D_Commit_IO
    val l2 = Flipped(new L2_DCache_IO)
}

class DCache extends Module {
    val io = IO(new DCache_IO)

    /* 
    DCache (write through and write unallocate) has two channels:
        1. read and first write
        2. commit write
        l1_way now is 2
    */
    // tag
    val tag_tab     = VecInit.fill(l1_way)(Module(new xilinx_true_dual_port_read_first_1_clock_ram(l1_tag, l1_index_num)).io)
    val vld_tab     = VecInit.fill(l1_way)(Module(new AsyncRegRam(Bool(), l1_index_num, 1, 2, false.B)).io)
    // data
    val data_tab    = VecInit.fill(l1_way)(Module(new xilinx_true_dual_port_read_first_byte_write_1_clock_ram(l1_line, 8, l1_index_num)).io)
    // lru: 0 for channel 1if the kth-bit is 1, the kth-way is the one to be replaced
    val lru_tab     = Module(new AsyncRegRam(UInt(2.W), l1_index_num, 1, 1, 1.U(2.W))).io

    /* utils */
    def index(addr: UInt) = addr(l1_index+l1_offset-1, l1_offset)
    def offset(addr: UInt) = addr(l1_offset-1, 0)
    def tag(addr: UInt) = addr(31, l1_index+l1_offset)

    /* 
        channel 1: read and first write
        stage1: receive request
        stage2: MMU and hit check
        stage3: generate miss and visit store buffer
        stage4: generate rdata
    */
    val fsm         = Module(new DCache_FSM)
    val miss_c1     = RegInit(false.B)
    // val cmiss_c1    = WireDefault(false.B)
    val hit_c1      = RegInit(0.U(l1_way.W))
    val rbuf        = RegInit(0.U(l1_line_bits.W))
    val sb          = Module(new Store_Buffer)

    // stage 1
    val c1s1 = (new D_Channel1_Stage1_Signal)(io.pp)
    
    // stage 2
    val c1s2        = ShiftRegister(Mux(io.cmt.flush, 0.U.asTypeOf(new D_Channel1_Stage1_Signal), c1s1), 1, 0.U.asTypeOf(new D_Channel1_Stage1_Signal), !(miss_c1 || !sb.io.enq.ready) || io.cmt.flush)
    val vld_c1s2    = vld_tab.map(_.rdata(0))
    val tag_c1s2    = tag_tab.map(_.douta)
    val data_c1s2   = data_tab.map(_.douta)
    val hit_c1s2    = VecInit(tag_c1s2.zip(vld_c1s2).map { case (t, v) => t === tag(io.mmu.paddr) && v }).asUInt
    assert(!c1s2.rreq || PopCount(hit_c1s2) <= 1.U, "DCache: channel 1: multiple hits")

    val c1s3_in = (new D_Channel1_Stage2_Signal)(c1s2, VecInit(tag_c1s2), VecInit(data_c1s2), hit_c1s2, lru_tab.rdata(0), io.mmu.paddr, io.mmu.uncache)
    // miss check
    miss_c1 := Mux(fsm.io.cc.cmiss, false.B, Mux(miss_c1 || !sb.io.enq.ready, miss_c1, (c1s2.rreq && (io.mmu.uncache || !hit_c1s2.orR)) || (c1s2.wreq && io.mmu.uncache)))

    // stage 3
    val c1s3        = ShiftRegister(Mux(io.cmt.flush, 0.U.asTypeOf(new D_Channel1_Stage2_Signal), c1s3_in), 1, 0.U.asTypeOf(new D_Channel1_Stage2_Signal), !(miss_c1 || !sb.io.enq.ready) || io.cmt.flush)
    val lru_c1s3    = lru_tab.rdata(0)
    // store buffer
    val sb_enq = (new sb_entry)(c1s3.paddr, c1s3.wdata, c1s3.mtype, c1s3.uncache)
    sb.io.enq.valid     := c1s3.wreq && !miss_c1
    sb.io.enq.bits      := sb_enq
    sb.io.st_cmt        := io.cmt.st_cmt
    sb.io.st_finish     := io.l2.wrsp
    sb.io.flush         := io.cmt.flush
    // fsm
    fsm.io.cc.rreq      := c1s3.rreq
    fsm.io.cc.wreq      := c1s3.wreq
    fsm.io.cc.uncache   := c1s3.uncache
    fsm.io.cc.hit       := c1s3.hit
    fsm.io.cc.is_latest := c1s3.is_latest
    fsm.io.cc.lru       := lru_c1s3
    fsm.io.cc.sb_clear  := sb.io.clear
    fsm.io.cc.flush     := io.cmt.flush
    fsm.io.l2.rrsp      := io.l2.rrsp
    fsm.io.l2.miss      := io.l2.miss
    fsm.io.cc.sb_full   := !sb.io.enq.ready
    // return buffer
    when(io.l2.rrsp){
        rbuf := io.l2.rline
    }
    // lru
    lru_tab.raddr(0) := index(c1s3.paddr)
    lru_tab.wen(0)   := fsm.io.cc.lru_upd.orR
    lru_tab.waddr(0) := index(c1s3.paddr)
    lru_tab.wdata(0) := fsm.io.cc.lru_upd

    // tag and mem
    tag_tab.zipWithIndex.foreach{ case (tagt, i) =>
        tagt.clka  := clock
        tagt.addra := Mux1H(fsm.io.cc.addr_1H, VecInit(index(c1s1.vaddr), index(c1s2.vaddr), index(c1s3.vaddr)))
        tagt.ena   := Mux1H(fsm.io.cc.addr_1H, VecInit(c1s1.rreq || c1s1.wreq, c1s2.rreq || c1s2.wreq, c1s3.rreq || c1s3.wreq))
        tagt.dina  := tag(c1s3.paddr)
        tagt.wea   := fsm.io.cc.tagv_we(i)
    }
    data_tab.zipWithIndex.foreach{ case (datat, i) =>
        datat.clka  := clock
        datat.addra := Mux1H(fsm.io.cc.addr_1H, VecInit(index(c1s1.vaddr), index(c1s2.vaddr), index(c1s3.vaddr)))
        datat.ena   := Mux1H(fsm.io.cc.addr_1H, VecInit(c1s1.rreq || c1s1.wreq, c1s2.rreq || c1s2.wreq, c1s3.rreq || c1s3.wreq))
        datat.dina  := rbuf
        datat.wea   := Fill(l1_line, fsm.io.cc.mem_we(i))
    }
    vld_tab.zipWithIndex.foreach{ case (vldt, i) =>
        vldt.raddr(0) := index(c1s2.vaddr)
        vldt.wen(0)   := fsm.io.cc.tagv_we(i)
        vldt.waddr(0) := index(c1s3.vaddr)
        vldt.wdata(0) := true.B
    }
    io.pp.miss          := miss_c1
    io.pp.load_replay   := c1s3.uncache && c1s3.rreq && !c1s3.is_latest
    io.pp.sb_full       := !sb.io.enq.ready
    val mem_data        = Mux(c1s3.uncache, rbuf(31, 0), (Mux1H(fsm.io.cc.r1H, VecInit(Mux1H(c1s3.hit, c1s3.rdata), rbuf)).asTypeOf(Vec(l1_line / 4, UInt(32.W))))(offset(c1s3.vaddr) >> 2) >> (offset(c1s3.vaddr)(1, 0) << 3))
    val c1s4_in = (new D_Channel1_Stage3_Signal)(c1s3, sb.io.ld_hit_data, mem_data, sb.io.ld_sb_hit)
    val c1s4    = ShiftRegister(Mux(miss_c1 || !sb.io.enq.ready || io.cmt.flush || io.pp.load_replay, 0.U.asTypeOf(new D_Channel1_Stage3_Signal), c1s4_in), 1, 0.U.asTypeOf(new D_Channel1_Stage3_Signal), true.B)
    val rdata   = VecInit.tabulate(4)(i => Mux(c1s4.sb_hit(i) && !c1s4.uncache, c1s4.sb_hit_data(i*8+7, i*8), c1s4.mem_data(i*8+7, i*8))).asUInt
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
    val c2s1 = (new D_Channel2_Stage1_Signal)(
        sb.io.deq.bits,
        sb.io.deq.valid,
        sb.io.deq_idx
    )
    sb.io.deq.ready := !io.l2.miss

    // stage
    val c2s2        = ShiftRegister(c2s1, 1, 0.U.asTypeOf(new D_Channel2_Stage1_Signal), !io.l2.miss)
    val vld_c2s2    = vld_tab.map(_.rdata(1))
    val tag_c2s2    = tag_tab.map(_.doutb)
    val hit_c2s2    = VecInit(tag_c2s2.zip(vld_c2s2).map { case (t, v) => t === tag(c2s2.paddr) && v }).asUInt
    assert(!c2s2.wreq || PopCount(hit_c2s2) <= 1.U, "DCache: channel 2: multiple hits")

    // stage 3
    val c2s3_in = (new D_Channel2_Stage2_Signal)(c2s2, hit_c2s2)
    val c2s3 = ShiftRegister(c2s3_in, 1, 0.U.asTypeOf(new D_Channel2_Stage2_Signal), !io.l2.miss)
    // tag and mem
    tag_tab.zipWithIndex.foreach{ case (tagt, i) =>
        tagt.addrb := Mux(io.l2.miss, index(c2s2.paddr), index(c2s1.paddr))
        tagt.enb   := Mux(io.l2.miss, c2s2.wreq, c2s1.wreq)
        tagt.dinb  := DontCare
        tagt.web   := false.B
    }
    data_tab.zipWithIndex.foreach{ case (datat, i) =>
        datat.addrb := index(c2s3.paddr)
        datat.enb   := c2s3.wreq
        datat.dinb  := c2s3.wdata << (offset(c2s3.paddr) << 3.U)
        datat.web   := Fill(l1_line, c2s3.wreq && c2s3.hit(i)) & (mtype_decode(c2s3.mtype) << offset(c2s3.paddr))
    }
    vld_tab.zipWithIndex.foreach{ case (vldt, i) =>
        vldt.raddr(1) := index(c2s2.paddr)
    }
    // l2 cache
    // rreq: get the posedge of rreq
    io.l2.rreq      := fsm.io.l2.rreq & !ShiftRegister(fsm.io.l2.rreq, 1, false.B, (!io.l2.miss && sb.io.clear)) 
    io.l2.wreq      := c2s3.wreq
    io.l2.paddr     := Mux(c2s3.wreq, c2s3.paddr, c1s3.paddr) // wreq first
    io.l2.uncache   := Mux(c2s3.wreq, c2s3.uncache, c1s3.uncache)
    io.l2.wdata     := c2s3.wdata
    io.l2.mtype     := c2s3.mtype
}
