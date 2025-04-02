import chisel3._
import chisel3.util._
import Zircon_Config.Cache._
import Zircon_Util._

class L2_Channel1_Stage1_Signal extends Bundle {
    val rreq        = Bool()
    val paddr       = UInt(32.W)
    val uncache       = Bool()

    def apply(rreq: Bool, paddr: UInt, uncache: Bool): L2_Channel1_Stage1_Signal = {
        val c = Wire(new L2_Channel1_Stage1_Signal)
        c.rreq      := rreq
        c.paddr     := paddr
        c.uncache   := uncache
        c
    }
}
class L2_Channel1_Stage2_Signal extends L2_Channel1_Stage1_Signal{
    val rtag        = Vec(l2_way, UInt(l2_tag.W))
    val rdata       = Vec(l2_way, UInt(l2_line_bits.W))
    val hit         = UInt(l2_way.W)
    val lru         = UInt(2.W)

    def apply(_c: L2_Channel1_Stage1_Signal, rtag: Vec[UInt], rdata: Vec[UInt], hit: UInt, lru: UInt): L2_Channel1_Stage2_Signal = {
        val c = Wire(new L2_Channel1_Stage2_Signal)
        InheritFields(c, _c)
        c.rtag := rtag
        c.rdata := rdata
        c.hit := hit
        c.lru := lru
        c
    }
}
class L2_Channel2_Stage1_Signal extends Bundle {
    val rreq        = Bool()
    val wreq        = Bool()
    val uncache     = Bool()
    val paddr       = UInt(32.W)
    val mtype       = UInt(2.W)
    val wdata       = UInt(32.W)

    def apply(rreq: Bool, wreq: Bool, uncache: Bool, paddr: UInt, mtype: UInt, wdata: UInt): L2_Channel2_Stage1_Signal = {
        val c = Wire(new L2_Channel2_Stage1_Signal)
        c.rreq  := rreq
        c.wreq  := wreq
        c.uncache := uncache
        c.paddr := paddr
        c.mtype := mtype
        c.wdata := wdata
        c
    }
}
class L2_Channel2_Stage2_Signal extends L2_Channel2_Stage1_Signal{
    val rtag        = Vec(l2_way, UInt(l2_tag.W))
    val rdata       = Vec(l1_way, UInt(l2_line_bits.W))
    val hit         = UInt(l2_way.W)
    val lru         = UInt(2.W)

    def apply(_c: L2_Channel2_Stage1_Signal, rtag: Vec[UInt], rdata: Vec[UInt], hit: UInt, lru: UInt): L2_Channel2_Stage2_Signal = {
        val c = Wire(new L2_Channel2_Stage2_Signal)
        InheritFields(c, _c)
        c.rtag := rtag
        c.rdata := rdata
        c.hit := hit
        c.lru := lru
        c
    }
}
class Write_Buffer extends Bundle {
    val paddr  = UInt(32.W)
    val wdata  = UInt(l2_line_bits.W)
}
class L2_ICache_IO extends Bundle{
    val rreq        = Input(Bool())
    val rrsp        = Output(Bool())
    val paddr       = Input(UInt(32.W))
    val uncache     = Input(Bool())
    val rline       = Output(UInt(l1_line_bits.W))
    val miss        = Output(Bool())
}
class L2_DCache_IO extends Bundle {
    // replace port
    val rreq        = Input(Bool())
    val rrsp        = Output(Bool())
    val rline       = Output(UInt(l1_line_bits.W))

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
class Mem_IO(ic: Boolean) extends Bundle {
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
class L2Cache_IO extends Bundle {
    val ic      = new L2_ICache_IO
    val dc      = new L2_DCache_IO
    val mem     = new MixedVec(Seq(new Mem_IO(true), new Mem_IO(false)))
}




class L2Cache extends Module {
    val io = IO(new L2Cache_IO)
    /* 
    This Cache has two channels:
        1. for dcache 
        2. for icache
    ICache: Read Operation can see all the way, but can only refill in way0-1
    Dcache: Read Operation can see all the way, but can only refill in way2-3
    */
    /* 
        memory, now the l2_way is 4 
        way0 and way1 are for icache
        way2 and way3 are for dcache
    */
    // TODO: L1Cache should give L2 the way to be replaced, in order to make sure the L2 includes the data in L1
    // tag
    val tag_tab     = VecInit.fill(l2_way)(Module(new xilinx_true_dual_port_read_first_1_clock_ram(l2_tag, l2_index_num)).io)
    val vld_tab     = VecInit.fill(l2_way)(Module(new AsyncRegRam(Bool(), l2_index_num, 2, 2, false.B)).io)
    // data
    val data_tab    = VecInit.fill(l2_way)(Module(new xilinx_true_dual_port_read_first_byte_write_1_clock_ram(l2_line, 8, l2_index_num)).io)
    // dirty table, dirty for dcache
    val dirty_tab   = VecInit.fill(l1_way)(Module(new AsyncRegRam(Bool(), l2_index_num, 1, 1, false.B)).io)
    // lru, 0 for icache, 1 for dcache, if the kth-bit is 1, the kth-way is the one to be replaced
    val lru_tab     = VecInit.fill(2)(Module(new AsyncRegRam(UInt(2.W), l2_index_num, 1, 1, 1.U(2.W))).io)

    /* hazard */
    val ic_hazard   = WireDefault(false.B)
    val dc_hazard   = WireDefault(false.B)

    /* utils */
    def index(paddr: UInt) = paddr(l2_index+l2_offset-1, l2_offset)
    def offset(paddr: UInt) = paddr(l2_offset-1, 0)
    def tag(paddr: UInt) = paddr(31, l2_index+l2_offset)

    /* 
    channel 1: icache visit
        stage 1: receive icache request
        stage 2: search the tag to determine which line to read
        stage 3: fsm
    */
    
    val fsm_c1      = Module(new L2Cache_FSM(true))
    val miss_c1     = RegInit(false.B)
    val rbuf_c1     = RegInit(0.U(l2_line_bits.W))
    /* stage 1: receive the write through request */
    val c1s1        = (new L2_Channel1_Stage1_Signal)(Mux(ic_hazard, false.B, io.ic.rreq), io.ic.paddr, io.ic.uncache)
    // Segreg1-2
    val c1s2        = ShiftRegister(c1s1, 1, 0.U.asTypeOf(new L2_Channel1_Stage1_Signal), !miss_c1)
    /* stage 2: search the tag to determine which line to write */
    val vld_c1s2    = vld_tab.map(_.rdata(0))
    val hit_c1s2    = VecInit(tag_tab.zip(vld_c1s2).map{ case (tagt, vld) => vld && tagt.douta === tag(c1s2.paddr)}).asUInt

    val c1s3_in     = (new L2_Channel1_Stage2_Signal)(c1s2, VecInit(tag_tab.map(_.douta)), VecInit(data_tab.map(_.douta)), hit_c1s2, lru_tab(0).rdata(0))
    // miss update logic
    miss_c1         := Mux(fsm_c1.io.cc.cmiss, false.B, Mux(miss_c1, miss_c1, c1s2.rreq && (c1s2.uncache || !hit_c1s2.orR)))
    // Segreg2-3
    val c1s3        = ShiftRegister(c1s3_in, 1, 0.U.asTypeOf(new L2_Channel1_Stage2_Signal), !miss_c1)
    assert(!c1s3.rreq || PopCount(c1s3.hit) <= 1.U, "L2Cache: icache visit hit more than one line")
    val c1_lru      = lru_tab(0).rdata(0)
    
    /* stage 3: fsm */
    // fsm
    fsm_c1.io.cc.rreq    := c1s3.rreq
    fsm_c1.io.cc.uncache := c1s3.uncache
    fsm_c1.io.cc.hit     := c1s3.hit
    fsm_c1.io.cc.lru     := c1_lru
    fsm_c1.io.mem.rrsp   := io.mem(0).rrsp
    fsm_c1.io.mem.rlast  := io.mem(0).rlast
    // rbuf
    when(io.mem(0).rreq && io.mem(0).rrsp){
        rbuf_c1 := io.mem(0).rdata ## rbuf_c1(l2_line_bits-1, 32)
    }
    // lru
    lru_tab(0).raddr(0)   := index(c1s3.paddr)
    lru_tab(0).wen(0)     := fsm_c1.io.cc.lru_upd.orR
    lru_tab(0).waddr(0)   := index(c1s3.paddr)
    lru_tab(0).wdata(0)   := fsm_c1.io.cc.lru_upd
    // tag and mem 
    tag_tab.zipWithIndex.foreach{ case (tagt, i) =>
        tagt.clka   := clock
        tagt.addra  := Mux1H(fsm_c1.io.cc.addr_1H, VecInit(index(c1s1.paddr), index(c1s2.paddr), index(c1s3.paddr)))
        tagt.ena    := Mux1H(fsm_c1.io.cc.addr_1H, VecInit(c1s1.rreq, c1s2.rreq, c1s3.rreq))
        tagt.dina   := tag(c1s3.paddr)
        tagt.wea    := (if(i < 2) fsm_c1.io.cc.tagv_we(i) else false.B)
    }
    data_tab.zipWithIndex.foreach{ case (datat, i) =>
        datat.clka   := clock
        datat.addra  := Mux1H(fsm_c1.io.cc.addr_1H, VecInit(index(c1s1.paddr), index(c1s2.paddr), index(c1s3.paddr)))
        datat.ena    := Mux1H(fsm_c1.io.cc.addr_1H, VecInit(c1s1.rreq, c1s2.rreq, c1s3.rreq))
        datat.dina   := rbuf_c1
        datat.wea    := (if(i < 2) Fill(l2_line, fsm_c1.io.cc.mem_we(i)) else Fill(l2_line, false.B))
    }
    vld_tab.zipWithIndex.foreach{ case (vldt, i) =>
        vldt.raddr(0)   := index(c1s2.paddr)
        vldt.wen(0)     := (if(i < 2) fsm_c1.io.cc.tagv_we(i) else false.B)
        vldt.waddr(0)   := index(c1s3.paddr)
        vldt.wdata(0)   := true.B
    }

    io.ic.rrsp      := !miss_c1 && c1s3.rreq
    io.ic.rline     := Mux(c1s3.uncache, 0.U((l1_line_bits-32).W) ## rbuf_c1(l2_line_bits-1, l2_line_bits-32), 
                                        (Mux1H(fsm_c1.io.cc.r1H, VecInit(Mux1H(c1s3.hit, c1s3.rdata), rbuf_c1)).asTypeOf(Vec(l2_line_bits / l1_line_bits, UInt(l1_line_bits.W))))(c1s3.paddr(l2_offset-1, l1_offset)))
    // io.ic.uc_out    := c1s3.uncache
    io.mem(0).rreq  := fsm_c1.io.mem.rreq
    io.mem(0).raddr := tag(c1s3.paddr) ## index(c1s3.paddr) ## Mux(c1s3.uncache, offset(c1s3.paddr), 0.U(l2_offset.W))
    io.mem(0).rlen  := Mux(c1s3.uncache, 0.U, (l2_line_bits / 32 - 1).U)
    io.mem(0).rsize := 2.U

    /*
    channel 2: dcache visit
        stage 1: receive the dcache request
        stage 2: search the tag to determine hit or miss, as well as read the data from the line
        stage 3: read the data from the line
    */

    val fsm_c2      = Module(new L2Cache_FSM(false))
    val miss_c2     = RegInit(false.B)
    val hit_c2      = RegInit(0.U(l2_way.W))
    val wbuf_c2     = RegInit(0.U.asTypeOf(new Write_Buffer))
    val rbuf_c2     = RegInit(0.U(l2_line_bits.W))
    /* stage 1: receive the request, and arbitrate the request */
    val c2s1        = (new L2_Channel2_Stage1_Signal)(Mux(dc_hazard, false.B, io.dc.rreq), Mux(dc_hazard, false.B, io.dc.wreq), io.dc.uncache, io.dc.paddr, io.dc.mtype, io.dc.wdata)
    // Segreg1-2
    val c2s2        = ShiftRegister(c2s1, 1, 0.U.asTypeOf(new L2_Channel2_Stage1_Signal), !miss_c2)
    /* stage 2: search the tag to determine hit or miss, as well as read the data from the line */
    val vld_c2s2    = vld_tab.map(_.rdata(1))
    val hit_c2s2    = VecInit(tag_tab.zip(vld_c2s2).map{ case (tagt, vld) => vld && tagt.doutb === tag(c2s2.paddr)}).asUInt
    // miss update logic
    miss_c2         := Mux(fsm_c2.io.cc.cmiss, false.B, Mux(miss_c2, miss_c2, (c2s2.rreq || c2s2.wreq) && (c2s2.uncache || !hit_c2s2(3, 2).orR)))    
    val c2s3_in     = (new L2_Channel2_Stage2_Signal)(c2s2, VecInit(tag_tab.map(_.doutb)), VecInit(data_tab.map(_.doutb).drop(2)), hit_c2s2, lru_tab(1).rdata(0))
    // Segreg2-3
    val c2s3        = ShiftRegister(c2s3_in, 1, 0.U.asTypeOf(new L2_Channel2_Stage2_Signal), !miss_c2)
    assert(!(c2s3.rreq || c2s3.wreq) || PopCount(c2s3.hit(3, 2)) <= 1.U, "L2Cache: dcache visit hit more than one line")
    /* stage 3: read the data from the line */
    val c2_lru      = lru_tab(1).rdata(0)
    // fsm
    fsm_c2.io.cc.rreq        := c2s3.rreq
    fsm_c2.io.cc.wreq.get    := c2s3.wreq
    fsm_c2.io.cc.uncache     := c2s3.uncache
    fsm_c2.io.cc.hit         := c2s3.hit
    fsm_c2.io.cc.lru         := c2_lru
    fsm_c2.io.cc.drty.get    := dirty_tab.map(_.rdata(0))
    fsm_c2.io.mem.rrsp       := io.mem(1).rrsp
    fsm_c2.io.mem.rlast      := io.mem(1).rlast
    fsm_c2.io.mem.wrsp.get   := io.mem(1).wrsp.get
    // wbuf
    when(fsm_c2.io.cc.wbuf_we){
        wbuf_c2.paddr := Mux(c2s3.uncache, c2s3.paddr, Mux1H(c2_lru, c2s3.rtag.drop(2)) ## index(c2s3.paddr) ## 0.U(l2_offset.W))
        wbuf_c2.wdata := Mux(c2s3.uncache, 0.U((l2_line_bits-32).W) ## c2s3.wdata, Mux1H(c2_lru, c2s3.rdata))
    }.elsewhen(io.mem(1).wreq.get && io.mem(1).wrsp.get){
        wbuf_c2.wdata := wbuf_c2.wdata >> 32
    }
    // rbuf
    when(io.mem(1).rreq && io.mem(1).rrsp){
        rbuf_c2 := io.mem(1).rdata ## rbuf_c2(l2_line_bits-1, 32)
    }
    val mtype = MTypeDecode(c2s3.mtype) 
    val wmask = Mux(!c2s3.wreq, 0.U, VecInit.tabulate(32)(i => mtype(i/8)).asUInt)
    val wmask_shift = wmask << (offset(c2s3.paddr) << 3)
    val wdata_shift = c2s3.wdata << (offset(c2s3.paddr) << 3)
    val wdata_rbuf = rbuf_c2 & ~wmask_shift | wdata_shift & wmask_shift
    val rdata_mem = Mux1H(c2s3.hit(3, 2), c2s3.rdata) & ~wmask_shift | wdata_shift & wmask_shift
    // lru
    lru_tab(1).raddr(0) := index(c2s3.paddr)
    lru_tab(1).wen(0) := fsm_c2.io.cc.lru_upd.orR
    lru_tab(1).waddr(0) := index(c2s3.paddr)
    lru_tab(1).wdata(0) := fsm_c2.io.cc.lru_upd
    
    // dirty
    dirty_tab.zipWithIndex.foreach{ case (dirtyt, i) =>
        dirtyt.raddr(0) := index(c2s3.paddr)
        dirtyt.wen(0)   := fsm_c2.io.cc.drty_we.get(i)
        dirtyt.waddr(0) := index(c2s3.paddr)
        dirtyt.wdata(0) := fsm_c2.io.cc.drty_d.get(i)
    }
    // tag and mem
    tag_tab.zipWithIndex.foreach{ case (tagt, i) =>
        tagt.addrb  := Mux1H(fsm_c2.io.cc.addr_1H, VecInit(index(c2s1.paddr), index(c2s2.paddr), index(c2s3.paddr)))
        tagt.enb    := Mux1H(fsm_c2.io.cc.addr_1H, VecInit(c2s1.rreq || c2s1.wreq, c2s2.rreq || c2s2.wreq, c2s3.rreq || c2s3.wreq))
        tagt.dinb   := tag(c2s3.paddr)
        tagt.web    := (if(i >= 2) fsm_c2.io.cc.tagv_we(i-2) else false.B)
    }
    data_tab.zipWithIndex.foreach{ case (datat, i) =>
        datat.addrb  := Mux1H(fsm_c2.io.cc.addr_1H, VecInit(index(c2s1.paddr), index(c2s2.paddr), index(c2s3.paddr)))
        datat.enb    := Mux1H(fsm_c2.io.cc.addr_1H, VecInit(c2s1.rreq || c2s1.wreq, c2s2.rreq || c2s2.wreq, c2s3.rreq || c2s3.wreq))
        datat.dinb   := Mux(!c2s3.hit(3, 2).orR, wdata_rbuf, c2s3.wdata << (offset(c2s3.paddr) << 3))
        datat.web    := (if(i >= 2) Fill(l2_line, fsm_c2.io.cc.mem_we(i-2)) & Mux(!c2s3.hit(3, 2).orR, Fill(l2_line, fsm_c2.io.cc.mem_we(i-2)), mtype << offset(c2s3.paddr)) else Fill(l2_line, false.B))
    }
    vld_tab.zipWithIndex.foreach{ case (vldt, i) =>
        vldt.raddr(1)   := index(c2s2.paddr)
        vldt.wen(1)     := (if(i >= 2) fsm_c2.io.cc.tagv_we(i-2) else fsm_c2.io.cc.vld_inv.get(i))
        vldt.waddr(1)   := index(c2s3.paddr)
        vldt.wdata(1)   := (if(i < 2) Mux(fsm_c2.io.cc.vld_inv.get(i), false.B, true.B) else true.B)
    }

    io.dc.rrsp          := !miss_c2 && c2s3.rreq
    io.dc.rline         := Mux(c2s3.uncache, 0.U((l1_line_bits-32).W) ## rbuf_c2(l2_line_bits-1, l2_line_bits-32), 
                                        (Mux1H(fsm_c2.io.cc.r1H, VecInit(rdata_mem, wdata_rbuf)).asTypeOf(Vec(l2_line_bits / l1_line_bits, UInt(l1_line_bits.W))))(c2s3.paddr(l2_offset-1, l1_offset)))
    io.dc.wrsp          := !miss_c2 && c2s3.wreq
    io.mem(1).rreq      := fsm_c2.io.mem.rreq
    io.mem(1).raddr     := tag(c2s3.paddr) ## index(c2s3.paddr) ## Mux(c2s3.uncache, offset(c2s3.paddr), 0.U(l2_offset.W))
    io.mem(1).rlen      := Mux(c2s3.uncache, 0.U, (l2_line_bits / 32 - 1).U)
    io.mem(1).rsize     := Mux(c2s3.uncache, c2s3.mtype, 2.U)
    io.mem(1).wreq.get  := fsm_c2.io.mem.wreq.get
    io.mem(1).wlast.get := fsm_c2.io.mem.wlast.get
    io.mem(1).waddr.get := wbuf_c2.paddr(31, 2) ## 0.U(2.W)
    io.mem(1).wdata.get := (wbuf_c2.wdata(31, 0) << (wbuf_c2.paddr(1, 0) << 3))
    io.mem(1).wlen.get  := Mux(c2s3.uncache, 0.U, (l2_line_bits / 32 - 1).U)
    io.mem(1).wsize.get := 2.U
    io.mem(1).wstrb.get := Mux(c2s3.uncache, mtype << wbuf_c2.paddr(1, 0), 0xf.U)

    /* related check: */
    dc_hazard := c2s3.wreq

    io.ic.miss := miss_c1 || ic_hazard
    io.dc.miss := miss_c2 || dc_hazard
}