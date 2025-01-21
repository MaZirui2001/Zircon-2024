import chisel3._
import chisel3.util._
import CPU_Config.Cache._
import Zircon_Util._
class Channel1_Stage1_Signal extends Bundle {
    val rreq        = Bool()
    val paddr       = UInt(32.W)
    val uc_in       = Bool()

    def apply(rreq: Bool, paddr: UInt, uc_in: Bool): Channel1_Stage1_Signal = {
        val c = Wire(new Channel1_Stage1_Signal)
        c.rreq  := rreq
        c.paddr := paddr
        c.uc_in := uc_in
        c
    }
}
class Channel1_Stage2_Signal extends Channel1_Stage1_Signal{
    val rtag        = Vec(l2_way, UInt(l2_tag.W))
    val rdata       = Vec(l2_way, UInt(l2_line_bits.W))
    val hit         = UInt(l2_way.W)
    val lru         = Vec(l2_way, Bool())

    def apply(_c: Channel1_Stage1_Signal, rtag: Vec[UInt], rdata: Vec[UInt], hit: UInt, lru: Vec[Bool]): Channel1_Stage2_Signal = {
        val c = Wire(new Channel1_Stage2_Signal)
        inheritFields(c, _c)
        c.rtag := rtag
        c.rdata := rdata
        c.hit := hit
        c.lru := lru
        c
    }
}
class Channel2_Stage1_Signal extends Bundle {
    val rreq        = Bool()
    val wreq        = Bool()
    val uc_in       = Bool()
    val paddr       = UInt(32.W)
    val mtype       = UInt(2.W)
    val wdata       = UInt(32.W)

    def apply(rreq: Bool, wreq: Bool, uc_in: Bool, paddr: UInt, mtype: UInt, wdata: UInt): Channel2_Stage1_Signal = {
        val c = Wire(new Channel2_Stage1_Signal)
        c.rreq  := rreq
        c.wreq  := wreq
        c.uc_in := uc_in
        c.paddr := paddr
        c.mtype := mtype
        c.wdata := wdata
        c
    }
}
class Channel2_Stage2_Signal extends Channel2_Stage1_Signal{
    val rtag        = Vec(l2_way, UInt(l2_tag.W))
    val rdata       = Vec(l2_way, UInt(l2_line_bits.W))
    val hit         = UInt(l2_way.W)
    val lru         = Vec(l2_way, Bool())

    def apply(_c: Channel2_Stage1_Signal, rtag: Vec[UInt], rdata: Vec[UInt], hit: UInt, lru: Vec[Bool]): Channel2_Stage2_Signal = {
        val c = Wire(new Channel2_Stage2_Signal)
        inheritFields(c, _c)
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
class ICache_IO extends Bundle{
    val rreq        = Input(Bool())
    val rrsp        = Output(Bool())
    val paddr       = Input(UInt(32.W))
    val uc_in       = Input(Bool())
    val rline       = Output(UInt(ic_line_bits.W))
    val uc_out      = Output(Bool())
    val miss        = Output(Bool())
}
class DCache_IO extends Bundle {
    // replace port
    val rreq        = Input(Bool())
    val rrsp        = Output(Bool())
    val rline       = Output(UInt(dc_line_bits.W))
    val uc_out      = Output(Bool())
    val paddr_out   = Output(UInt(32.W))

    // write through port
    val wreq        = Input(Bool())
    val wrsp        = Output(Bool())

    // shared port
    val paddr_in    = Input(UInt(32.W))
    val uc_in       = Input(Bool())
    val wdata       = Input(UInt(32.W))
    val mtype       = Input(UInt(2.W))
    val miss        = Output(Bool())
}
class Mem_IO extends Bundle {
    val rreq        = Output(Bool())
    val rrsp        = Input(Bool())
    val rlast       = Input(Bool())
    val raddr       = Output(UInt(32.W))
    val rdata       = Input(UInt(32.W))
    val rlen        = Output(UInt(8.W))
    val rsize       = Output(UInt(2.W))

    val wreq        = Output(Bool())
    val wrsp        = Input(Bool())
    val wlast       = Output(Bool())
    val waddr       = Output(UInt(32.W))
    val wdata       = Output(UInt(32.W))
    val wlen        = Output(UInt(8.W))
    val wsize       = Output(UInt(2.W))
    val wstrb       = Output(UInt(4.W))
}
class L2Cache_IO extends Bundle {
    val ic      = new ICache_IO
    val dc      = new DCache_IO
    val mem     = Vec(2, new Mem_IO)
}




class L2Cache extends Module {
    val io = IO(new L2Cache_IO)
    /* 
    This Cache has two channels:
        1. for dcache write through, it can't be blocked, because all the write through data will hit
        2. for icache and dcache miss, it use a fsm to handle the miss
    */
    /* memory, now the l2_way is 2 */
    // tag
    val tag_tab     = VecInit.fill(l2_way)(Module(new xilinx_true_dual_port_read_first_1_clock_ram(l2_tag, l2_index_num)).io)
    val vld_tab     = VecInit.fill(l2_way)(Module(new AsyncRegRam(Bool(), l2_index_num, 2, 2, false.B)).io)
    // data
    val data_tab    = VecInit.fill(l2_way)(Module(new xilinx_true_dual_port_read_first_byte_write_1_clock_ram(l2_line, 8, l2_index_num)).io)
    // dirty
    val dirty_tab   = VecInit.fill(l2_way)(Module(new AsyncRegRam(Bool(), l2_index_num, 2, 2, false.B)).io)
    // lru
    val lru_tab     = VecInit.tabulate(l2_way)(i => Module(new AsyncRegRam(Bool(), l2_index_num, 2, 2, if(i == 0) true.B else false.B)).io)

    /* hazard */
    val ic_hazard   = Wire(Bool())
    val dc_hazard   = Wire(Bool())

    /* utils */
    def index(paddr: UInt) = paddr(l2_index+l2_offset-1, l2_offset)
    def offset(paddr: UInt) = paddr(l2_offset-1, 0)
    def tag(paddr: UInt) = paddr(31, l2_index+l2_offset)

    /* 
    channel 1: icache visit
        stage 1: receive the write through request
        stage 2: search the tag to determine which line to read
        stage 3: fsm
    */
    
    val fsm_c1      = Module(new L2Cache_FSM(true))
    val miss_c1     = RegInit(false.B)
    val hit_c1      = RegInit(0.U(l2_way.W))
    val wbuf_c1     = RegInit(0.U.asTypeOf(new Write_Buffer))
    val rbuf_c1     = RegInit(0.U(l2_line_bits.W))
    /* stage 1: receive the write through request */
    val c1s1        = (new Channel1_Stage1_Signal)(Mux(ic_hazard, false.B, io.ic.rreq), io.ic.paddr, io.ic.uc_in)
    // Segreg1-2
    val c1s2        = ShiftRegister(c1s1, 1, 0.U.asTypeOf(new Channel1_Stage1_Signal), !miss_c1)
    /* stage 2: search the tag to determine which line to write */
    val vld_c1s2    = vld_tab.map(_.rdata(0))
    val hit_c1s2    = VecInit(tag_tab.zip(vld_c1s2).map{ case (tagt, vld) => vld && tagt.douta === tag(c1s2.paddr)}).asUInt
    assert(!c1s2.rreq || PopCount(hit_c1s2) <= 1.U, "icache visit hit more than one line")

    val c1s3_in     = (new Channel1_Stage2_Signal)(c1s2, VecInit(tag_tab.map(_.douta)), VecInit(data_tab.map(_.douta)), hit_c1s2, VecInit(lru_tab.map(_.rdata(0))))
    // miss update logic
    miss_c1         := Mux(fsm_c1.io.cache.cmiss, false.B, Mux(miss_c1, miss_c1, c1s2.rreq && (c1s2.uc_in || !hit_c1s2.orR)))
    // Segreg2-3
    val c1s3        = ShiftRegister(c1s3_in, 1, 0.U.asTypeOf(new Channel1_Stage2_Signal), !miss_c1)
    val c1_lru      = lru_tab.map(_.rdata(0))
    /* stage 3: fsm */
    dirty_tab.zipWithIndex.foreach{ case (dirtyt, i) =>
        dirtyt.raddr(0) := index(c1s3.paddr)
        dirtyt.wen(0)   := fsm_c1.io.cache.drty_we(i)
        dirtyt.waddr(0) := index(c1s3.paddr)
        dirtyt.wdata(0) := fsm_c1.io.cache.drty_d(i)
    }
    // fsm
    fsm_c1.io.cache.rreq    := c1s3.rreq
    fsm_c1.io.cache.uc_in   := c1s3.uc_in
    fsm_c1.io.cache.hit     := c1s3.hit
    fsm_c1.io.cache.lru     := c1_lru
    fsm_c1.io.cache.drty    := dirty_tab.map(_.rdata(0))
    fsm_c1.io.mem.rrsp      := io.mem(0).rrsp
    fsm_c1.io.mem.rlast     := io.mem(0).rlast
    fsm_c1.io.mem.wrsp      := io.mem(0).wrsp
    // wbuf
    when(fsm_c1.io.cache.wbuf_we){
        wbuf_c1.paddr := Mux1H(c1_lru, c1s3.rtag) ## index(c1s3.paddr) ## 0.U(l2_offset.W)
        wbuf_c1.wdata := Mux1H(c1_lru, c1s3.rdata)
    }.elsewhen(io.mem(0).wreq && io.mem(0).wrsp){
        wbuf_c1.wdata := wbuf_c1.wdata >> 32
    }
    // rbuf
    when(io.mem(0).rreq && io.mem(0).rrsp){
        rbuf_c1 := io.mem(0).rdata ## rbuf_c1(l2_line_bits-1, 32)
    }
    // lru
    lru_tab.zipWithIndex.foreach{ case (lrut, i) =>
        lrut.raddr(0)   := index(c1s3.paddr)
        lrut.wen(0)     := fsm_c1.io.cache.lru_upd.asUInt.orR
        lrut.waddr(0)   := index(c1s3.paddr)
        lrut.wdata(0)   := fsm_c1.io.cache.lru_upd(i)
    }
    // tag and mem 
    tag_tab.zipWithIndex.foreach{ case (tagt, i) =>
        tagt.clka   := clock
        tagt.addra  := Mux1H(fsm_c1.io.cache.addr_1H, VecInit(index(c1s1.paddr), index(c1s2.paddr), index(c1s3.paddr)))
        tagt.ena    := Mux1H(fsm_c1.io.cache.addr_1H, VecInit(c1s1.rreq, c1s2.rreq, c1s3.rreq))
        tagt.dina   := tag(c1s3.paddr)
        tagt.wea    := fsm_c1.io.cache.tagv_we(i)
    }
    data_tab.zipWithIndex.foreach{ case (datat, i) =>
        datat.clka   := clock
        datat.addra  := Mux1H(fsm_c1.io.cache.addr_1H, VecInit(index(c1s1.paddr), index(c1s2.paddr), index(c1s3.paddr)))
        datat.ena    := Mux1H(fsm_c1.io.cache.addr_1H, VecInit(c1s1.rreq, c1s2.rreq, c1s3.rreq))
        datat.dina   := rbuf_c1
        datat.wea    := Fill(l2_line, fsm_c1.io.cache.mem_we(i))
    }
    vld_tab.zipWithIndex.foreach{ case (vldt, i) =>
        vldt.raddr(0)   := index(c1s2.paddr)
        vldt.wen(0)     := fsm_c1.io.cache.tagv_we(i)
        vldt.waddr(0)   := index(c1s3.paddr)
        vldt.wdata(0)   := true.B
    }

    io.ic.rrsp      := !miss_c1 && c1s3.rreq
    io.ic.rline     := (Mux1H(fsm_c1.io.cache.r1H, VecInit(Mux1H(c1s3.hit, c1s3.rdata), rbuf_c1)).asTypeOf(Vec(l2_line_bits / ic_line_bits, UInt(ic_line_bits.W))))(c1s3.paddr(l2_offset-1, ic_offset))
    io.ic.uc_out    := c1s3.uc_in
    io.mem(0).rreq  := fsm_c1.io.mem.rreq
    io.mem(0).raddr := tag(c1s3.paddr) ## index(c1s3.paddr) ## Mux(c1s3.uc_in, offset(c1s3.paddr), 0.U(l2_offset.W))
    io.mem(0).rlen  := Mux(c1s3.uc_in, 0.U, (l2_line_bits / 32 - 1).U)
    io.mem(0).rsize := 2.U
    io.mem(0).wreq  := fsm_c1.io.mem.wreq
    io.mem(0).wlast := fsm_c1.io.mem.wlast
    io.mem(0).waddr := wbuf_c1.paddr
    io.mem(0).wdata := wbuf_c1.wdata(31, 0)
    io.mem(0).wlen  := (l2_line_bits / 32 - 1).U
    io.mem(0).wsize := 2.U
    io.mem(0).wstrb := 0xf.U

    /*
    channel 2: dcache visit
        stage 1: receive the request, and arbitrate the request
        stage 2: search the tag to determine hit or miss, as well as read the data from the line
        stage 3: read the data from the line
    */

    val fsm_c2      = Module(new L2Cache_FSM)
    val miss_c2     = RegInit(false.B)
    val hit_c2      = RegInit(0.U(l2_way.W))
    val wbuf_c2     = RegInit(0.U.asTypeOf(new Write_Buffer))
    val rbuf_c2     = RegInit(0.U(l2_line_bits.W))
    /* stage 1: receive the request, and arbitrate the request */
    val c2s1        = (new Channel2_Stage1_Signal)(Mux(dc_hazard, false.B, io.dc.rreq), Mux(dc_hazard, false.B, io.dc.wreq), io.dc.uc_in, io.dc.paddr_in, io.dc.mtype, io.dc.wdata)
    // Segreg1-2
    val c2s2        = ShiftRegister(c2s1, 1, 0.U.asTypeOf(new Channel2_Stage1_Signal), !miss_c2)
    /* stage 2: search the tag to determine hit or miss, as well as read the data from the line */
    val vld_c2s2    = vld_tab.map(_.rdata(1))
    val hit_c2s2    = VecInit(tag_tab.zip(vld_c2s2).map{ case (tagt, vld) => vld && tagt.doutb === tag(c2s2.paddr)}).asUInt
    assert(!(c2s2.rreq || c2s2.wreq) || PopCount(hit_c2s2) <= 1.U, "dcache miss hit more than one line")
    // miss update logic
    miss_c2         := Mux(fsm_c2.io.cache.cmiss, false.B, Mux(miss_c2, miss_c2, (c2s2.rreq || c2s2.wreq) && (c2s2.uc_in || !hit_c2s2.orR)))    
    val c2s3_in     = (new Channel2_Stage2_Signal)(c2s2, VecInit(tag_tab.map(_.doutb)), VecInit(data_tab.map(_.doutb)), hit_c2s2, VecInit(lru_tab.map(_.rdata(1))))
    // Segreg2-3
    val c2s3        = ShiftRegister(c2s3_in, 1, 0.U.asTypeOf(new Channel2_Stage2_Signal), !miss_c2)
    /* stage 3: read the data from the line */
    val c2_lru      = lru_tab.map(_.rdata(1))
    // fsm
    fsm_c2.io.cache.rreq        := c2s3.rreq
    fsm_c2.io.cache.wreq.get    := c2s3.wreq
    fsm_c2.io.cache.uc_in       := c2s3.uc_in
    fsm_c2.io.cache.hit         := c2s3.hit
    fsm_c2.io.cache.lru         := c2_lru
    fsm_c2.io.cache.drty        := dirty_tab.map(_.rdata(1))
    fsm_c2.io.mem.rrsp          := io.mem(1).rrsp
    fsm_c2.io.mem.rlast         := io.mem(1).rlast
    fsm_c2.io.mem.wrsp          := io.mem(1).wrsp
    // wbuf
    when(fsm_c2.io.cache.wbuf_we){
        wbuf_c2.paddr := Mux(c2s3.uc_in, c2s3.paddr, Mux1H(c2_lru, c2s3.rtag) ## index(c2s3.paddr) ## 0.U(l2_offset.W))
        wbuf_c2.wdata := Mux(c2s3.uc_in, 0.U((l2_line_bits-32).W) ## c2s3.wdata, Mux1H(c2_lru, c2s3.rdata))
    }.elsewhen(io.mem(1).wreq && io.mem(1).wrsp){
        wbuf_c2.wdata := wbuf_c2.wdata >> 32
    }
    // rbuf
    when(io.mem(1).rreq && io.mem(1).rrsp){
        rbuf_c2 := io.mem(1).rdata ## rbuf_c2(l2_line_bits-1, 32)
    }
    val mtype = mtype_decode(c2s3.mtype) 
    val wmask = Mux(!c2s3.wreq, 0.U, VecInit.tabulate(32)(i => mtype(i/8)).asUInt)
    val wmask_shift = wmask << (offset(c2s3.paddr) << 3)
    val wdata_shift = c2s3.wdata << (offset(c2s3.paddr) << 3)
    val wdata_rbuf = rbuf_c2 & ~wmask_shift | wdata_shift & wmask_shift
    val rdata_mem = Mux1H(c2s3.hit, c2s3.rdata) & ~wmask_shift |  wdata_shift & wmask_shift
    // lru
    lru_tab.zipWithIndex.foreach{ case (lrut, i) =>
        lrut.raddr(1) := index(c2s3.paddr)
        lrut.wen(1) := fsm_c2.io.cache.lru_upd.asUInt.orR
        lrut.waddr(1) := index(c2s3.paddr)
        lrut.wdata(1) := fsm_c2.io.cache.lru_upd(i)
    }
    
    // dirty
    dirty_tab.zipWithIndex.foreach{ case (dirtyt, i) =>
        dirtyt.raddr(1) := index(c2s3.paddr)
        dirtyt.wen(1)   := fsm_c2.io.cache.drty_we(i)
        dirtyt.waddr(1) := index(c2s3.paddr)
        dirtyt.wdata(1) := fsm_c2.io.cache.drty_d(i)
    }
    // tag and mem
    tag_tab.zipWithIndex.foreach{ case (tagt, i) =>
        tagt.addrb  := Mux1H(fsm_c2.io.cache.addr_1H, VecInit(index(c2s1.paddr), index(c2s2.paddr), index(c2s3.paddr)))
        tagt.enb    := Mux1H(fsm_c2.io.cache.addr_1H, VecInit(c2s1.rreq || c2s1.wreq, c2s2.rreq || c2s2.wreq, c2s3.rreq || c2s3.wreq))
        tagt.dinb   := tag(c2s3.paddr)
        tagt.web    := fsm_c2.io.cache.tagv_we(i)
    }
    data_tab.zipWithIndex.foreach{ case (datat, i) =>
        datat.addrb  := Mux1H(fsm_c2.io.cache.addr_1H, VecInit(index(c2s1.paddr), index(c2s2.paddr), index(c2s3.paddr)))
        datat.enb    := Mux1H(fsm_c2.io.cache.addr_1H, VecInit(c2s1.rreq || c2s1.wreq, c2s2.rreq || c2s2.wreq, c2s3.rreq || c2s3.wreq))
        datat.dinb   := Mux(!c2s3.hit.orR, wdata_rbuf, c2s3.wdata << (offset(c2s3.paddr) << 3))
        datat.web    := Fill(l2_line, fsm_c2.io.cache.mem_we(i)) & Mux(!c2s3.hit.orR, Fill(l2_line, fsm_c2.io.cache.mem_we(i)), mtype << offset(c2s3.paddr))
    }
    vld_tab.zipWithIndex.foreach{ case (vldt, i) =>
        vldt.raddr(1)   := index(c2s2.paddr)
        vldt.wen(1)     := fsm_c2.io.cache.tagv_we(i)
        vldt.waddr(1)   := index(c2s3.paddr)
        vldt.wdata(1)   := true.B
    }

    io.dc.rrsp      := !miss_c2 && c2s3.rreq
    io.dc.rline     := (Mux1H(fsm_c2.io.cache.r1H, VecInit(rdata_mem, wdata_rbuf)).asTypeOf(Vec(l2_line_bits / dc_line_bits, UInt(dc_line_bits.W))))(c2s3.paddr(l2_offset-1, dc_offset))
    io.dc.uc_out    := c2s3.uc_in
    io.dc.paddr_out := c2s3.paddr
    io.dc.wrsp      := !miss_c2 && c2s3.wreq
    io.mem(1).rreq  := fsm_c2.io.mem.rreq
    io.mem(1).raddr := tag(c2s3.paddr) ## index(c2s3.paddr) ## Mux(c2s3.uc_in, offset(c2s3.paddr), 0.U(l2_offset.W))
    io.mem(1).rlen  := Mux(c2s3.uc_in, 0.U, (l2_line_bits / 32 - 1).U)
    io.mem(1).rsize := Mux(c2s3.uc_in, c2s3.mtype, 2.U)
    io.mem(1).wreq  := fsm_c2.io.mem.wreq
    io.mem(1).wlast := fsm_c2.io.mem.wlast
    io.mem(1).waddr := wbuf_c2.paddr(31, 2) ## 0.U(2.W)
    io.mem(1).wdata := (wbuf_c2.wdata(31, 0) << (wbuf_c2.paddr(1, 0) << 3))
    io.mem(1).wlen  := Mux(c2s3.uc_in, 0.U, (l2_line_bits / 32 - 1).U)
    io.mem(1).wsize := 2.U
    io.mem(1).wstrb := Mux(c2s3.uc_in, mtype << wbuf_c2.paddr(1, 0), 0xf.U)

    /* related check: 
        reason:     1) The icache may replace a line that the dcache is writing through.
                    2) When the icache is writing back a line, the dcache may write the line.
                    3) Only one port for dcache, so the write in stage 3 must use the same port as the read in stage 1.
                    Both of the above two cases will make the data that will be write into the replaced line to be lost.
        
        solution:   1) If the icache addr in stage 1 is same as the dcache addr whether in any stage, the icache should wait.
                    2) If the dcache addr in stage 1 is same as the icache addr in stage 2 or 3, the dcache should wait.
                       (The reason why I don't compare the icache addr in stage 1 is to avoid the deadlock.)
                    3) If dcache write in stage 3, the dcache read should wait.
        result:     Using combinational logic to make signal 'miss' to be true. 
                    It seems that the l2 cache is missing from the l1 caches.
    */
    ic_hazard := (index(c1s1.paddr) === index(c2s1.paddr) && io.dc.wreq
               || index(c1s1.paddr) === index(c2s2.paddr) && c2s2.wreq
               || index(c1s1.paddr) === index(c2s3.paddr) && c2s3.wreq)
    dc_hazard := (index(c2s1.paddr) === index(c1s2.paddr) && c1s2.rreq
               || index(c2s1.paddr) === index(c1s3.paddr) && c1s3.rreq
               || io.dc.rreq && c2s3.wreq)

    io.ic.miss := miss_c1 || ic_hazard
    io.dc.miss := miss_c2 || dc_hazard
}