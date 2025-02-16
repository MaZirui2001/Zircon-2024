import chisel3._
import chisel3.util._
import CPU_Config.Cache._
import CPU_Config.Fetch._
import Zircon_Util._

class I_Stage1_Signal extends Bundle {
    val rreq        = Bool()
    val vaddr       = UInt(32.W)

    def apply(pp: I_Pipeline_IO): I_Stage1_Signal = {
        val c = Wire(new I_Stage1_Signal)
        inheritFields(c, pp)
        c
    }
}

class I_Stage2_Signal extends I_Stage1_Signal {
    val rtag        = Vec(l1_way, UInt(l1_tag.W))
    val rdata       = Vec(l1_way, UInt(ic_line_bits.W))
    val hit         = UInt(l1_way.W)
    val lru         = UInt(2.W)
    val paddr       = UInt(32.W)
    val uncache     = Bool()

    def apply(_c: I_Stage1_Signal, _mmu: I_MMU_IO, _tag: Vec[UInt], _data: Vec[UInt], _hit: UInt, _lru: UInt): I_Stage2_Signal = {
        val c = Wire(new I_Stage2_Signal)
        inheritFields(c, _c)
        inheritFields(c, _mmu)
        c.rtag    := _tag
        c.rdata   := _data
        c.hit     := _hit
        c.lru     := _lru
        c
    }
}

class I_Pipeline_IO extends Bundle {
    val rreq        = Input(Bool())
    val vaddr       = Input(UInt(32.W))
    val rdata       = Output(Vec(nfetch, UInt(32.W)))
    val miss        = Output(Bool())
    val rrsp        = Output(Bool())
    val stall       = Input(Bool())
}

class I_MMU_IO extends Bundle {
    val paddr       = Input(UInt(32.W))
    val uncache     = Input(Bool())
}

class ICache_IO extends Bundle {
    val pp          = new I_Pipeline_IO
    val mmu         = new I_MMU_IO
    val l2          = Flipped(new L2_ICache_IO)
}

class ICache extends Module {
    val io = IO(new ICache_IO)
    /*
    ICache has one channels, l1_way now is 2
    */
    // Memory arrays
    val tag_tab     = VecInit.fill(l1_way)(Module(new xilinx_single_port_ram_read_first(l1_tag, l1_index_num)).io)
    val vld_tab     = VecInit.fill(l1_way)(Module(new AsyncRegRam(Bool(), l1_index_num, 1, 1, false.B)).io)
    val data_tab    = VecInit.fill(l1_way)(Module(new xilinx_single_port_ram_read_first(ic_line_bits, l1_index_num)).io)
    val lru_tab     = Module(new AsyncRegRam(UInt(2.W), l1_index_num, 1, 1, 1.U(2.W))).io
    
    // Utils
    def index(addr: UInt)      = addr(l1_index+l1_offset-1, l1_offset)
    def offset(addr: UInt)     = addr(l1_offset-1, 0)
    def tag(addr: UInt)        = addr(31, l1_index+l1_offset)
    def tag_index(addr: UInt)  = addr(31, l1_offset)

    // Control modules
    val fsm         = Module(new ICache_FSM)
    val miss_c1     = RegInit(false.B)
    val rbuf        = RegInit(0.U((2*l1_line_bits).W))
    val plus_reg    = RegInit(0.U(1.W)) // for the second request
    
    // Stage 1: Request
    val c1s1 = (new I_Stage1_Signal)(io.pp)

    // Stage 2: MMU and hit check
    val c1s2 = ShiftRegister(c1s1, 1, 0.U.asTypeOf(new I_Stage1_Signal), !(miss_c1 || io.pp.stall))
    val vld_c1s2    = vld_tab.map(_.rdata(0))
    val tag_c1s2    = tag_tab.map(_.douta)
    val data_c1s2   = data_tab.map(_.douta)
    val hit_c1s2    = VecInit(
        tag_c1s2.zip(vld_c1s2).map { case (t, v) => t === tag(io.mmu.paddr) && v }
    ).asUInt    
    miss_c1 := Mux(fsm.io.cc.cmiss, false.B, Mux(miss_c1 || io.pp.stall, miss_c1, c1s2.rreq && (io.mmu.uncache || !hit_c1s2.orR)))

    val c1s3_in = (new I_Stage2_Signal)(c1s2, io.mmu, VecInit(tag_c1s2), VecInit(data_c1s2), hit_c1s2, lru_tab.rdata(0))
    // Stage 3: Data selection
    val c1s3 = ShiftRegister(c1s3_in, 1, 0.U.asTypeOf(new I_Stage2_Signal), !(miss_c1 || io.pp.stall))
    assert(!c1s3.rreq || PopCount(c1s3.hit) <= 1.U, "ICache: multiple hits")
    val lru_c1s3    = lru_tab.rdata(0)
    // fsm
    fsm.io.cc.rreq      := c1s3.rreq
    fsm.io.cc.uncache   := c1s3.uncache
    fsm.io.cc.hit       := c1s3.hit
    fsm.io.cc.lru       := lru_c1s3
    fsm.io.cc.stall     := io.pp.stall
    fsm.io.l2.rrsp      := io.l2.rrsp && ShiftRegister(io.l2.rrsp, 1, false.B, !io.l2.miss) // for two requests
    fsm.io.l2.miss      := io.l2.miss
    // lru
    lru_tab.raddr(0) := index(c1s3.vaddr)
    lru_tab.wen(0)   := fsm.io.cc.lru_upd.orR
    lru_tab.waddr(0) := index(c1s3.vaddr)
    lru_tab.wdata(0) := fsm.io.cc.lru_upd
    // tag and mem
    tag_tab.zipWithIndex.foreach{ case (tagt, i) =>
        tagt.clka  := clock
        tagt.addra := Mux1H(fsm.io.cc.addr_1H, VecInit(index(c1s1.vaddr), index(c1s2.vaddr), index(c1s3.vaddr)))
        tagt.ena   := Mux1H(fsm.io.cc.addr_1H, VecInit(c1s1.rreq, c1s2.rreq, c1s3.rreq))
        tagt.dina  := tag(c1s3.paddr)
        tagt.wea   := fsm.io.cc.tagv_we(i)
    }
    data_tab.zipWithIndex.foreach{ case (datat, i) =>
        datat.clka  := clock
        datat.addra := Mux1H(fsm.io.cc.addr_1H, VecInit(index(c1s1.vaddr), index(c1s2.vaddr), index(c1s3.vaddr)))
        datat.ena   := Mux1H(fsm.io.cc.addr_1H, VecInit(c1s1.rreq, c1s2.rreq, c1s3.rreq))
        datat.dina  := rbuf.take(ic_line_bits)
        datat.wea   := fsm.io.cc.mem_we(i)
    }
    vld_tab.zipWithIndex.foreach{ case (vldt, i) =>
        vldt.raddr(0) := index(c1s2.vaddr)
        vldt.wen(0)   := fsm.io.cc.tagv_we(i)
        vldt.waddr(0) := index(c1s3.vaddr)
        vldt.wdata(0) := true.B
    }
    // rbuf
    when(io.l2.rrsp){
        rbuf := io.l2.rline ## rbuf(2*l1_line_bits-1, l1_line_bits)
    }
    // plus_reg
    when(io.l2.rreq && !io.l2.miss){
        plus_reg := plus_reg ^ 1.U
    }
    val l2_rreq         = fsm.io.l2.rreq & !ShiftRegister(fsm.io.l2.rreq, 1, false.B, !io.l2.miss)
    io.pp.rrsp          := c1s3.rreq && !miss_c1 && !io.pp.stall
    io.l2.rreq          := l2_rreq || ShiftRegister(l2_rreq, 1, false.B, true.B)
    io.l2.paddr         := (tag_index(c1s3.paddr) + plus_reg) ## Mux(plus_reg.asBool, 0.U, offset(c1s3.paddr))
    io.l2.uncache       := c1s3.uncache

    io.pp.miss          := miss_c1
    val rline           = Mux1H(fsm.io.cc.r1H, VecInit(Mux1H(c1s3.hit, c1s3.rdata), rbuf.take(ic_line_bits)))
    io.pp.rdata.zipWithIndex.foreach{ case (rdata, i) =>
        rdata := Mux(c1s3.uncache, 
            rbuf(i*32+31, i*32),
            (rline >> (32*i)).asTypeOf(Vec(l1_line/4, UInt(32.W)))(offset(c1s3.vaddr) >> 2)
        )
    }

}