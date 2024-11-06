import chisel3._
import chisel3.util._
import CPU_Config.Cache._
import Zircon_Util._

class ICache_IO extends Bundle{
    val rreq        = Input(Bool())
    val rrsp        = Output(Bool())
    val paddr       = Input(UInt(32.W))
    val uc_in      = Input(Bool())
    val rline       = Output(UInt(ic_line_bits.W))
    val ruc_out     = Output(Bool())
}

class DCache_IO extends Bundle {
    // replace port
    val rreq        = Input(Bool())
    val is_wrt      = Input(Bool())
    val uc_in       = Input(Bool())
    val rtype       = Input(UInt(2.W))
    val wdata       = Input(UInt(32.W))
    val rrsp        = Output(Bool())
    val rline       = Output(UInt(dc_line_bits.W))
    val uc_out      = Output(Bool())
    val paddr_out   = Output(UInt(32.W))

    // write through port
    val tvld        = Input(Bool())
    val trdy        = Output(Bool())
    val ttype       = Input(UInt(2.W))
    val tdata       = Input(UInt(32.W))

    // shared port
    val paddr_in    = Input(UInt(32.W))
    
}

class Mem_IO extends Bundle {
    val rvld        = Output(Bool())
    val rrdy        = Input(Bool())
    val rlast       = Input(Bool())
    val raddr       = Output(UInt(32.W))
    val rdata       = Input(UInt(32.W))
    val rlen        = Output(UInt(8.W))
    val rsize       = Output(UInt(2.W))

    val wvld        = Output(Bool())
    val wrdy        = Input(Bool())
    val wlast       = Output(Bool())
    val waddr       = Output(UInt(32.W))
    val wdata       = Output(UInt(32.W))
    val wlen        = Output(UInt(8.W))
    val wsize       = Output(UInt(2.W))
}
class Channel1_Stage1_Signal extends Bundle {
    val tvld        = Bool()
    val taddr       = UInt(32.W)
    val tdata       = UInt(32.W)
    val ttype       = UInt(2.W)
}
class Channel1_Stage2_Signal extends Bundle {
    val tvld        = Bool()
    val taddr       = UInt(32.W)
    val tdata       = UInt(l2_line_bits.W)
    val twe         = UInt(l2_line.W)
    val thit        = UInt(l2_way.W)
}
class Channel2_Stage1_Signal extends Bundle {
    val rreq        = Bool()
    val rtype       = UInt(2.W)
    val paddr       = UInt(32.W)
    val uc_in       = Bool()
    val is_wrt      = Bool()
    val wdata       = UInt(32.W)
}
class Channel2_Stage2_Signal extends Bundle {
    val rreq        = Bool()
    val paddr       = UInt(32.W)
    val uc_in       = Bool()
    val is_wrt      = Bool()
    val wdata       = UInt(l2_line_bits.W)
    val we          = UInt(l2_line.W)
    val rhit        = UInt(l2_way.W)
    val rtag        = Vec(l2_way, UInt(l2_tag.W))
}
class L2Cache_IO extends Bundle {
    val ic      = new ICache_IO
    val dc      = new DCache_IO
    val mem     = new Mem_IO
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
    val tag_tab     = VecInit.fill(l2_way)(new xilinx_true_dual_port_read_first_1_clock_ram(l2_tag, l2_index_num).io)
    val vld_tab     = RegInit(VecInit.fill(l2_way)(VecInit.fill(l2_index_num)(false.B)))
    // data
    val data_tab    = VecInit.fill(l2_way)(new xilinx_true_dual_port_read_first_byte_write_1_clock_ram(l2_line, 8, l2_index_num).io)
    // dirty
    val dirty_tab   = RegInit(VecInit.fill(l2_way)(VecInit.fill(l2_index_num)(false.B)))
    // lru
    val lru_tab     = RegInit(VecInit.fill(l2_index_num)(0.U(2.W)))

    /* 
    channel 1: dcache write through
        stage 1: receive the write through request
        stage 2: search the tag to determine which line to write
        stage 3: write the data to the line
    */

    /* stage 1: receive the write through request */
    val c1s1_in     = Cat(io.dc.tvld, io.dc.paddr_in, io.dc.tdata, io.dc.ttype).asTypeOf(new Channel1_Stage1_Signal)
    io.dc.trdy      := true.B
    // Segreg1-2
    val c1s2        = ShiftRegister(c1s1_in, 1, 0.U.asTypeOf(new Channel1_Stage1_Signal), true.B)
    /* stage 2: search the tag to determine which line to write */
    tag_tab.zipWithIndex.foreach{ case (tagt, i) =>
        tagt.addra  := io.dc.paddr_in(c1s2.taddr(l2_index+l2_offset-1, l2_offset))
        tagt.ena    := c1s2.tvld
        tagt.dina   := DontCare
        tagt.wea    := false.B
    }
    val thit_s2     = VecInit(tag_tab.map(_.douta === c1s2.taddr(31, l2_index+l2_offset))).asUInt
    assert(PopCount(thit_s2) <= 1.U && PopCount(thit_s2) >= 0.U, "dcache write through hit more than one line or no line")
    
    val twe_s2      = (mtype_decode(c1s2.ttype, l2_line) << c1s2.taddr.take(l2_offset)).take(l2_line)
    val tdata_s2    = (c1s2.tdata << (c1s2.taddr(l2_offset-1, 0) << 3)).take(l2_line_bits)
    val c1s3_in     = Cat(c1s2.tvld, c1s2.taddr, tdata_s2, twe_s2, thit_s2).asTypeOf(new Channel1_Stage2_Signal)
    // Segreg2-3
    val c1s3        = ShiftRegister(c1s3_in, 1, 0.U.asTypeOf(new Channel1_Stage2_Signal), true.B)
    /* stage 3: write the data to the line */
    data_tab.zipWithIndex.foreach{ case (datat, i) =>
        datat.addra  := c1s3.taddr(l2_index+l2_offset-1, l2_offset)
        datat.dina   := c1s3.tdata
        datat.wea    := Fill(l2_line, c1s3.thit(i)) & c1s3.twe
        datat.ena    := c1s3.tvld
    }

    /*
    channel 2: icache and dcache miss
        stage 1: receive the request, and arbitrate the request
        stage 2: search the tag to determine hit or miss, as well as read the data from the line
        stage 3: read the data from the line
    */

    val miss        = RegInit(false.B)
    val hit         = RegInit(0.U(l2_way.W))
    /* stage 1: receive the request, and arbitrate the request */
    // arbiter: dcache has higher priority
    val from_dc     = io.dc.rreq
    val c2s1_in     = Mux(from_dc, Cat(io.dc.rreq, io.dc.rtype, io.dc.paddr_in, io.dc.uc_in,  io.dc.is_wrt, io.dc.wdata), 
                                   Cat(io.ic.rreq, 0xf.U(4.W),  io.ic.paddr,    io.ic.uc_in,  false.B,      io.dc.wdata)).asTypeOf(new Channel2_Stage1_Signal)
    // Segreg1-2
    val c2s2        = ShiftRegister(c2s1_in, 1, 0.U.asTypeOf(new Channel2_Stage1_Signal), !miss)
    /* stage 2: search the tag to determine hit or miss, as well as read the data from the line */
    tag_tab.zipWithIndex.foreach{ case (tagt, i) =>
        tagt.addrb  := c2s2.paddr(l2_index+l2_offset-1, l2_offset) // TODO
        tagt.enb    := c2s2.rreq // TODO
        tagt.dinb   := DontCare // TODO
        tagt.web    := false.B // TODO
    }

    val rhit_s2     = Mux(c2s2.uc_in, 0.U, VecInit(tag_tab.map(_.doutb === c2s2.paddr(31, l2_index+l2_offset))).asUInt)
    assert(PopCount(rhit_s2) <= 1.U, "icache and dcache miss hit more than one line")

    val we_s2       = (mtype_decode(c2s2.rtype, l2_line) << Mux(c2s2.uc_in, 0.U, c2s2.paddr.take(l2_offset))).take(l2_line)
    val wdata_s2    = Mux(c2s2.uc_in, 0.U((l2_line_bits-32).W) ## c2s2.wdata, 
                                      (c2s2.wdata << (c2s2.paddr(l2_offset-1, 0) << 3)).take(l2_line_bits))
    // miss update logic
    miss            := Mux(miss, miss, Mux(c2s2.uc_in, true.B, rhit_s2 === 0.U)) // TODO
    
    val c2s3_in     = Cat(c2s2.rreq, c2s2.paddr, c2s2.uc_in, c2s2.is_wrt, wdata_s2, rhit_s2, VecInit(tag_tab.map(_.doutb)).asUInt).asTypeOf(new Channel2_Stage2_Signal)
    // Segreg2-3
    val c2s3        = ShiftRegister(c2s3_in, 1, 0.U.asTypeOf(new Channel2_Stage2_Signal), !miss)

    
}