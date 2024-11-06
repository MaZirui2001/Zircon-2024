import chisel3._
import chisel3.util._

class xilinx_true_dual_port_read_first_1_clock_ram(RAM_WIDTH: Int, RAM_DEPTH: Int) extends BlackBox(Map( "RAM_WIDTH" -> RAM_WIDTH,
                                                                                                "RAM_DEPTH" -> RAM_DEPTH)) with HasBlackBoxInline {
    val io = IO(new Bundle {
        val addra = Input(UInt(log2Ceil(RAM_DEPTH).W))
        val addrb = Input(UInt(log2Ceil(RAM_DEPTH).W))
        val dina = Input(UInt(RAM_WIDTH.W))
        val dinb = Input(UInt(RAM_WIDTH.W))
        val clka = Input(Clock())
        val wea = Input(Bool())
        val web = Input(Bool())
        val ena = Input(Bool())
        val enb = Input(Bool())
        val douta = Output(UInt(RAM_WIDTH.W))
        val doutb = Output(UInt(RAM_WIDTH.W))
    })
    val module = "xilinx_true_dual_port_read_first_1_clock_ram.sv"
    setInline(module,
"""
| module xilinx_true_dual_port_read_first_1_clock_ram #(
|   parameter RAM_WIDTH = 18,                       // Specify RAM data width
|   parameter RAM_DEPTH = 1024,                     // Specify RAM depth (number of entries)
| ) (
|   input [clogb2(RAM_DEPTH-1)-1:0] addra,  // Port A address bus, width determined from RAM_DEPTH
|   input [clogb2(RAM_DEPTH-1)-1:0] addrb,  // Port B address bus, width determined from RAM_DEPTH
|   input [RAM_WIDTH-1:0] dina,           // Port A RAM input data
|   input [RAM_WIDTH-1:0] dinb,           // Port B RAM input data
|   input clka,                           // Clock
|   input wea,                            // Port A write enable
|   input web,                            // Port B write enable
|   input ena,                            // Port A RAM Enable, for additional power savings, disable port when not in use
|   input enb,                            // Port B RAM Enable, for additional power savings, disable port when not in use
|   output [RAM_WIDTH-1:0] douta,         // Port A RAM output data
|   output [RAM_WIDTH-1:0] doutb          // Port B RAM output data
| );
| 
|   reg [RAM_WIDTH-1:0] BRAM [RAM_DEPTH-1:0];
|   reg [RAM_WIDTH-1:0] ram_data_a = {RAM_WIDTH{1'b0}};
|   reg [RAM_WIDTH-1:0] ram_data_b = {RAM_WIDTH{1'b0}};
| 
|   // The following code either initializes the memory values to a specified file or to all zeros to match hardware
|   generate
|       integer ram_index;
|       initial
|         for (ram_index = 0; ram_index < RAM_DEPTH; ram_index = ram_index + 1)
|           BRAM[ram_index] = {RAM_WIDTH{1'b0}};
|   endgenerate
| 
|   always @(posedge clka)
|     if (ena) begin
|       if (wea)
|         BRAM[addra] <= dina;
|       ram_data_a <= BRAM[addra];
|     end
| 
|   always @(posedge clka)
|     if (enb) begin
|       if (web)
|         BRAM[addrb] <= dinb;
|       ram_data_b <= BRAM[addrb];
|     end
| 
|   generate
|         // The following is a 1 clock cycle read latency at the cost of a longer clock-to-out timing
|        assign douta = ram_data_a;
|        assign doutb = ram_data_b;
| 
|   endgenerate
| 
|   //  The following function calculates the address width based on specified RAM depth
|   function integer clogb2;
|     input integer depth;
|       for (clogb2=0; depth>0; clogb2=clogb2+1)
|         depth = depth >> 1;
|   endfunction
| 
| endmodule
|	
""".stripMargin)
}            

    