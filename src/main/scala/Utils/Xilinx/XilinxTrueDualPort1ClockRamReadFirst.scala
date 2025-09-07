import chisel3._
import chisel3.util._

class XilinxTrueDualPortReadFirst1ClockRam(RAMWIDTH: Int, RAMDEPTH: Int) extends BlackBox(Map( "RAMWIDTH" -> RAMWIDTH,
                                                                                                "RAMDEPTH" -> RAMDEPTH)) with HasBlackBoxInline {
    val io = IO(new Bundle {
        val addra = Input(UInt(log2Ceil(RAMDEPTH).W))
        val addrb = Input(UInt(log2Ceil(RAMDEPTH).W))
        val dina  = Input(UInt(RAMWIDTH.W))
        val dinb  = Input(UInt(RAMWIDTH.W))
        val clka  = Input(Clock())
        val wea   = Input(Bool())
        val web   = Input(Bool())
        val ena   = Input(Bool())
        val enb   = Input(Bool())
        val douta = Output(UInt(RAMWIDTH.W))
        val doutb = Output(UInt(RAMWIDTH.W))
    })
    val module = "XilinxTrueDualPortReadFirst1ClockRam.sv"
    setInline(module,
"""
| module XilinxTrueDualPortReadFirst1ClockRam #(
|   parameter RAMWIDTH = 18,                       // Specify RAM data width
|   parameter RAMDEPTH = 1024                     // Specify RAM depth (number of entries)
| ) (
|   input [clogb2(RAMDEPTH-1)-1:0] addra,  // Port A address bus, width determined from RAMDEPTH
|   input [clogb2(RAMDEPTH-1)-1:0] addrb,  // Port B address bus, width determined from RAMDEPTH
|   input [RAMWIDTH-1:0] dina,           // Port A RAM input data
|   input [RAMWIDTH-1:0] dinb,           // Port B RAM input data
|   input clka,                           // Clock
|   input wea,                            // Port A write enable
|   input web,                            // Port B write enable
|   input ena,                            // Port A RAM Enable, for additional power savings, disable port when not in use
|   input enb,                            // Port B RAM Enable, for additional power savings, disable port when not in use
|   output [RAMWIDTH-1:0] douta,         // Port A RAM output data
|   output [RAMWIDTH-1:0] doutb          // Port B RAM output data
| );
|   (*ram_style="block"*)
|   reg [RAMWIDTH-1:0] BRAM [RAMDEPTH-1:0];
|   reg [$clog2(RAMDEPTH)-1:0] addrRA;
|   reg [$clog2(RAMDEPTH)-1:0] addrRB;
| 
|   // The following code either initializes the memory values to a specified file or to all zeros to match hardware
|   generate
|       integer ramIndex;
|       initial
|         for (ramIndex = 0; ramIndex < RAMDEPTH; ramIndex = ramIndex + 1)
|           BRAM[ramIndex] = {RAMWIDTH{1'b0}};
|   endgenerate
| 
|   always @(posedge clka)
|     if (ena) begin
|       if (wea)
|         BRAM[addra] <= dina;
|       addrRA <= addra;
|     end
| 
|   always @(posedge clka)
|     if (enb) begin
|       if (web)
|         BRAM[addrb] <= dinb;
|       addrRB <= addrb;
|     end
| 
|   generate
|         // The following is a 1 clock cycle read latency at the cost of a longer clock-to-out timing
|        assign douta = BRAM[addrRA];
|        assign doutb = BRAM[addrRB];
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

    