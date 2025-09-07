
import chisel3._
import chisel3.util._

class XilinxSinglePortRamReadFirst(RAMWIDTH: Int, RAMDEPTH: Int) extends BlackBox(Map( "RAMWIDTH" -> RAMWIDTH,
                                                                                                "RAMDEPTH" -> RAMDEPTH)) with HasBlackBoxInline {
    val io = IO(new Bundle {
        val addra = Input(UInt(log2Ceil(RAMDEPTH).W))
        val dina  = Input(UInt(RAMWIDTH.W))
        val clka  = Input(Clock())
        val wea   = Input(Bool())
        val ena   = Input(Bool())
        val douta = Output(UInt(RAMWIDTH.W))
    })
    val module = "XilinxSinglePortRamReadFirst.sv"
    setInline(module,
"""
| module XilinxSinglePortRamReadFirst #(
|   parameter RAMWIDTH = 18,                       // Specify RAM data width
|   parameter RAMDEPTH = 1024                     // Specify RAM depth (number of entries)
| ) (
|   input [clogb2(RAMDEPTH-1)-1:0] addra,  // Address bus, width determined from RAMDEPTH
|   input [RAMWIDTH-1:0] dina,           // RAM input data
|   input clka,                           // Clock
|   input wea,                            // Write enable
|   input ena,                            // RAM Enable, for additional power savings, disable port when not in use
|   output [RAMWIDTH-1:0] douta          // RAM output data
| );
|   (*ram_style="block"*)
|   reg [RAMWIDTH-1:0] BRAM [RAMDEPTH-1:0];
|   reg [$clog2(RAMDEPTH)-1:0] addrR;
|   reg [RAMWIDTH-1:0] ramData = {RAMWIDTH{1'b0}};
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
|       addrR <= addra;
|     end
|   assign douta = BRAM[addrR];
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

    