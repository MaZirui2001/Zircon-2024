import chisel3._
import chisel3.util._

class XilinxTrueDualPortReadFirstByteWrite1ClockRam(NBCOL: Int, COLWIDTH: Int, RAMDEPTH: Int) extends BlackBox(Map( "NBCOL" -> NBCOL, "COLWIDTH" -> COLWIDTH, "RAMDEPTH" -> RAMDEPTH)) with HasBlackBoxInline {
    val io = IO(new Bundle {
        val addra = Input(UInt(log2Ceil(RAMDEPTH).W))
        val addrb = Input(UInt(log2Ceil(RAMDEPTH).W))
        val dina  = Input(UInt((NBCOL*COLWIDTH).W))
        val dinb  = Input(UInt((NBCOL*COLWIDTH).W))
        val clka  = Input(Clock())
        val wea   = Input(UInt(NBCOL.W))
        val web   = Input(UInt(NBCOL.W))
        val ena   = Input(Bool())
        val enb   = Input(Bool())
        val douta = Output(UInt((NBCOL*COLWIDTH).W))
        val doutb = Output(UInt((NBCOL*COLWIDTH).W))
    })
    val module = "XilinxTrueDualPortReadFirstByteWrite1ClockRam.sv"
    setInline(module,
"""
|module XilinxTrueDualPortReadFirstByteWrite1ClockRam #(
|  parameter NBCOL = 4,                           // Specify number of columns (number of bytes)
|  parameter COLWIDTH = 9,                        // Specify column width (byte width, typically 8 or 9)
|  parameter RAMDEPTH = 1024                     // Specify RAM depth (number of entries)
|) (
|  input [clogb2(RAMDEPTH-1)-1:0] addra,   // Port A address bus, width determined from RAMDEPTH
|  input [clogb2(RAMDEPTH-1)-1:0] addrb,   // Port B address bus, width determined from RAMDEPTH
|  input [(NBCOL*COLWIDTH)-1:0] dina,   // Port A RAM input data
|  input [(NBCOL*COLWIDTH)-1:0] dinb,   // Port B RAM input data
|  input clka,                            // Clock
|  input [NBCOL-1:0] wea,                // Port A write enable
|  input [NBCOL-1:0] web,                // Port B write enable
|  input ena,                             // Port A RAM Enable, for additional power savings, disable port when not in use
|  input enb,                             // Port B RAM Enable, for additional power savings, disable port when not in use
|  output [(NBCOL*COLWIDTH)-1:0] douta, // Port A RAM output data
|  output [(NBCOL*COLWIDTH)-1:0] doutb  // Port B RAM output data
|);
|  (*ram_style="block"*)
|  reg [(NBCOL*COLWIDTH)-1:0] BRAM [RAMDEPTH-1:0];
|  reg [$clog2(RAMDEPTH)-1:0] addrRA;
|  reg [$clog2(RAMDEPTH)-1:0] addrRB;
|
|  // The following code either initializes the memory values to a specified file or to all zeros to match hardware
|  generate
|      integer ramIndex;
|      initial
|        for (ramIndex = 0; ramIndex < RAMDEPTH; ramIndex = ramIndex + 1)
|          BRAM[ramIndex] = {(NBCOL*COLWIDTH){1'b0}};
|  endgenerate
|
|  always @(posedge clka)
|    if (ena) begin
|      addrRA <= addra;
|    end
|
|  always @(posedge clka)
|    if (enb) begin
|      addrRB <= addrb;
|    end
|
|  generate
|  genvar i;
|     for (i = 0; i < NBCOL; i = i+1) begin: byteWrite
|       always @(posedge clka)
|         if (ena)
|           if (wea[i])
|             BRAM[addra][(i+1)*COLWIDTH-1:i*COLWIDTH] <= dina[(i+1)*COLWIDTH-1:i*COLWIDTH];
|       always @(posedge clka)
|         if (enb)
|           if (web[i])
|             BRAM[addrb][(i+1)*COLWIDTH-1:i*COLWIDTH] <= dinb[(i+1)*COLWIDTH-1:i*COLWIDTH];
|end
|  endgenerate
|
|  generate
|      // The following is a 1 clock cycle read latency at the cost of a longer clock-to-out timing
|       assign douta = BRAM[addrRA];
|       assign doutb = BRAM[addrRB];
|  endgenerate
|
|  //  The following function calculates the address width based on specified RAM depth
|  function integer clogb2;
|    input integer depth;
|      for (clogb2=0; depth>0; clogb2=clogb2+1)
|        depth = depth >> 1;
|  endfunction
|
|endmodule
""".stripMargin)
}            