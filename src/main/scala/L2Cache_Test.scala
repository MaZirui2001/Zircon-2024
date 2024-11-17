import chisel3._
import chisel3.util._

class L2Cache_Test_IO extends Bundle {
    val ic = new ICache_IO
    val dc = new DCache_IO
    val axi = new AXI_IO
}
class L2Cache_Test extends Module{
    val io = IO(new L2Cache_Test_IO)

    val l2cache = Module(new L2Cache)
    val arb = Module(new AXI_Arbiter)

    l2cache.io.ic <> io.ic
    l2cache.io.dc <> io.dc
    l2cache.io.mem <> arb.io.l2
    arb.io.axi <> io.axi
}