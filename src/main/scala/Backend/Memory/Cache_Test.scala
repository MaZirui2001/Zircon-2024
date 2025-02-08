import chisel3._
import chisel3.util._

class L2Cache_Test_IO extends Bundle {
    val ic = new L2_ICache_IO
    val dc = new L2_DCache_IO
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

class DCache_Test_IO extends Bundle {
    val d_pp = new D_Pipeline_IO
    val d_mmu = new D_MMU_IO
    val d_cmt = new D_Commit_IO
    val ic = new L2_ICache_IO
    val axi = new AXI_IO
}

class DCache_Test extends Module {
    val io = IO(new DCache_Test_IO)

    val dcache = Module(new DCache)
    val l2cache = Module(new L2Cache)
    val arb = Module(new AXI_Arbiter)
    dcache.io.pp <> io.d_pp
    dcache.io.mmu <> io.d_mmu
    dcache.io.cmt <> io.d_cmt
    dcache.io.l2 <> l2cache.io.dc
    
    l2cache.io.mem <> arb.io.l2
    arb.io.axi <> io.axi
    l2cache.io.ic <> io.ic
    
}