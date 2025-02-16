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

class Cache_Test_IO extends Bundle {
    val i_pp = new I_Pipeline_IO
    val i_mmu = new I_MMU_IO
    val d_pp = new D_Pipeline_IO
    val d_mmu = new D_MMU_IO
    val d_cmt = new D_Commit_IO

    val axi = new AXI_IO
}

class Cache_Test extends Module {
    val io = IO(new Cache_Test_IO)

    val icache = Module(new ICache)
    val dcache = Module(new DCache)
    val l2cache = Module(new L2Cache)
    val arb = Module(new AXI_Arbiter)

    icache.io.pp <> io.i_pp
    icache.io.mmu <> io.i_mmu
    icache.io.l2 <> l2cache.io.ic

    dcache.io.pp <> io.d_pp
    dcache.io.mmu <> io.d_mmu
    dcache.io.cmt <> io.d_cmt
    dcache.io.l2 <> l2cache.io.dc

    
    l2cache.io.mem <> arb.io.l2
    arb.io.axi <> io.axi
    
}