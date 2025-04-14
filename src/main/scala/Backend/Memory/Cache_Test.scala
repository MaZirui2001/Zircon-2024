import chisel3._
import chisel3.util._

class L2CacheTestIO extends Bundle {
    val ic = new L2ICacheIO
    val dc = new L2DCacheIO
    val axi = new AXIIO
}
class L2CacheTest extends Module{
    val io = IO(new L2CacheTestIO)

    val l2cache = Module(new L2Cache)
    val arb = Module(new AXIArbiter)

    l2cache.io.ic <> io.ic
    l2cache.io.dc <> io.dc
    l2cache.io.mem <> arb.io.l2
    arb.io.axi <> io.axi
}

class CacheTestIO extends Bundle {
    val iPP = new IPipelineIO
    val iMmu = new IMMUIO
    val dPP = new DPipelineIO
    val dMmu = new DMMUIO
    val dCmt = new DCommitIO

    val axi = new AXIIO
}

class CacheTest extends Module {
    val io = IO(new CacheTestIO)

    val icache = Module(new ICache)
    val dcache = Module(new DCache)
    val l2cache = Module(new L2Cache)
    val arb = Module(new AXIArbiter)

    icache.io.pp <> io.iPP
    icache.io.mmu <> io.iMmu
    icache.io.l2 <> l2cache.io.ic

    dcache.io.pp <> io.dPP
    dcache.io.mmu <> io.dMmu
    dcache.io.cmt <> io.dCmt
    dcache.io.l2 <> l2cache.io.dc

    
    l2cache.io.mem <> arb.io.l2
    arb.io.axi <> io.axi
    
}