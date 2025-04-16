import chisel3._
import chisel3.util._
import ZirconConfig.Issue._
import ZirconConfig.Decode._
import ZirconConfig.Commit._

class CPUDebugIO extends Bundle {
    val cmt = new ROBCommitIO
    val rf  = new RegfileDBGIO
    val fte = new FrontendDBGIO
    val bke = new BackendDBGIO
    val dsp = new ROBDebugIO
    val l2  = Output(Vec(2, new L2CacheDBG))
}


class CPUIO extends Bundle {
    val axi = new AXIIO
    val dbg = new CPUDebugIO
}

class CPU extends Module {
    val io = IO(new CPUIO)

    val fte = Module(new Frontend)
    val dsp = Module(new Dispatch)
    val bke = Module(new Backend)
    val rob = Module(new ReorderBuffer)

    val l2  = Module(new L2Cache)
    val arb = Module(new AXIArbiter)

    fte.io.dsp <> dsp.io.fte
    dsp.io.bke <> bke.io.dsp
    
    fte.io.cmt <> rob.io.fte
    bke.io.cmt <> rob.io.bke
    dsp.io.cmt <> rob.io.dsp
    rob.io.cmt.deq.foreach{ deq => deq.ready := DontCare }

    fte.io.mem.l2 <> l2.io.ic
    bke.io.mem.l2 <> l2.io.dc
    arb.io.l2  <> l2.io.mem
    arb.io.axi <> io.axi

    io.dbg.cmt <> rob.io.cmt
    io.dbg.rf  <> bke.io.dbg.rf

    // debug
    io.dbg.fte <> fte.io.dbg
    io.dbg.bke <> bke.io.dbg
    io.dbg.l2  <> l2.io.dbg
    io.dbg.dsp <> rob.io.dbg
}