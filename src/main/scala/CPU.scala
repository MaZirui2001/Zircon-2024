import chisel3._
import chisel3.util._
import ZirconConfig.Issue._
import ZirconConfig.Decode._
import ZirconConfig.Commit._

class CPUDebugIO extends Bundle {
    val cmt = new CommitDBGIO
    val rf  = new RegfileDBGIO
    val fte = new FrontendDBGIO
    val bke = new BackendDBGIO
    val l2  = Output(Vec(2, new L2CacheDBG))
}


class CPUIO(sim: Boolean) extends Bundle {
    val axi = new AXIIO
    val dbg = if(sim) Some(new CPUDebugIO) else None
}

class CPU(sim: Boolean = false) extends Module {
    val io = IO(new CPUIO(sim))

    val fte = Module(new Frontend)
    val dsp = Module(new Dispatch)
    val bke = Module(new Backend)
    val cmt = Module(new Commit)

    val l2  = Module(new L2Cache)
    val arb = Module(new AXIArbiter)

    fte.io.dsp <> dsp.io.fte
    dsp.io.bke <> bke.io.dsp
    // fte.io.bke <> bke.io.fte
    
    fte.io.cmt <> cmt.io.fte
    bke.io.cmt <> cmt.io.bke
    dsp.io.cmt <> cmt.io.dsp

    fte.io.mem.l2 <> l2.io.ic
    bke.io.mem.l2 <> l2.io.dc
    arb.io.l2  <> l2.io.mem
    arb.io.axi <> io.axi


    cmt.io.dbg.robDeq.flush := false.B
    cmt.io.dbg.bdbDeq.flush := false.B
    cmt.io.dbg.robDeq.deq.foreach{ case(deq) => deq.ready := DontCare }
    cmt.io.dbg.bdbDeq.deq.ready := DontCare
    // debug
    if(sim){
        io.dbg.get.cmt <> cmt.io.dbg
        io.dbg.get.rf  <> bke.io.dbg.rf
        io.dbg.get.fte <> fte.io.dbg
        io.dbg.get.bke <> bke.io.dbg
        io.dbg.get.l2  <> l2.io.dbg
    }
}