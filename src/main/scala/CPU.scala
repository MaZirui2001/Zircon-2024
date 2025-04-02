import chisel3._
import chisel3.util._
import Zircon_Config.Issue._
import Zircon_Config.Decode._
import Zircon_Config.Commit._

class CPU_Debug_IO extends Bundle {
    val cmt = new ROB_Commit_IO
    val rf  = new Regfile_DBG_IO
}


class CPU_IO extends Bundle {
    val axi = new AXI_IO
    val dbg = new CPU_Debug_IO
}

class CPU extends Module {
    val io = IO(new CPU_IO)

    val fte = Module(new Frontend)
    val dsp = Module(new Dispatch)
    val bke = Module(new Backend)
    val rob = Module(new Reorder_Buffer)

    val l2  = Module(new L2Cache)
    val arb = Module(new AXI_Arbiter)

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
}