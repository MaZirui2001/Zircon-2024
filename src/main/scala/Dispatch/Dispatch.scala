import chisel3._
import chisel3.util._
import Zircon_Config.Decode._
import Zircon_Config.Issue._
import Zircon_Util._


class Dispatch_IO extends Bundle {
    val cmt     = Flipped(new ROB_Dispatch_IO)
    val fte     = Flipped(new Frontend_Dispatch_IO)
    val bke     = Flipped(new Backend_Dispatch_IO)
}

class Dispatch extends Module {
    val io = IO(new Dispatch_IO)

    val dsp = Module(new Dispatcher)
    val rboard = Module(new Ready_Board)

    // ready board
    rboard.io.pinfo     := io.fte.inst_pkg.map(_.bits.pinfo)
    rboard.io.wake_bus  := io.bke.wake_bus
    rboard.io.rply_bus  := io.bke.rply_bus
    rboard.io.flush     := io.cmt.flush

    val fte_pkg = VecInit.tabulate(ndecode){ i => Mux(io.cmt.enq(0).ready, 
        (new Backend_Package)(io.fte.inst_pkg(i).bits, io.cmt.enq_idx(i), rboard.io.prj_info(i), rboard.io.prk_info(i)), 
        0.U.asTypeOf(new Backend_Package))
    }

    // dispatcher
    dsp.io.fte_pkg.zipWithIndex.foreach{ case (fte, i) =>
        fte.valid := io.fte.inst_pkg(i).valid
        fte.bits := fte_pkg(i)
        io.fte.inst_pkg(i).ready := fte.ready && io.cmt.enq.map(_.ready).reduce(_ && _)
    }
    dsp.io.func.zipWithIndex.foreach{ case (func, i) =>
        func := io.fte.inst_pkg(i).bits.func
    }
    dsp.io.bke_pkg <> io.bke.inst_pkg
    io.cmt.enq.zipWithIndex.foreach{ case (enq, i) =>
        enq.valid := io.fte.inst_pkg(i).valid
        enq.bits := (new ROB_Frontend_Entry)(io.fte.inst_pkg(i).bits)
    }
}

