import chisel3._
import chisel3.util._
import ZirconConfig.Decode._
import ZirconConfig.Issue._
import ZirconUtil._


class DispatchIO extends Bundle {
    val cmt = Flipped(new ROBDispatchIO)
    val fte = Flipped(new FrontendDispatchIO)
    val bke = Flipped(new BackendDispatchIO)
}

class Dispatch extends Module {
    val io     = IO(new DispatchIO)

    val dsp    = Module(new Dispatcher)
    val rboard = Module(new ReadyBoard)

    // ready board
    rboard.io.pinfo   := io.fte.instPkg.map(_.bits.pinfo)
    rboard.io.wakeBus := io.bke.wakeBus
    rboard.io.rplyBus := io.bke.rplyBus
    rboard.io.flush   := io.cmt.flush

    val ftePkg = VecInit.tabulate(ndcd){ i =>  
        (new BackendPackage)(io.fte.instPkg(i).bits, io.cmt.enqIdx(i), rboard.io.prjInfo(i), rboard.io.prkInfo(i))
    }

    // dispatcher
    dsp.io.ftePkg.zipWithIndex.foreach{ case (d, i) =>
        d.valid := io.fte.instPkg(i).valid && io.cmt.enq(0).ready
        d.bits  := ftePkg(i)
        io.fte.instPkg(i).ready := d.ready && io.cmt.enq.map(_.ready).reduce(_ && _)
    }
    dsp.io.func.zipWithIndex.foreach{ case (func, i) =>
        func := io.fte.instPkg(i).bits.func
    }
    dsp.io.bkePkg <> io.bke.instPkg
    io.cmt.enq.zipWithIndex.foreach{ case (enq, i) =>
        enq.valid := io.fte.instPkg(i).valid && dsp.io.ftePkg(i).ready
        enq.bits  := (new ROBFrontendEntry)(io.fte.instPkg(i).bits)
    }
}

