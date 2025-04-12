import chisel3._
import chisel3.util._
import ZirconConfig.Issue._

class ForwardIO extends Bundle {
    val instPkgWB  = Input(Vec(nis, new BackendPackage))
    // only forward arith pipeline
    val instPkgEX  = Input(Vec(4, new BackendPackage))
    val src1Fwd    = Vec(4, Decoupled(UInt(32.W)))
    val src2Fwd    = Vec(4, Decoupled(UInt(32.W)))
}

class Forward extends Module {
    val io = IO(new ForwardIO)

    io.src1Fwd.zipWithIndex.foreach{ case (fwd, i) =>
        val fwdEn = VecInit.tabulate(nis){ j =>
            io.instPkgEX(i).prj === io.instPkgWB(j).prd && io.instPkgWB(j).rdVld
        }
        fwd.valid := fwdEn.reduce(_ || _)
        fwd.bits  := Mux1H(fwdEn, io.instPkgWB.map(_.rfWdata))
    }

    io.src2Fwd.zipWithIndex.foreach{ case (fwd, i) =>
        val fwdEn = VecInit.tabulate(nis){ j =>
            io.instPkgEX(i).prk === io.instPkgWB(j).prd && io.instPkgWB(j).rdVld
        }
        fwd.valid := fwdEn.reduce(_ || _)
        fwd.bits  := Mux1H(fwdEn, io.instPkgWB.map(_.rfWdata))
    }
}
