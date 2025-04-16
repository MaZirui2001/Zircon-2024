import chisel3._
import chisel3.util._
import ZirconConfig.Decode._
import ZirconConfig.Issue._
import ZirconUtil._

class DispatcherIO extends Bundle {
    val ftePkg = Vec(ndcd, Flipped(Decoupled(new BackendPackage)))
    val func   = Input(Vec(ndcd, UInt(niq.W)))
    val bkePkg = Vec(niq, Vec(ndcd, Decoupled(new BackendPackage)))
}

class Dispatcher extends Module {
    val io = IO(new DispatcherIO)
    val bkePkgAllReady = io.bkePkg.map(_.map(_.ready)(0)).reduce(_ && _)
    for(i <- 0 until niq){
        val portMap      = VecInit.fill(ndcd)(0.U(ndcd.W))
        val portMapTrans = Transpose(portMap)
        var enqPtr       = 1.U(ndcd.W)
        for(j <- 0 until ndcd){
            portMap(j) := Mux(io.func(j)(i) && io.ftePkg(j).valid, enqPtr, 0.U)
            enqPtr     = Mux(io.func(j)(i) && io.ftePkg(j).valid, ShiftAdd1(enqPtr), enqPtr)
        }
        io.bkePkg(i).zipWithIndex.foreach{case (e, j) =>
            e.valid := portMapTrans(j).orR && bkePkgAllReady
            e.bits  := Mux1H(portMapTrans(j), io.ftePkg.map(_.bits))
        }
    }
    io.ftePkg.foreach{ fte =>
        fte.ready := bkePkgAllReady
    }
}
