import chisel3._
import chisel3.util._
import Zircon_Config.Decode._
import Zircon_Config.Issue._
import Zircon_Util._

class Dispatcher_IO extends Bundle {
    val fte_pkg = Vec(ndecode, Flipped(Decoupled(new Backend_Package)))
    val func    = Input(Vec(ndecode, UInt(niq.W)))
    val bke_pkg = Vec(niq, Vec(ndecode, Decoupled(new Backend_Package)))
}

class Dispatcher extends Module {
    val io = IO(new Dispatcher_IO)
    for(i <- 0 until niq){
        val port_map = VecInit.fill(ndecode)(0.U(ndecode.W))
        val port_map_trans = Transpose(port_map)
        var enq_ptr = 1.U(ndecode.W)
        for(j <- 0 until ndecode){
            port_map(j) := Mux(io.func(j)(i) && io.fte_pkg(j).valid, enq_ptr, 0.U)
            enq_ptr = Mux(io.func(j)(i) && io.fte_pkg(j).valid, ShiftAdd1(enq_ptr), enq_ptr)
        }
        io.bke_pkg(i).zipWithIndex.foreach{case (e, j) =>
            e.valid := port_map_trans(j).orR
            e.bits := Mux1H(port_map_trans(j), io.fte_pkg.map(_.bits))
        }
    }
    io.fte_pkg.foreach{ fte =>
        fte.ready := io.bke_pkg.map(_.map(_.ready)(0)).reduce(_ && _)
    }
}
