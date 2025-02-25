import chisel3._
import chisel3.util._
import CPU_Config.RegisterFile._
import CPU_Config.Commit._
import CPU_Config.Decode._
import Zircon_Util._

class Free_List_Rename_IO extends Bundle{
    val deq         = Vec(ndecode, Decoupled(UInt(wpreg.W)))
}

class Free_List_Commit_IO extends Bundle{
    val enq         = Vec(ncommit, Flipped(Decoupled(UInt(wpreg.W))))
    val flush       = Input(Bool())
}

class Free_List_IO extends Bundle{
    val rnm = new Free_List_Rename_IO
    val cmt = new Free_List_Commit_IO
}

class PReg_Free_List extends Module{
    val io = IO(new Free_List_IO)

    val flst = Module(new Cluster_Index_FIFO(UInt(wpreg.W), npreg, ncommit, ndecode, 0, 0, true, Some(Seq.tabulate(npreg)(i => (i+1).U(wpreg.W)))))
    /* calculate the port map
        port_map(i) means io port i is connected to the port_map(i)th enq port
        it traverse is the enq port mapped to the io port
    */
    val port_map_rnm = VecInit.fill(ndecode)(0.U(ndecode.W))
    val port_map_trav_rnm = VecInit.fill(ndecode)(0.U(ndecode.W))
    var valid_ptr_rnm = 1.U(ndecode.W)
    for(i <- 0 until ndecode) {
        port_map_rnm(i) := Mux(io.rnm.deq(i).ready, valid_ptr_rnm, 0.U)
        valid_ptr_rnm = Mux(io.rnm.deq(i).ready, shift_add_1(valid_ptr_rnm), valid_ptr_rnm)
    }
    for(i <- 0 until ndecode) {
        port_map_trav_rnm(i) := VecInit(port_map_rnm.map(_(i))).asUInt
    }
    // rename stage
    flst.io.deq.zipWithIndex.foreach{ case (d, i) =>
        d.ready := port_map_trav_rnm(i).orR
    }
    io.rnm.deq.zipWithIndex.foreach{ case (d, i) =>
        d.valid := flst.io.deq(i).valid
        d.bits := Mux1H(port_map_rnm(i), flst.io.deq.map(_.bits))
    }
    // commit stage
    val port_map_cmt = VecInit.fill(ncommit)(0.U(ncommit.W))
    val port_map_trav_cmt = VecInit.fill(ncommit)(0.U(ncommit.W))
    var valid_ptr_cmt = 1.U(ncommit.W)
    for(i <- 0 until ncommit) {
        port_map_cmt(i) := Mux(io.cmt.enq(i).valid, valid_ptr_cmt, 0.U)
        valid_ptr_cmt = Mux(io.cmt.enq(i).valid, shift_add_1(valid_ptr_cmt), valid_ptr_cmt)
    }
    for(i <- 0 until ncommit) {
        port_map_trav_cmt(i) := VecInit(port_map_cmt.map(_(i))).asUInt
    }
    flst.io.enq.zipWithIndex.foreach{ case (e, i) =>
        e.valid := port_map_trav_cmt(i).orR
        e.bits := Mux1H(port_map_trav_cmt(i), io.cmt.enq.map(_.bits))
    }
    io.cmt.enq.foreach(_.ready := flst.io.enq.map(_.ready).reduce(_ && _))
    flst.io.flush := io.cmt.flush
}
    