import chisel3._
import chisel3.util._
import CPU_Config.RegisterFile._
import CPU_Config.Commit._
import CPU_Config.Decode._
import Zircon_Util._

class Free_List_Frontend_IO extends Bundle{
    val deq         = Vec(ndecode, Decoupled(UInt(wpreg.W)))
}

class Free_List_Commit_IO extends Bundle{
    val enq         = Vec(ncommit, Flipped(Decoupled(UInt(wpreg.W))))
    val flush       = Input(Bool())
}

class Free_List_Diff_IO extends Bundle{
    val free_list   = Output(Vec(npreg, UInt(wpreg.W)))
}

class Free_List_IO extends Bundle{
    val fte = new Free_List_Frontend_IO
    val cmt = new Free_List_Commit_IO
    val dif = new Free_List_Diff_IO
}

class PReg_Free_List extends Module{
    val io = IO(new Free_List_IO)

    val flst = Module(new Cluster_Index_FIFO(UInt(wpreg.W), npreg, ncommit, ndecode, 0, 0, true, Some(Seq.tabulate(npreg)(i => (i+1).U(wpreg.W)))))
    /* calculate the port map
        port_map(i) means io port i is connected to the port_map(i)th enq port
        it traverse is the enq port mapped to the io port
    */
    val port_map_fte = VecInit.fill(ndecode)(0.U(ndecode.W))
    val port_map_trans_fte = transpose(port_map_fte)
    var valid_ptr_fte = 1.U(ndecode.W)
    for(i <- 0 until ndecode) {
        port_map_fte(i) := Mux(io.fte.deq(i).ready, valid_ptr_fte, 0.U)
        valid_ptr_fte = Mux(io.fte.deq(i).ready, shift_add_1(valid_ptr_fte), valid_ptr_fte)
    }
    // rename stage
    flst.io.deq.zipWithIndex.foreach{ case (d, i) =>
        d.ready := port_map_trans_fte(i).orR
    }
    io.fte.deq.zipWithIndex.foreach{ case (d, i) =>
        d.valid := flst.io.deq(i).valid
        d.bits := Mux1H(port_map_fte(i), flst.io.deq.map(_.bits))
    }
    // commit stage
    val port_map_cmt = VecInit.fill(ncommit)(0.U(ncommit.W))
    val port_map_trans_cmt = transpose(port_map_cmt)
    var valid_ptr_cmt = 1.U(ncommit.W)
    for(i <- 0 until ncommit) {
        // pprd =/= 0: for the first several instructions, the pprd is not valid
        port_map_cmt(i) := Mux(io.cmt.enq(i).valid && io.cmt.enq(i).bits =/= 0.U, valid_ptr_cmt, 0.U)
        valid_ptr_cmt = Mux(io.cmt.enq(i).valid && io.cmt.enq(i).bits =/= 0.U, shift_add_1(valid_ptr_cmt), valid_ptr_cmt)
    }
    flst.io.enq.zipWithIndex.foreach{ case (e, i) =>
        e.valid := port_map_trans_cmt(i).orR
        e.bits := Mux1H(port_map_trans_cmt(i), io.cmt.enq.map(_.bits))
    }
    io.cmt.enq.foreach(_.ready := DontCare)
    flst.io.flush := io.cmt.flush
    io.dif.free_list := flst.io.dbg_FIFO
}
    