import chisel3._
import chisel3.util._
import Zircon_Config.RegisterFile._
import Zircon_Config.Commit._
import Zircon_Config.Decode._
import Zircon_Util._

class Free_List_Frontend_IO extends Bundle{
    val deq         = Vec(ndecode, Decoupled(UInt(wpreg.W)))
}

class Free_List_Commit_IO extends Bundle{
    val enq         = Vec(ncommit, Flipped(Decoupled(UInt(wpreg.W))))
    val flush       = Input(Bool())
}

class Free_List_Diff_IO extends Bundle{
    val free_list   = Output(Vec(npreg-32, UInt(wpreg.W)))
}

class Free_List_IO extends Bundle{
    val fte = new Free_List_Frontend_IO
    val cmt = new Free_List_Commit_IO
    val dif = new Free_List_Diff_IO
}

class PReg_Free_List extends Module{
    val io = IO(new Free_List_IO)

    val flst = Module(new Cluster_Index_FIFO(UInt(wpreg.W), npreg-32, ncommit, ndecode, 0, 0, true, Some(Seq.tabulate(npreg-32)(i => (i+32).U(wpreg.W)))))
    /* calculate the port map
        port_map(i) means io port i is connected to the port_map(i)th enq port
        it traverse is the enq port mapped to the io port
    */
    val port_map_fte = VecInit.fill(ndecode)(0.U(ndecode.W))
    val port_map_trans_fte = Transpose(port_map_fte)
    var valid_ptr_fte = 1.U(ndecode.W)
    for(i <- 0 until ndecode) {
        port_map_fte(i) := Mux(io.fte.deq(i).ready, valid_ptr_fte, 0.U)
        valid_ptr_fte = Mux(io.fte.deq(i).ready, ShiftAdd1(valid_ptr_fte), valid_ptr_fte)
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
    val port_map_enq = VecInit.fill(ncommit)(0.U(ncommit.W))
    val port_map_trans_enq = Transpose(port_map_enq)
    var valid_ptr_enq = 1.U(ncommit.W)
    
    for(i <- 0 until ncommit) {
        port_map_enq(i) := Mux(io.cmt.enq(i).valid, valid_ptr_enq, 0.U)
        valid_ptr_enq = Mux(io.cmt.enq(i).valid, ShiftAdd1(valid_ptr_enq), valid_ptr_enq)
    }
    flst.io.enq.zipWithIndex.foreach{ case (e, i) =>
        e.valid := port_map_trans_enq(i).orR
        e.bits := Mux1H(port_map_trans_enq(i), io.cmt.enq.map(_.bits))
    }
    io.cmt.enq.foreach(_.ready := DontCare)
    flst.io.flush := ShiftRegister(io.cmt.flush, 1, false.B, true.B)
    io.dif.free_list := flst.io.dbg_FIFO
}
    