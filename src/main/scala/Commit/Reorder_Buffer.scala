import chisel3._
import chisel3.util._
import CPU_Config.RegisterFile._
import CPU_Config.Decode._
import CPU_Config.Commit._
import CPU_Config.Issue._

class ROB_Frontend_Entry extends Bundle{
    val rd_vld      = Bool()
    val rd          = UInt(wlreg.W)
    val prd         = UInt(wpreg.W)
    val pprd        = UInt(wpreg.W)
    val pc          = UInt(32.W)
    val pred_type   = UInt(2.W)
    val is_store    = Bool()
    val is_priv     = Bool()
}

class ROB_Backend_Entry extends Bundle{
    val complete    = Bool()
    val jump_en     = Bool()
    val pred_fail   = Bool()
    val exception   = UInt(8.W)
    val result      = UInt(32.W)
    val nxt_cmt_en  = Bool()
}

class ROB_Entry extends Bundle{
    val fte = new ROB_Frontend_Entry
    val bke = new ROB_Backend_Entry
    
    def apply(fte: ROB_Frontend_Entry, bke: ROB_Backend_Entry): ROB_Entry = {
        val entry = Wire(new ROB_Entry)
        entry.fte := fte
        entry.bke := bke
        entry
    }
    def flush_gen(): Bool = {
        this.bke.pred_fail || this.bke.exception(7)
    }
}

object ROB_Entry {
    def apply(fte: ROB_Frontend_Entry, bke: ROB_Backend_Entry): ROB_Entry = {
        val entry = Wire(new ROB_Entry)
        entry.fte := fte
        entry.bke := bke
        entry
    }
}

class ROB_Frontend_IO extends Bundle{
    val enq     = Vec(ndecode, Flipped(Decoupled(new ROB_Frontend_Entry)))
    val enq_idx = Output(Vec(ndecode, new Cluster_Entry(nrob_q, ndecode)))
}

class ROB_Backend_IO extends Bundle{
    val widx    = Input(Vec(nissue, new Cluster_Entry(nrob_q, ndecode)))
    val wen     = Input(Vec(nissue, Bool()))
    val wdata   = Input(Vec(nissue, new ROB_Backend_Entry))
}

class ROB_Commit_IO extends Bundle{
    val deq     = Vec(ncommit, Decoupled(new ROB_Entry))
    val sb      = Flipped(new D_Commit_IO)
    val rnm     = Flipped(new Free_List_Commit_IO)
    val flush   = Output(Bool())
}

class Reorder_Buffer_IO extends Bundle{
    val fte = new ROB_Frontend_IO
    val bke = new ROB_Backend_IO
    val cmt = new ROB_Commit_IO
}


class Reorder_Buffer extends Module{
    val io = IO(new Reorder_Buffer_IO)

    val q = Module(new Cluster_Index_FIFO(new ROB_Entry, nrob, ndecode, ncommit, 0, nissue))

    // 1. frontend: in dispatch stage, each instruction will enqueue into the ROB
    q.io.enq.zip(io.fte.enq).foreach{case (enq, fte) =>
        enq.bits.fte := fte.bits
        enq.bits.bke := DontCare
        enq.valid := fte.valid
        fte.ready := enq.ready
    }
    io.fte.enq_idx := q.io.enq_idx
    // 2. backend: in writeback stage, some instruction will write some data into the ROB
    q.io.wdata.zip(io.bke.wdata).foreach{case (wdata, bke) =>
        wdata.fte := DontCare
        wdata.bke := bke
    }
    q.io.widx := io.bke.widx
    q.io.wen := io.bke.wen
    // 3. commit: in commit stage, some instruction will be committed
    
    for(i <- 0 until ncommit){
        io.cmt.deq(i).bits := {
            val entry = Wire(new ROB_Entry)
            entry.fte := q.io.deq(i).bits.fte
            entry.bke := q.io.deq(i).bits.bke
            entry
        }
        io.cmt.deq(i).valid := (
            if(i == 0) q.io.deq(i).bits.bke.complete
            else q.io.deq.take(i+1).map(_.bits.bke.complete).reduce(_ && _) 
              && q.io.deq(i-1).bits.bke.nxt_cmt_en
        )
        q.io.deq(i).ready := io.cmt.deq(i).valid
    }

    // output
    val last_cmt_index1H = Reverse(PriorityEncoderOH(Reverse(VecInit(io.cmt.deq.map(_.valid)).asUInt)))
    val last_cmt_item    = Mux1H(last_cmt_index1H, q.io.deq.map(_.bits))
    val flush            = last_cmt_item.flush_gen()
    // self flush
    q.io.flush       := flush || ShiftRegister(flush, 1, false.B, true.B)
    // store buffer
    io.cmt.sb.st_cmt := ShiftRegister(last_cmt_item.fte.is_store, 1, false.B, true.B)
    io.cmt.sb.flush  := ShiftRegister(flush, 1, false.B, true.B)
    // rename
    io.cmt.rnm.enq.zipWithIndex.foreach{ case (enq, i) =>
        enq.valid   := ShiftRegister(io.cmt.deq(i).valid && q.io.deq(i).bits.fte.rd_vld, 1, false.B, true.B)
        enq.bits    := ShiftRegister(q.io.deq(i).bits.fte.pprd, 1, 0.U(wpreg.W), true.B)
    }
    io.cmt.rnm.flush := ShiftRegister(flush, 1, false.B, true.B)


}