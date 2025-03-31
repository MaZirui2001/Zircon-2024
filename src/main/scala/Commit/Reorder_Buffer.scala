import chisel3._
import chisel3.util._
import CPU_Config.RegisterFile._
import CPU_Config.Decode._
import CPU_Config.Commit._
import CPU_Config.Issue._
import Jump_Op._
import EXE_Op._

class ROB_Frontend_Entry extends Bundle{
    val rd_vld      = Bool()
    val inst        = UInt(32.W)
    val rd          = UInt(wlreg.W)
    val prd         = UInt(wpreg.W)
    val pprd        = UInt(wpreg.W)
    val pc          = UInt(32.W)
    val pred_type   = UInt(2.W)
    val is_store    = Bool()

    def apply(pkg: Frontend_Package): ROB_Frontend_Entry = {
        val entry = Wire(new ROB_Frontend_Entry)
        entry.rd_vld      := pkg.rinfo.rd_vld
        entry.inst        := pkg.inst
        entry.rd          := pkg.rinfo.rd
        entry.prd         := pkg.pinfo.prd
        entry.pprd        := pkg.pinfo.pprd
        entry.pc          := pkg.pc
        entry.is_store    := pkg.op(6) && pkg.func(2)
        entry.pred_type   := Mux1H(Seq(
            ((pkg.op(4) && (pkg.op(2) || !pkg.op(1))) || (pkg.op(4, 0) === JAL && pkg.rinfo.rd === 0.U)) -> BR,
            (pkg.op(4) && !pkg.op(2) && pkg.op(1) && pkg.rinfo.rd =/= 0.U) -> CALL,
            (pkg.op(4, 0) === JALR && pkg.rinfo.rd === 0.U) -> RET,
        ))
        entry
    }
}

class ROB_Backend_Entry extends Bundle{
    val complete    = Bool()
    val jump_en     = Bool()
    val pred_fail   = Bool()
    val exception   = UInt(8.W)
    val result      = UInt(32.W)
    val nxt_cmt_en  = Bool()

    def apply(pkg: Backend_Package): ROB_Backend_Entry = {
        val entry = Wire(new ROB_Backend_Entry)
        entry.complete    := pkg.valid
        entry.jump_en     := pkg.jump_en
        entry.pred_fail   := pkg.pred_fail
        entry.exception   := pkg.exception
        entry.result      := pkg.result
        entry.nxt_cmt_en  := pkg.nxt_cmt_en
        entry
    }
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

class ROB_Dispatch_IO extends Bundle{
    val flush       = Output(Bool())
    val enq         = Vec(ndecode, Flipped(Decoupled(new ROB_Frontend_Entry)))
    val enq_idx     = Output(Vec(ndecode, new Cluster_Entry(wrob_q, wdecode)))
}

class ROB_Commit_IO extends Bundle{
    val deq         = Vec(ncommit, Decoupled(new ROB_Entry))
}
class Reorder_Buffer_IO extends Bundle{
    val fte = Flipped(new Frontend_Commit_IO)
    val bke = Flipped(new Backend_Commit_IO)
    val cmt = new ROB_Commit_IO
    val dsp = new ROB_Dispatch_IO
}


class Reorder_Buffer extends Module{
    val io = IO(new Reorder_Buffer_IO)

    val q = Module(new Cluster_Index_FIFO(new ROB_Entry, nrob, ndecode, ncommit, 0, nissue))

    // 1. frontend: in dispatch stage, each instruction will enqueue into the ROB
    q.io.enq.zip(io.dsp.enq).foreach{case (enq, fte) =>
        enq.bits.fte := fte.bits
        enq.bits.bke := DontCare
        enq.valid := fte.valid
        fte.ready := enq.ready
    }
    io.dsp.enq_idx.zip(q.io.enq_idx).foreach{ case(idx, enq) =>
        idx.qidx    := OHToUInt(enq.qidx)
        idx.offset  := OHToUInt(enq.offset)
        idx.high    := enq.high
    }
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
    io.bke.sb.st_cmt := ShiftRegister(last_cmt_item.fte.is_store, 1, false.B, true.B)
    io.bke.sb.flush  := ShiftRegister(flush, 1, false.B, true.B)
    // rename
    io.fte.rnm.flst.enq.zipWithIndex.foreach{ case (enq, i) =>
        enq.valid   := ShiftRegister(io.cmt.deq(i).valid && q.io.deq(i).bits.fte.rd_vld, 1, false.B, true.B)
        enq.bits    := ShiftRegister(q.io.deq(i).bits.fte.pprd, 1, 0.U(wpreg.W), true.B)
    }
    io.fte.rnm.flst.flush   := ShiftRegister(flush, 1, false.B, true.B)
    io.fte.rnm.srat.rd_vld.zipWithIndex.foreach{ case (rd_vld, i) =>
        rd_vld      := ShiftRegister(io.cmt.deq(i).valid && q.io.deq(i).bits.fte.rd_vld, 1, false.B, true.B)
    }
    io.fte.rnm.srat.rd      := ShiftRegister(VecInit(io.cmt.deq.map(_.bits.fte.rd)), 1, VecInit.fill(ncommit)(0.U(wlreg.W)), true.B)
    io.fte.rnm.srat.prd     := ShiftRegister(VecInit(io.cmt.deq.map(_.bits.fte.prd)), 1, VecInit.fill(ncommit)(0.U(wpreg.W)), true.B)
    io.fte.rnm.srat.flush   := ShiftRegister(flush, 1, false.B, true.B)

    // fetch queue flush
    io.fte.fq.flush         := ShiftRegister(flush, 1, false.B, true.B) 

    // npc
    io.fte.npc.flush        := ShiftRegister(flush, 1, false.B, true.B)
    io.fte.npc.jump_en      := ShiftRegister(last_cmt_item.bke.jump_en, 1, false.B, true.B)
    io.fte.npc.jump_tgt     := ShiftRegister(Mux(last_cmt_item.bke.jump_en, last_cmt_item.bke.result, last_cmt_item.fte.pc), 1, 0.U(32.W), true.B)

    // dispatch
    io.dsp.flush := ShiftRegister(flush, 1, false.B, true.B)

    // backend
    io.bke.flush := ShiftRegister(VecInit.fill(nissue)(flush), 1, VecInit.fill(nissue)(false.B), true.B)

}