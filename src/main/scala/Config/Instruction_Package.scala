import chisel3._
import chisel3.util._
import CPU_Config.Issue._
import CPU_Config.Decode._
import CPU_Config.Commit._
import CPU_Config.RegisterFile._

class Predict_Info extends Bundle {
    val offset      = UInt(32.W)
    val jump_en     = Bool()
    val vld         = Bool()
}

class Frontend_Package extends Bundle {
    val valid       = Bool()
    val pc          = UInt(32.W)
    val pred_info   = new Predict_Info()
    val inst        = UInt(32.W)
    val op          = UInt(7.W)
    val imm         = UInt(32.W)
    val func        = UInt(niq.W)
    val rinfo       = new Register_Info()
    val pinfo       = new PRegister_Info()
}

class Backend_Package extends Bundle {
    val valid       = Bool()
    val pc          = UInt(32.W)
    val pred_offset = UInt(32.W)
    val prj         = UInt(wpreg.W)
    val prk         = UInt(wpreg.W)
    val prd         = UInt(wpreg.W)
    val rd_vld      = Bool()
    val op          = UInt(7.W)
    val imm         = UInt(32.W)
    val rob_idx     = new Cluster_Entry(wrob_q, wdecode)
    val prj_wk      = Bool()
    val prk_wk      = Bool()

    // for inferred wakeup
    val prj_lpv     = UInt(3.W)
    val prk_lpv     = UInt(3.W)

    val is_latest   = Bool()
    val src1        = UInt(32.W)
    val src2        = UInt(32.W)
    val rf_wdata    = UInt(32.W)
    val jump_en     = Bool()
    val pred_fail   = Bool()
    val exception   = UInt(8.W)
    val result      = UInt(32.W)
    val nxt_cmt_en  = Bool()
    
    def apply(fte: Frontend_Package, rob_idx: Cluster_Entry, prj_info: Ready_Board_Entry, prk_info: Ready_Board_Entry): Backend_Package = {
        val bke = Wire(new Backend_Package)
        bke.valid       := fte.valid
        bke.pc          := fte.pc
        bke.pred_offset := fte.pred_info.offset
        bke.prj         := fte.pinfo.prj
        bke.prk         := fte.pinfo.prk
        bke.prd         := fte.pinfo.prd
        bke.rd_vld      := fte.rinfo.rd_vld
        bke.op          := fte.op(6, 0)
        bke.imm         := fte.imm
        bke.rob_idx     := rob_idx
        bke.prj_wk      := prj_info.ready && fte.pinfo.prj_wk
        bke.prk_wk      := prk_info.ready && fte.pinfo.prk_wk
        bke.prj_lpv     := prj_info.lpv
        bke.prk_lpv     := prk_info.lpv
        bke.is_latest   := false.B
        bke.src1        := 0.U
        bke.src2        := 0.U
        bke.result      := 0.U
        bke.rf_wdata    := 0.U
        bke.jump_en     := false.B
        bke.pred_fail   := false.B
        bke.exception   := 0.U
        bke.nxt_cmt_en  := false.B
        bke
    }
}
