import chisel3._
import chisel3.util._
import Zircon_Util._
import Adder._
import CPU_Config.Fetch._

/* Pre-Decoder: 
    In order to shorten the frontend pipleline, we need to decode the instruction
    in the previous stage.
    However, because the decode may be a critical path by connected behind the icache, 
    we can only decode something that will use in rename stage or the branch predictor
    in this stage.
*/
class Register_Info extends Bundle {
    val rd_vld  = Bool()
    val rd      = UInt(5.W)
    val rj_vld  = Bool()
    val rj      = UInt(5.W)
    val rk_vld  = Bool()
    val rk      = UInt(5.W)
}
class Pre_Decoder_IO extends Bundle {
    val inst_pkg    = Input(new Frontend_Package)
    val rinfo       = Output(new Register_Info)
    val npc         = Flipped(new NPC_PreDecode_IO)
}

class Pre_Decoder extends Module {
    val io = IO(new Pre_Decoder_IO)
    val inst_pkg    = io.inst_pkg
    val pred_info   = inst_pkg.pred_info
    val inst        = inst_pkg.inst

    // rd
    val rd_vld = inst(3, 0) === 0x3.U && !(
        inst(6, 4) === 0x6.U || // store
        inst(6, 4) === 0x2.U    // branch
    ) || inst(2, 0) === 0x7.U

    val rd = inst(11, 7)
    io.rinfo.rd_vld := Mux(rd === 0.U, false.B, rd_vld)
    io.rinfo.rd := Mux(rd_vld, rd, 0.U)

    // rj
    val rj_vld = (
        inst(2, 0) === 0x3.U && !(inst(6, 3) === 0xe.U && inst(14)) ||
        inst(6, 0) === 0x67.U || // jalr
        inst(3, 0) === 0xf.U
    )

    val rj = inst(19, 15)
    io.rinfo.rj_vld := Mux(rj === 0.U, false.B, rj_vld)
    io.rinfo.rj := Mux(rj_vld, rj, 0.U)

    // rk
    val rk_vld = (
        inst(6, 0) === 0x63.U || // branch
        inst(6, 0) === 0x23.U || // store
        inst(6, 0) === 0x33.U || // reg-reg
        inst(6, 0) === 0x2f.U // atom
    )
    val rk = inst(24, 20)
    io.rinfo.rk_vld := Mux(rk === 0.U, false.B, rk_vld)
    io.rinfo.rk := Mux(rk_vld, rk, 0.U)

    /* branch */
    // jalr: not care
    // jal: must jump, and jump_tgt must be pc + imm
    // branch: if not pred, static predict; if pred, jump_tgt must be pc + pred_offset
    // if not jump or branch, shouldn't predict jump

    val is_jalr = inst(6, 0) === 0x67.U
    val is_jal  = inst(6, 0) === 0x6f.U
    val is_br   = inst(6, 0) === 0x63.U
    val isn_j  = !(is_jalr || is_jal || is_br)

    val imm = Mux1H(Seq(
        is_jal  -> SE(inst(31) ## inst(19, 12) ## inst(20) ## inst(30, 21) ## 0.U(1.W)),
        is_br   -> SE(inst(31) ## inst(7) ## inst(30, 25) ## inst(11, 8) ## 0.U(1.W))
    ))

    io.npc.flush := Mux1H(Seq(
        isn_j   -> pred_info.jump_en, // isn't jump: predictor must be wrong if it predicts jump
        is_jal  -> (pred_info.offset =/= imm), 
        is_br   -> Mux(pred_info.vld, pred_info.offset =/= imm, imm(31))
    )) && io.inst_pkg.valid

    io.npc.jump_offset := Mux(isn_j, 4.U, imm)
    io.npc.pc := inst_pkg.pc

}

class Pre_Decoders_IO extends Bundle {
    val inst_pkg    = Input(Vec(nfetch, new Frontend_Package))
    val rinfo       = Vec(nfetch, Decoupled(new Register_Info))
    val npc         = Flipped(new NPC_PreDecode_IO)
}

class Pre_Decoders extends Module {
    val io = IO(new Pre_Decoders_IO)
    val pds = VecInit.fill(nfetch)(Module(new Pre_Decoder).io)
    for (i <- 0 until nfetch) {
        pds(i).inst_pkg := io.inst_pkg(i)
        io.rinfo(i).bits := pds(i).rinfo
        io.rinfo(i).valid := (if(i == 0) true.B else !pds.map{case(p) => p.npc.flush && p.inst_pkg.valid}.take(i).reduce(_ || _)) && io.inst_pkg(i).valid
    }
    io.npc.flush := pds.map(_.npc.flush).reduce(_ || _)
    io.npc.jump_offset := Mux1H(Log2OHRev(pds.map(_.npc.flush)), pds.map(_.npc.jump_offset))
    io.npc.pc := Mux1H(Log2OHRev(pds.map(_.npc.flush)), pds.map(_.npc.pc))
}