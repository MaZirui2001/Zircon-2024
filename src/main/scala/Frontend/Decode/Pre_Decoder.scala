import chisel3._
import chisel3.util._

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
class Branch_Info extends Bundle {
    val jump_en     = Bool()
    val imm         = UInt(26.W)
}
class Pre_Decoder_IO extends Bundle {
    val inst    = Input(UInt(32.W))
    val rinfo   = Output(new Register_Info)
    // val binfo   = Output(new Branch_Info)
}

class Pre_Decoder extends Module {
    val io = IO(new Pre_Decoder_IO)
    val inst = io.inst

    // rd
    val rd_vld = VecInit(
        inst(31, 23) === 0.U && !(inst(21) && inst(19)), // algebraic reg and shift imm
        inst(31, 27) === 0.U && (inst(26) ^ inst(25)), // algebraic imm and csr
        inst(31, 28) === 1.U, //lu12i.w and pcaddu12i
        inst(31, 29) === 1.U && !((inst(27) && inst(24)) || inst(28)), // load and atomic
        inst(31, 29) === 0x2.U && (inst(28) ^ inst(27)) && inst(26) // jirl and bl
    ).reduceTree(_ || _)

    val rd_from_rj = inst(31, 20) === 0.U && inst(4, 0) === 0.U
    val rd_from_r1 = inst(31, 26) === 0x15.U
    val rd = Mux1H(Seq(
        rd_from_rj  -> inst(9, 5),
        rd_from_r1  -> 1.U,
        !(rd_from_rj || rd_from_r1) -> inst(4, 0)
    ))
    io.rinfo.rd_vld := Mux(rd === 0.U, false.B, rd_vld)
    io.rinfo.rd := Mux(rd_vld, rd, 0.U)

    // rj
    val rj_vld = VecInit(
        inst(31, 23) === 0.U && !(inst(21) && inst(19) || inst(22, 20) === 0.U), // algebraic reg
        inst(31, 25) === 1.U, // algebraic imm
        inst(31, 26) === 1.U && (!inst(25) && inst(9, 6) =/= 0.U || inst(25) && (!inst(22) || inst(16))), // csrxchg, cacop and idle
        inst(31, 29) === 1.U && !inst(28), // load and store
        inst(31, 26) === 0x13.U, // jirl
    ).reduceTree(_ || _)

    val rj = inst(9, 5)
    io.rinfo.rj_vld := Mux(rj === 0.U, false.B, rj_vld)
    io.rinfo.rj := Mux(rj_vld, rj, 0.U)

    // rk
    val rk_vld = VecInit(
        inst(31, 22) === 0.U && !(inst(21) && inst(19) || inst(21, 20) === 0.U), // algebraic reg
        inst(31, 25) === 3.U && inst(16), // invtlb
        inst(31, 29) === 1.U && (inst(27) && inst(24)),  // store
        inst(31, 30) === 0x1.U && (inst(29) || inst(28, 27) === 3.U) // branch
    ).reduceTree(_ || _)
    val rk = Mux(inst(31, 29) =/= 0.U, inst(4, 0), inst(14, 10))
    io.rinfo.rk_vld := Mux(rk === 0.U, false.B, rk_vld)
    io.rinfo.rk := Mux(rk_vld, rk, 0.U)
}