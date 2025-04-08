import chisel3._
import chisel3.util._
import Zircon_Config.EXE_Op._
import Zircon_Config.Issue._
import Zircon_Util._

class Decoder_IO extends Bundle{
    val inst    = Input(UInt(32.W))
    val rinfo   = Input(new Register_Info())
    val op      = Output(UInt(7.W))
    val imm     = Output(UInt(32.W))
    val func    = Output(UInt(niq.W))
}

class Decoder extends Module{
    val io = IO(new Decoder_IO)

    val inst            = io.inst
    val funct3          = inst(14, 12)
    val funct7          = inst(31, 25)
    val is_algebra_reg  = inst(6, 0) === 0x33.U && funct7(0) === 0.U
    val is_algebra_imm  = inst(6, 0) === 0x13.U
    val is_lui          = inst(6, 0) === 0x37.U
    val is_auipc        = inst(6, 0) === 0x17.U
    val is_jal          = inst(6, 0) === 0x6f.U
    val is_jalr         = inst(6, 0) === 0x67.U
    val is_br           = inst(6, 0) === 0x63.U
    val is_priv         = inst(6, 0) === 0x73.U || inst(6, 0) === 0x0f.U
    val is_atom         = inst(6, 0) === 0x2f.U
    val is_load         = inst(6, 0) === 0x03.U || is_atom && inst(31, 27) === 0x02.U
    val is_store        = inst(6, 0) === 0x23.U || is_atom && inst(31, 27) === 0x03.U
    val is_mem          = is_load || is_store
    val is_muldiv       = inst(6, 0) === 0x33.U && funct7(0) === 1.U

    /* op: 
        bit6: indicates src1 source, 0-reg 1-pc, or indicates store, 1-store, 0-not
        bit5: indicates src2 source, 1-reg 0-imm, or indicates load, 1-load, 0-not
        bit4: indicates is branch or jump
        bit3-0: alu operation or memory operation(bit 3 indicates atom)
    */
    val op_6 = is_jal || is_jalr || is_auipc || is_store
    val op_5 = is_algebra_reg || is_load
    val op_4 = is_br || is_jal || is_jalr
    val op_3_0 = Mux1H(Seq(
        (is_algebra_reg || is_muldiv)  -> funct7(5) ## funct3,
        is_algebra_imm  -> Mux(funct3 === 0x5.U, funct7(5) ## funct3, 0.U(1.W) ## funct3),
        is_jalr         -> JALR(3, 0),
        is_jal          -> JAL(3, 0),
        is_br           -> 1.U(1.W) ## funct3,
        is_mem          -> is_atom ## funct3
    ))
    io.op := op_6 ## op_5 ## op_4 ## op_3_0

    /* imm */
    val I_type = is_algebra_imm || is_load || is_jalr
    val S_type = is_store
    val J_type = is_jal
    val U_type = is_lui || is_auipc
    val B_type = is_br
    val imm = Mux1H(Seq(
        I_type     -> SE(inst(31, 20)),
        U_type     -> inst(31, 12) ## 0.U(12.W),
        J_type     -> SE(inst(31) ## inst(19, 12) ## inst(20) ## inst(30, 21) ## 0.U(1.W)),
        B_type     -> SE(inst(31) ## inst(7) ## inst(30, 25) ## inst(11, 8) ## 0.U(1.W)),
        S_type     -> SE(inst(31, 25) ## inst(11, 7)),
        // priv: bit11-0 is csr, bit 16-12 is uimm
        is_priv    -> ZE(inst(19, 15) ## inst(31, 20))
    ))
    io.imm := imm

    io.func := is_mem ## (is_muldiv || is_priv) ## !(is_mem || is_muldiv || is_priv)

    // io.jump_op := Mux1H(Seq(
    //     (is_jalr && io.rinfo.rd =/= 0.U  || is_jal) -> CALL,
    //     (is_jalr && io.rinfo.rd === 0.U)            -> RET,
    //     is_br                                       -> BR,
    //     (!(is_br || is_jal || is_jalr))             -> NOP
    // ))

}