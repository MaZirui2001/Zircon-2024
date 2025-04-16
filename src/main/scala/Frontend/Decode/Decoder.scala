import chisel3._
import chisel3.util._
import ZirconConfig.EXEOp._
import ZirconConfig.Issue._
import ZirconUtil._

class DecoderIO extends Bundle{
    val inst    = Input(UInt(32.W))
    val rinfo   = Input(new RegisterInfo())
    val op      = Output(UInt(7.W))
    val imm     = Output(UInt(32.W))
    val func    = Output(UInt(niq.W))
}

class Decoder extends Module{
    val io = IO(new DecoderIO)

    val inst         = io.inst
    val funct3       = inst(14, 12)
    val funct7       = inst(31, 25)
    val isAlgebraReg = inst(6, 0) === 0x33.U && funct7(0) === 0.U
    val isAlgebraImm = inst(6, 0) === 0x13.U
    val isLui        = inst(6, 0) === 0x37.U
    val isAuipc      = inst(6, 0) === 0x17.U
    val isJal        = inst(6, 0) === 0x6f.U
    val isJalr       = inst(6, 0) === 0x67.U
    val isBr         = inst(6, 0) === 0x63.U
    val isPriv       = inst(6, 0) === 0x73.U || inst(6, 0) === 0x0f.U
    val isAtom       = inst(6, 0) === 0x2f.U
    val isLoad       = inst(6, 0) === 0x03.U || isAtom && inst(31, 27) === 0x02.U
    val isStore      = inst(6, 0) === 0x23.U || isAtom && inst(31, 27) === 0x03.U
    val isMem        = isLoad || isStore
    val isMuldiv     = inst(6, 0) === 0x33.U && funct7(0) === 1.U

    /* op: 
        bit6: indicates src1 source, 0-reg 1-pc, or indicates store, 1-store, 0-not
        bit5: indicates src2 source, 1-reg 0-imm, or indicates load, 1-load, 0-not
        bit4: indicates is branch or jump
        bit3-0: alu operation or memory operation(bit 3 indicates atom)
    */
    val op_6 = isJal || isJalr || isAuipc || isStore
    val op_5 = isAlgebraReg || isLoad
    val op_4 = isBr || isJal || isJalr
    val op_3_0 = Mux1H(Seq(
        (isAlgebraReg || isMuldiv)  -> funct7(5) ## funct3,
        isAlgebraImm  -> Mux(funct3 === 0x5.U, funct7(5) ## funct3, 0.U(1.W) ## funct3),
        isJalr        -> JALR(3, 0),
        isJal         -> JAL(3, 0),
        isBr          -> 1.U(1.W) ## funct3,
        isMem         -> isAtom ## funct3
    ))
    io.op := op_6 ## op_5 ## op_4 ## op_3_0

    /* imm */
    val IType = isAlgebraImm || isLoad || isJalr
    val SType = isStore
    val JType = isJal
    val UType = isLui || isAuipc
    val BType = isBr
    val imm   = Mux1H(Seq(
        IType   -> SE(inst(31, 20)),
        UType   -> inst(31, 12) ## 0.U(12.W),
        JType   -> SE(inst(31) ## inst(19, 12) ## inst(20) ## inst(30, 21) ## 0.U(1.W)),
        BType   -> SE(inst(31) ## inst(7) ## inst(30, 25) ## inst(11, 8) ## 0.U(1.W)),
        SType   -> SE(inst(31, 25) ## inst(11, 7)),
        // priv: bit11-0 is csr, bit 16-12 is uimm
        isPriv  -> ZE(inst(19, 15) ## inst(31, 20))
    ))
    io.imm := imm

    io.func := isMem ## (isMuldiv || isPriv) ## !(isMem || isMuldiv || isPriv)

}