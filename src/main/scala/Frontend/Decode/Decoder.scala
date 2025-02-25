import chisel3._
import chisel3.util._
import EXE_Op._
import Zircon_Util._

class Decoder_IO extends Bundle{
    val inst    = Input(UInt(32.W))
    val op      = Output(UInt(6.W))
    val imm     = Output(UInt(32.W))
}

class Decoder extends Module{
    val io = IO(new Decoder_IO)

    val inst            = io.inst
    val is_algebra_reg  = inst(31, 22) === 0.U
    val is_shift_imm    = inst(31, 22) === 1.U
    val is_algebra_imm  = inst(31, 25) === 1.U
    val is_lu12i        = inst(31, 27) === 1.U
    val is_pcaddu12i    = inst(31, 27) === 3.U
    val is_br           = inst(31, 30) === 1.U
    val is_mem          = inst(31, 28) === 2.U
    val is_atom         = is_mem && !inst(27)
    val is_load         = is_mem && !inst(24)
    val is_store        = is_mem && inst(24)
    val is_priv         = inst(31, 26) === 1.U


    /* op: 
        bit5: indicates src1 source, 0-reg 1-pc, or indicates store, 0-store, 1-not
        bit4: indicates src2 source, 0-reg 1-imm, or indicates load, 0-load, 1-not
        bit3-0: alu operation or memory operation(bit 3 indicates atom)
    */
    val op_5 = is_br || is_pcaddu12i || is_store
    val op_4 = !is_algebra_reg || is_load
    val op_3_0 = Mux1H(Seq(
        is_algebra_reg  -> Mux(inst(19) && !inst(18), 1.U, inst(18, 15)), // sra need to be 1
        is_shift_imm    -> Mux(inst(19), inst(18, 15), 0x7.U(3.W) ## (inst(18) && inst(15))),
        is_algebra_imm  -> (inst(18) && inst(17)) ## !inst(17) ## inst(16, 15),
        is_br           -> inst(29, 26),
        is_mem          -> Mux(is_atom, 0xa.U, inst(26, 25) ## inst(23, 22))
    ))
    io.op := op_5 ## op_4 ## op_3_0

    /* imm */
    val imm = Mux1H(Seq(
        is_shift_imm                -> ZE(inst(14, 10)),
        // algebra: addi, slti, sltui is se, else ze
        is_algebra_imm              -> Mux(inst(24), ZE(inst(21, 10)), SE(inst(21, 10))),
        // priv: cacop is 12 bits, else 14 bits for csr 
        is_priv                     -> Mux(inst(25), SE(inst(21, 10)), ZE(inst(23, 10))),
        (is_lu12i || is_pcaddu12i)  -> inst(24, 5) ## 0.U(12.W),
        // mem: atomic is 14 bits, else 12 bits
        is_mem                      -> Mux(inst(27), SE(inst(21, 10)), SE(inst(23, 10))),
        // br: bl and b is 28 bits, else 18 bits
        is_br                       -> Mux(inst(27) || inst(29), SE(inst(25, 10) ## 0.U(2.W)), SE(inst(9, 0) ## inst(25, 10) ## 0.U(2.W)))
    ))
    io.imm := imm

}