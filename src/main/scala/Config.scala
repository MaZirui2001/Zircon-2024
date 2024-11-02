import chisel3._
import chisel3.util._

object ALU_BR_Op extends ChiselEnum{
    val ADD, SUB, SLT, SLTU, AND, OR, XOR, NOR, SLL, SRL, SRA, LUI, 
        BEQ, BNE, BLT, BGE, BLTU, BGEU, B, BL, JIRL = Value
}
object MDU_Op extends ChiselEnum{
    val MUL, MULH, MULHU, DIV, DIVU, REM, REMU = Value
}
object CPU_Config{
    object RegisterFile{
        val nlreg = 32
        val wlreg = log2Ceil(nlreg)
        val npreg = 64
        val wpreg = log2Ceil(npreg)
    }
    object ReserveQueue{
        val npcq = 16
        val wpcq = log2Ceil(npcq)
        val nimq = 16
        val wimq = log2Ceil(nimq)
        val nrob = 32
        val wrob = log2Ceil(nrob)
    }
}