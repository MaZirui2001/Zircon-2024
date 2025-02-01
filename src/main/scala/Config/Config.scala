import chisel3._
import chisel3.util._

object ALU_BR_Op extends ChiselEnum{
    val ADD, SUB, SLT, SLTU, AND, OR, XOR, NOR, SLL, SRL, SRA, LUI = Value
    val BEQ = Value(0x10.U) 
    val BNE, BLT, BGE, BLTU, BGEU, B, BL, JIRL = Value

    def getVal(op: ALU_BR_Op.Type): UInt = {
        op.asUInt.take(getWidth-1)
    }
    def getType(op: ALU_BR_Op.Type): Bool = {
        op.asUInt(getWidth-1).asBool
    }
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
    object Issue{
        val wissue = 5
    }
    object StoreBuffer{
        val nsb = 8
        val wsb = log2Ceil(nsb)
    }
    object Cache{
        val l1_way          = 2
        val ic_offset       = 3
        val ic_index        = 3
        val ic_tag          = 32 - ic_offset - ic_index
        // val ic_way          = 2
        val ic_line         = (1 << ic_offset)
        val ic_line_bits    = ic_line * 8

        val dc_offset       = 3
        val dc_index        = 3
        val dc_tag          = 32 - dc_offset - dc_index
        // val dc_way          = 2
        val dc_line         = (1 << dc_offset)
        val dc_line_bits    = dc_line * 8

        val l2_offset       = 5
        val l2_index        = 5
        val l2_index_num    = 1 << l2_index
        val l2_tag          = 32 - l2_offset - l2_index
        val l2_way          = 2 * l1_way
        val l2_line         = (1 << l2_offset)
        val l2_line_bits    = l2_line * 8

    }
    object TLB{
        val ENTRY_NUM = 16
    }
}