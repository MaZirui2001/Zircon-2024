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
    object Fetch{
        val nfetch = 2
    }
    object StoreBuffer{
        val nsb = 8
        val wsb = log2Ceil(nsb)
    }
    object Cache{
        import CPU_Config.Fetch._
        val l1_way          = 2
        val l1_offset       = 4
        val l1_index        = 3
        val l1_index_num    = 1 << l1_index
        val l1_tag          = 32 - l1_offset - l1_index
        val l1_line         = (1 << l1_offset)
        val l1_line_bits    = l1_line * 8
        val ic_line         = l1_line + nfetch * 4
        val ic_line_bits    = ic_line * 8
        val fetch_offset    = 2 + log2Ceil(nfetch)
        assert(l1_offset >= fetch_offset, "l1_offset must be greater than fetch_offset")

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