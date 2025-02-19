import chisel3._
import chisel3.util._

object EXE_Op {
    // alu
    val ADD     = 0x0.U(4.W)
    val SRA     = 0x1.U(4.W)
    val SUB     = 0x2.U(4.W)
    val ADD4    = 0x3.U(4.W)
    val SLT     = 0x4.U(4.W)
    val SLTU    = 0x5.U(4.W)
    val NOR     = 0x8.U(4.W)
    val AND     = 0x9.U(4.W)
    val OR      = 0xa.U(4.W)
    val XOR     = 0xb.U(4.W)
    val SLL     = 0xe.U(4.W)
    val SRL     = 0xf.U(4.W)
    // branch
    val JIRL    = 0x13.U(5.W)
    val B       = 0x14.U(5.W)
    val BL      = 0x15.U(5.W)
    val BEQ     = 0x16.U(5.W)
    val BNE     = 0x17.U(5.W)
    val BLT     = 0x18.U(5.W)
    val BGE     = 0x19.U(5.W)
    val BLTU    = 0x1a.U(5.W)
    val BGEU    = 0x1b.U(5.W)
    // mul and div
    val MUL     = 0x8.U(4.W)
    val MULH    = 0x9.U(4.W)
    val MULHU   = 0xa.U(4.W)
    val DIV     = 0x0.U(4.W)
    val MOD     = 0x1.U(4.W)
    val DIVU    = 0x2.U(4.W)
    val MODU    = 0x3.U(4.W)
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