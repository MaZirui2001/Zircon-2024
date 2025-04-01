import chisel3._
import chisel3.util._

object EXE_Op {
    // alu
    val ADD     = 0x0.U(5.W)
    val SLL     = 0x1.U(5.W)
    val SLT     = 0x2.U(5.W)
    val SLTU    = 0x3.U(5.W)
    val XOR     = 0x4.U(5.W)
    val SRL     = 0x5.U(5.W)
    val OR      = 0x6.U(5.W)
    val AND     = 0x7.U(5.W)
    val SUB     = 0x8.U(5.W)
    val SRA     = 0xd.U(5.W)
    
    // branch
    val BEQ     = 0x18.U(5.W)
    val BNE     = 0x19.U(5.W)
    val JALR    = 0x1a.U(5.W)
    val JAL     = 0x1b.U(5.W)
    val BLT     = 0x1c.U(5.W)
    val BGE     = 0x1d.U(5.W)
    val BLTU    = 0x1e.U(5.W)
    val BGEU    = 0x1f.U(5.W)
    
    // mul and div
    val MUL     = 0x0.U(4.W)
    val MULH    = 0x1.U(4.W)
    val MULHSU  = 0x2.U(4.W)
    val MULHU   = 0x3.U(4.W)
    val DIV     = 0x4.U(4.W)
    val DIVU    = 0x5.U(4.W)
    val REM     = 0x6.U(4.W)
    val REMU    = 0x7.U(4.W)
}

object Jump_Op{
    val NOP     = 0x0.U(4.W)
    val BR      = 0x1.U(4.W)
    val CALL    = 0x2.U(4.W)
    val RET     = 0x3.U(4.W)
}
object CPU_Config{
    object RegisterFile{
        val nlreg = 32
        val wlreg = log2Ceil(nlreg)
        val npreg = 62
        val wpreg = log2Ceil(npreg)
    }
    object ReserveQueue{
        val npcq = 16
        val wpcq = log2Ceil(npcq)
        val nimq = 16
        val wimq = log2Ceil(nimq)
    }
    object Issue{
        val niq           = 3
        val nissue        = 5
        val arith_niq     = 24
        val arith_nissue  = 3
        val muldiv_niq    = 9
        val muldiv_nissue = 1
        val lsu_niq       = 9
        val lsu_nissue    = 1
    }
    object Fetch{
        val nfetch = 4
        val nfetch_q = 16
    }
    object Decode{
        val ndecode = 3
        val wdecode = log2Ceil(ndecode)
    }
    object StoreBuffer{
        val nsb = 8
        val wsb = log2Ceil(nsb)
    }
    object Commit{
        import CPU_Config.Decode._
        val ncommit = 2
        assert(ncommit <= ndecode, "ncommit must be less than or equal to ndecode")
        val nrob = 60
        assert(nrob % ndecode == 0, "nrob must be divisible by ndecode")
        val nrob_q = nrob / ndecode
        val wrob = log2Ceil(nrob)
        val wrob_q = log2Ceil(nrob_q)
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