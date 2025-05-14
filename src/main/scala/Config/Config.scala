package ZirconConfig
import chisel3._
import chisel3.util._

object EXEOp {
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

object JumpOp{
    val NOP     = 0x0.U(2.W)
    val BR      = 0x1.U(2.W)
    val CALL    = 0x2.U(2.W)
    val RET     = 0x3.U(2.W)
}
object RegisterFile{
    val nlreg = 32
    val wlreg = log2Ceil(nlreg)
    val npreg = 68
    val wpreg = log2Ceil(npreg)
}

object Issue{
    val niq          = 3
    val nis          = 5
    val arithNiq     = 15
    val arithNissue  = 3
    val muldivNiq    = 9
    val muldivNissue = 1
    val lsuNiq       = 9
    val lsuNissue    = 1
}
object Fetch{
    val nfch = 4
    val nfq = 16
}

object Predict{
    object GShare{
        import Fetch._
        val ghrWidth       = 12
        val phtWidth       = ghrWidth
        val phtSize        = 1 << phtWidth
    }
    object BTBMini{
        import Fetch._
        val bank              = nfch
        val bankWidth         = log2Ceil(bank)
        val size              = 64
        val addrWidth         = log2Ceil(size)
        assert(size % bank == 0, "size must be divisible by way")
        val sizePerBank       = size / bank
        val totalWidth        = 16
        val tagWidth          = totalWidth - addrWidth - bankWidth
        val way               = 2
    }
    object BTB{
        import Fetch._
        val totalWidth        = 12
    }
    object RAS{
        val size = 8
        val width = log2Ceil(size)
    }

}

object Decode{
    val ndcd = 3
    val wdecode = log2Ceil(ndcd)
}
object StoreBuffer{
    val nsb = 8
    val wsb = log2Ceil(nsb)
}
object Commit{
    import Decode._
    val ncommit = 2
    assert(ncommit <= ndcd, "ncommit must be less than or equal to ndcd")
    val nrob = 42
    assert(nrob % ndcd == 0, "nrob must be divisible by ndcd")
    val nrobQ = nrob / ndcd
    val wrob = log2Ceil(nrob)
    val wrobQ = log2Ceil(nrobQ)
    val nbdb = 18
    val nbdbQ = nbdb / ndcd
    val wbdb = log2Ceil(nbdb)
    val wbdbQ = log2Ceil(nbdbQ)
}
object Cache{
    import Fetch._
    val l1Way         = 2
    val l1Offset      = 6
    val l1Index       = 6
    val l1IndexNum    = 1 << l1Index
    val l1Tag         = 32 - l1Offset - l1Index
    val l1Line        = (1 << l1Offset)
    val l1LineBits    = l1Line * 8
    val icLine        = l1Line
    val icLineBits    = icLine * 8
    val fetchOffset   = 2 + log2Ceil(nfch)
    assert(l1Offset >= fetchOffset, "l1Offset must be greater than fetchOffset")
    val l2Offset      = 7
    val l2Index       = 7
    val l2IndexNum    = 1 << l2Index
    val l2Tag         = 32 - l2Offset - l2Index
    val l2Way         = 2 * l1Way
    val l2Line        = (1 << l2Offset)
    val l2LineBits    = l2Line * 8
}
object TLB{
    val ENTRYNUM = 16
}
