import chisel3._
import chisel3.util._


object Zircon_Util{
    def shift_add_1(x: UInt): UInt = {
        val n = x.getWidth
        x(n-2, 0) ## x(n-1)
    }
    def shift_sub_1(x: UInt): UInt = {
        val n = x.getWidth
        x(0) ## x(n-1, 1)
    }
    def shift_add_n(x: UInt, k: Int): UInt = {
        val n = x.getWidth
        if (k == 0) x
        else x(n-k-1, 0) ## x(n-1, n-k)
    }
    def shift_sub_n(x: UInt, k: Int): UInt = {
        val n = x.getWidth
        if (k == 0) x
        else x(k-1, 0) ## x(n-1, k)
    }
    // extend slt unsigned
    def esltu(src1: UInt, src2: UInt): Bool = {
        val n = src1.getWidth
        assert(n == src2.getWidth, "src1 and src2 must have the same width")
        val sign_neq = src1(n-1) ^ src2(n-1)
        val src1_lt_src2 = src1(n-2, 0) < src2(n-2, 0)
        Mux(sign_neq, !src1_lt_src2, src1_lt_src2)
    }
    // one-hot slt
    def slt_1H(src1: UInt, src2: UInt): Bool = {
        val n = src1.getWidth
        assert(n == src2.getWidth, "src1 and src2 must have the same width")
        val src1_acc = VecInit.tabulate(n)(i => src1.take(i).orR)
        val src2_acc = VecInit.tabulate(n)(i => src2.take(i).orR)
        val diff = src1_acc.zip(src2_acc).map{ case (s1, s2) => s1 & !s2 }
        diff.reduce(_ | _)
    }
    // write-first read
    def wfirst_read[T <: Data](rdata: T, ridx: UInt, widx: Vec[UInt], wdata: Vec[T], wen: Vec[Bool]): T = {
        assert(widx.size == wdata.size && widx.size == wen.size, "widx, wdata and wen must have the same size")
        val n = wdata.size
        val whit = VecInit.tabulate(n)(i => (ridx === widx(i)) && wen(i))
        Mux(whit.asUInt.orR, Mux1H(whit, wdata), rdata)
    }

}
