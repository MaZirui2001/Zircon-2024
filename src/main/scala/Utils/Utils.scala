import chisel3._
import chisel3.util._



object Zircon_Util{
    def shift_add_1(x: UInt): UInt = {
        val n = x.getWidth
        x(n-2, 0) ## x(n-1)
    }
    def shift_1(x: UInt): UInt = {
        val n = x.getWidth
        x(n-2, 0) ## 0.U(1.W)
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
    // sign extend
    def SE(x: UInt, n: Int = 32): UInt = {
        val len = x.getWidth
        assert(len <= n, "x must have less than n bits")
        val sign = x(len-1)
        Fill(n-len, sign) ## x
    }
    // zero extend
    def ZE(x: UInt, n: Int = 32): UInt = {
        val len = x.getWidth
        assert(len <= n, "x must have less than n bits")
        Fill(n-len, 0.U) ## x
    }
    // align
    def align(x: UInt, n: Int): UInt = {
        val len = x.getWidth
        if(len == n) x
        else if (len < n) ZE(x, n)
        else x(n-1, 0)
    }
    def MuxOH[T <: Data](sel: Seq[Bool], in: Seq[T]): T = {
        val n = in.size
        assert(n > 0, "in must have at least one element")
        VecInit(in.zip(sel).map{
            case(i, s) => i.asUInt & Fill(i.getWidth, s)
        }).reduceTree((a: UInt, b: UInt) => (a | b)).asTypeOf(in(0))
    }
    def MuxOH[T <: Data](sel: UInt, in:Seq[T]): T = {
        MuxOH(sel.asBools, in)
        // Mux1H
    }
    // write-first read
    def wfirst_read[T <: Data](rdata: T, ridx: UInt, widx: Seq[UInt], wdata: Seq[T], wen: Seq[Bool]): T = {
        assert(widx.size == wdata.size && widx.size == wen.size, "widx, wdata and wen must have the same size")
        val n = wdata.size
        val whit = VecInit.tabulate(n)(i => (ridx === widx(i)) && wen(i))
        Mux(whit.asUInt.orR, Mux1H(whit, wdata), rdata)
    }
    // memtype decode
    def mtype_decode(mtype: UInt, n: Int = 4): UInt = {
        val res = Wire(UInt(n.W))
        res := MuxLookup(mtype, 1.U(n.W))(Seq(
            0.U -> 0x1.U(n.W),
            1.U -> 0x3.U(n.W),
            2.U -> 0xf.U(n.W),
        ))
        res
    }
    // memtype encode
    def mtype_encode(mtype: UInt, n: Int = 2): UInt = {
        val res = Wire(UInt(n.W))
        res := MuxLookup(mtype, 0.U(n.W))(Seq(
            0x1.U -> 0.U(n.W),
            0x3.U -> 1.U(n.W),
            0xf.U -> 2.U(n.W),
        ))
        res
    }
    // inherit fields
    def inheritFields[T <: Bundle, P <: Bundle](child: T, parent: P): Unit = {
        parent.elements.foreach { case (name, data) =>
            if (child.elements.contains(name)) {
                child.elements(name) := data
            }
        }
    }
    def rotateRightOH(x: UInt, nOH: UInt): UInt = {
        val width = x.getWidth
        assert(width == nOH.getWidth, "two operators must have the same width")
        val x_shifts = VecInit.tabulate(width)(i => shift_sub_n(x, i))
        Mux1H(nOH, x_shifts)
    }
    def rotateLeftOH(x: UInt, nOH: UInt): UInt = {  
        val width = x.getWidth
        assert(width == nOH.getWidth, "two operators must have the same width")
        val x_shifts = VecInit.tabulate(width)(i => shift_add_n(x, i))
        Mux1H(nOH, x_shifts)
    }
    def transpose(x: Vec[UInt]): Vec[UInt] = {
        val n = x(0).getWidth
        VecInit.tabulate(n)(i => VecInit(x.map(_(i))).asUInt)
    }
    def lshift1H(x: UInt, nOH: UInt): UInt = {
        val width = nOH.getWidth
        val x_shifts = VecInit.tabulate(width)(i => x << i)
        Mux1H(nOH, x_shifts)
    }
    def rshift1H(x: UInt, nOH: UInt): UInt = {
        val width = nOH.getWidth
        val x_shifts = VecInit.tabulate(width)(i => x >> i)
        Mux1H(nOH, x_shifts)
    }
}

object Log2Rev {

  def apply(x: Bits, width: Int): UInt = {
    if (width < 2) {
      1.U
    } else if (width == 2) {
      !x(0)
    } else if( width <= divideAndConquerThreshold){
        PriorityEncoder(x)
    } else {
      val mid = 1 << (log2Ceil(width) - 1)
      val hi = x(width - 1, mid)
      val lo = x(mid - 1, 0)
      val useLo = lo.orR
      Cat(!useLo, Mux(useLo, Log2Rev(lo, mid), Log2Rev(hi, width - mid)))
    }
  }

  def apply(x: Bits): UInt = apply(x, x.getWidth)

  private def divideAndConquerThreshold = 4
}