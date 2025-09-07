
package ZirconUtil 
import chisel3._
import chisel3.util._

object ShiftAdd1 {
    def apply(x: UInt): UInt = {
        val n = x.getWidth
        x(n-2, 0) ## x(n-1)
    }
}
object ShiftSub1 {
    def apply(x: UInt): UInt = {
        val n = x.getWidth
        x(0) ## x(n-1, 1)
    }
}
object ShiftAddN {
    def apply(x: UInt, k: Int): UInt = {
        val n = x.getWidth
        if (k == 0) x
        else x(n-k-1, 0) ## x(n-1, n-k)
    }
}
object ShiftSubN {
    def apply(x: UInt, k: Int): UInt = {
        val n = x.getWidth
        if (k == 0) x
        else x(k-1, 0) ## x(n-1, k)
    }
}
object ESltu {  
    // extend slt unsigned
    def apply(src1: UInt, src2: UInt): Bool = {
        val n = src1.getWidth
        assert(n == src2.getWidth, "src1 and src2 must have the same width")
        val signNeq = src1(n-1) ^ src2(n-1)
        val src1LtSrc2 = src1(n-2, 0) < src2(n-2, 0)
        // Mux(signNeq, !src1LtSrc2, src1LtSrc2)
        signNeq ^ src1LtSrc2
    }
}
object Slt1H {
    // one-hot slt
    def apply(src1: UInt, src2: UInt): Bool = {
        val n = src1.getWidth
        assert(n == src2.getWidth, "src1 and src2 must have the same width")
        val src1Acc = VecInit.tabulate(n)(i => src1.take(i).orR)
        val src2Acc = VecInit.tabulate(n)(i => src2.take(i).orR)
        val diff = src1Acc.zip(src2Acc).map{ case (s1, s2) => s1 & !s2 }
        diff.reduce(_ | _)
    }
}
object SE {
    // sign extend
    def apply(x: UInt, n: Int = 32): UInt = {
        val len = x.getWidth
        assert(len <= n, "x must have less than n bits")
        val sign = x(len-1)
        Fill(n-len, sign) ## x
    }
}
object ZE {
    // zero extend
    def apply(x: UInt, n: Int = 32): UInt = {
        val len = x.getWidth
        assert(len <= n, "x must have less than n bits")
        Fill(n-len, 0.U) ## x
    }
}
object BitAlign {
    // BitAlign
    def apply(x: UInt, n: Int): UInt = {
        val len = x.getWidth
        if(len == n) x
        else if (len < n) ZE(x, n)
        else x(n-1, 0)
    }
}
object MuxOH {
    def apply[T <: Data](sel: Seq[Bool], in: Seq[T]): T = {
        val n = in.size
        assert(n > 0, "in must have at least one element")
        VecInit(in.zip(sel).map{
            case(i, s) => i.asUInt & Fill(i.getWidth, s)
        }).reduceTree((a: UInt, b: UInt) => (a | b)).asTypeOf(in(0))
    }
    
    def apply[T <: Data](sel: UInt, in:Seq[T]): T = {
        apply(sel.asBools, in)
        // Mux1H
    }
}
object WFirstRead {
    // write-first read
    def apply[T <: Data](rdata: T, ridx: UInt, widx: Seq[UInt], wdata: Seq[T], wen: Seq[Bool]): T = {
        assert(widx.size == wdata.size && widx.size == wen.size, "widx, wdata and wen must have the same size")
        val n = wdata.size
        val whit = VecInit.tabulate(n)(i => (ridx === widx(i)) && wen(i))
        Mux(whit.asUInt.orR, Mux1H(whit, wdata), rdata)
    }
}
object MTypeDecode {
    // memtype decode
    def apply(mtype: UInt, n: Int = 4): UInt = {
        val res = Wire(UInt(n.W))
        res := MuxLookup(mtype, 1.U(n.W))(Seq(
            0.U -> 0x1.U(n.W),
            1.U -> 0x3.U(n.W),
            2.U -> 0xf.U(n.W),
        ))
        res
    }
}
object MTypeEncode {
    // memtype encode
    def apply(mtype: UInt, n: Int = 2): UInt = {
        val res = Wire(UInt(n.W))
        res := MuxLookup(mtype, 0.U(n.W))(Seq(
            0x1.U -> 0.U(n.W),
            0x3.U -> 1.U(n.W),
            0xf.U -> 2.U(n.W),
        ))
        res
    }
}
object InheritFields {
    // inherit fields
    def apply[T <: Bundle, P <: Bundle](child: T, parent: P): Unit = {
        parent.elements.foreach { case (name, data) =>
            if (child.elements.contains(name)) {
                child.elements(name) := data
            }
        }
    }
}
object RotateRightOH {
    def apply(x: UInt, nOH: UInt): UInt = {
        val width = x.getWidth
        assert(width == nOH.getWidth, "two operators must have the same width")
        val xShifts = VecInit.tabulate(width)(i => ShiftSubN(x, i))
        Mux1H(nOH, xShifts)
    }
}
object RotateLeftOH {
    def apply(x: UInt, nOH: UInt): UInt = {  
        val width = x.getWidth
        assert(width == nOH.getWidth, "two operators must have the same width")
        val xShifts = VecInit.tabulate(width)(i => ShiftAddN(x, i))
        Mux1H(nOH, xShifts)
    }
}
object Transpose {
    def apply(x: Vec[UInt]): Vec[UInt] = {
        val n = x(0).getWidth
        VecInit.tabulate(n)(i => VecInit(x.map(_(i))).asUInt)
    }
}
object Lshift1H {
    def apply(x: UInt, nOH: UInt): UInt = {
        val width = nOH.getWidth
        val xShifts = VecInit.tabulate(width)(i => x << i)
        Mux1H(nOH, xShifts)
    }
}
object Rshift1H {
    def apply(x: UInt, nOH: UInt): UInt = {
        val width = nOH.getWidth
        val xShifts = VecInit.tabulate(width)(i => x >> i)
        Mux1H(nOH, xShifts)
    }
}

object Log2Rev {

  def apply(x: Bits, width: Int): UInt = {
    if (width < 2) {
      0.U
    } else if (width == 2) {
      x(1) && !x(0)
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

object Log2OH {
    def apply(x: Bits, width: Int): UInt = {
        if(width < 2) {
            x(0)
        } else if(width == 2) {
            Cat(x(1), (!x(1) && x(0)))
        } else if(width <= divideAndConquerThreshold) {
            Mux(x(width - 1), Cat(1.U(1.W), 0.U((width - 1).W)), Cat(0.U(1.W), apply(x(width - 2, 0), width - 1)))
        } else {
            val mid = 1 << (log2Ceil(width) - 1)
            val hi = x(width - 1, mid)
            val lo = x(mid - 1, 0)
            val usehi = hi.orR
            // Cat(usehi, Mux(usehi, apply(hi, width - mid), apply(lo, mid)))
            Mux(usehi, Cat(apply(hi, width - mid), 0.U(mid.W)), Cat(0.U((width - mid).W), apply(lo, mid)))
        }
    }
    def apply(x: Bits): UInt = apply(x, x.getWidth)
    def apply(x: Seq[Bool]): UInt = apply(VecInit(x).asUInt, x.size)
    private def divideAndConquerThreshold = 4
}

object Log2OHRev {
    def apply(x: Bits): UInt = {
        Reverse(Log2OH(Reverse(x.asUInt)))
    }
    def apply(x: Seq[Bool]): UInt = {
        apply(VecInit(x).asUInt)
    }
}
