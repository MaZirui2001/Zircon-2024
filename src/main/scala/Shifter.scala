import chisel3._
import chisel3.util._

object Shifter {
    class Shifter_IO(n: Int) extends Bundle{
        val src = Input(UInt(n.W))
        val shf = Input(UInt(log2Ceil(n).W))
        val sgn = Input(Bool())
        val res = Output(UInt(n.W))
    }

    class Shifter extends Module{
        val io = IO(new Shifter_IO(32))
        // 桶形移位器，实现右移
        val candidates = Wire(Vec(32, UInt(32.W)))
        for (i <- 0 until 32){
            candidates(i) := VecInit.fill(i)(Mux(io.sgn, 0.U(1.W), io.src(31))).asUInt ## io.src(31, i)
        }
        io.res := candidates(io.shf)
    }

    object Shifter {
        def apply(src: UInt, shf: UInt, sgn: Bool): UInt = {
            val shifter = Module(new Shifter)
            shifter.io.src := src
            shifter.io.shf := shf
            shifter.io.sgn := sgn
            shifter.io.res
        }
    }

}