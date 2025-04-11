import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.util._
import ZirconConfig.EXEOp._

class MulTest extends AnyFlatSpec with ChiselScalatestTester{
    behavior of "MUL"
    it should "pass" in {
        test(new MulBooth2Wallce)
        .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation))
        { c =>
            for (i <- 0 until 100) {
                val a = Random.nextLong(0xFFFFFFFFL)
                val b = Random.nextLong(0xFFFFFFFFL)

                val op = Random.nextInt(8)
                // val op = MUL
                c.io.src1.poke(a.U)
                c.io.src2.poke(b.U)
                c.io.op.poke(op.U)
                for(j <- 0 until 3){
                    c.clock.step(1)
                }
                
                // 十六进制输出结果
                val refRes = op match{
                    case 0 => (a * b) & 0xFFFFFFFFL // MUL
                    case 1 => ((a.toLong * b.toLong) >>> 32) & 0xFFFFFFFFL // MULHU
                    case 2 => { // MULH
                        val aSigned = if (a > 0x7FFFFFFFL) a - 0x100000000L else a
                        val bSigned = if (b > 0x7FFFFFFFL) b - 0x100000000L else b
                        ((aSigned * bSigned) >> 32) & 0xFFFFFFFFL
                    }
                    case _ => 0L
                }
                // println(i)
                c.io.res.expect(refRes.U, s"src1: 0x${a.toHexString}, src2: 0x${b.toHexString}, op: ${op}")
            }
        }
    }
}