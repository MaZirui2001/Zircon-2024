import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.util._
import Multiply._
import EXE_Op._

class Mul_Test extends AnyFlatSpec with ChiselScalatestTester{
    behavior of "MUL"
    it should "pass" in {
        test(new Mul_Booth2_Wallce)
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
                val ref_res = op match{
                    case 0 => (a * b) & 0xFFFFFFFFL // MUL
                    case 1 => ((a.toLong * b.toLong) >>> 32) & 0xFFFFFFFFL // MULHU
                    case 2 => { // MULH
                        val a_signed = if (a > 0x7FFFFFFFL) a - 0x100000000L else a
                        val b_signed = if (b > 0x7FFFFFFFL) b - 0x100000000L else b
                        ((a_signed * b_signed) >> 32) & 0xFFFFFFFFL
                    }
                    case _ => 0L
                }
                // println(i)
                c.io.res.expect(ref_res.U, s"src1: 0x${a.toHexString}, src2: 0x${b.toHexString}, op: ${op}")
            }
        }
    }
}