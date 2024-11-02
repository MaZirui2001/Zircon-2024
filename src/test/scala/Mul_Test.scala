import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.util._
import Multiply._
import MDU_Op._

class Mul_Test extends AnyFlatSpec with ChiselScalatestTester{
    behavior of "MUL"
    it should "pass" in {
        test(new Mul_Booth2_Wallce)
        .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation))
        { c =>
            val values = MDU_Op.all
            for (i <- 0 until 100) {
                val a = Random.nextLong(0xFFFFFFFFL)
                val b = Random.nextLong(0xFFFFFFFFL)

                val op = values((Random.nextInt(MDU_Op.all.length % 3)))
                // val op = MUL
                c.io.src1.poke(a.U)
                c.io.src2.poke(b.U)
                c.io.op.poke(op)
                for(j <- 0 until 3){
                    c.clock.step(1)
                }
                
                // 十六进制输出结果
                val ref_res = op match{
                    case MUL => (a * b) & 0xFFFFFFFFL
                    case MULHU => ((a * b) >> 32) & 0xFFFFFFFFL
                    case MULH => ((a.toLong * b.toLong) >> 32) & 0xFFFFFFFFL
                }
                // println(i)
                val res = c.io.res.expect(ref_res, s"src1: 0x${a.toHexString}, src2: 0x${b.toHexString}, op: ${op}")

            }
        }
    }
}