import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.util._
import Adder._
import ALU_BR_Op._

class AdderTest extends AnyFlatSpec with ChiselScalatestTester{
    behavior of "Adder"
    it should "pass" in {
        test(new ALU).withAnnotations(Seq(WriteVcdAnnotation)){ c =>
            for (i <- 0 until 1000) {
                val a = Random.nextLong(0xFFFFFFFFL)
                val b = Random.nextLong(0xFFFFFFFFL)
                // val ci = Random.nextInt(2)
                c.io.src1.poke(a.U)
                c.io.src2.poke(b.U)
                c.io.op.poke(SLTU)
                c.clock.step(1)
                // 十六进制输出结果
                val res = c.io.res.expect( if(a < b) 1.U else 0.U, // 十六进制：
                    s"src1: 0x${a.toHexString}, src2: 0x${b.toHexString}")

            }
        }
    }
}