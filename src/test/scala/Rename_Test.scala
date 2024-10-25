import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.util._
class Rename_Test extends AnyFlatSpec with ChiselScalatestTester{
    val rn_w = 3
    behavior of "Rename"
    it should "pass" in {
        test(new SRat(rn_w))
        .withAnnotations(Seq(WriteVcdAnnotation))
        { c =>
            for(i <- 0 until rn_w){
                c.io.rd_vld(i).poke(true.B)
                c.io.rd(i).poke((i+3).U)
                c.io.prd(i).poke((i+6).U)
            }
            c.clock.step(1)
            c.io.arat.zipWithIndex.foreach{ case (a, i) => a.poke((31-i).U)}
            c.io.rdrct_en.poke(true.B)
            c.clock.step(1)
            while(c.io.srat_rdy.peek().litToBoolean == false){
                c.clock.step(1)
            }
            c.io.dbg_srat.zipWithIndex.foreach{ case (d, i) => d.expect((31-i).U, s"i: $i")}
        }
    }
}