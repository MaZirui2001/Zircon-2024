import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable.Queue
import scala.util._
import spire.math.UInt
import Multiply._
import EXE_Op._

class DIV_Ref{
    def calculate(a: Long, b: Long, op: Int): Long = {
        op match {
            case 4 => a.toInt / b.toInt
            case 5 => (UInt(a) / UInt(b)).toLong
            case 6 => a.toInt % b.toInt
            case 7 => (UInt(a) % UInt(b)).toLong
        }
    }
}

case class SRT2_Test_Item(var a: Long, var b: Long, var op: Int)


class SRT2_Test extends AnyFlatSpec with ChiselScalatestTester{
    behavior of "DIV"
    it should "pass" in {
        test(new SRT2)
        .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation))
        { c =>
            val ref = new DIV_Ref()
            val rand1 = new Random()
            val rand2 = new Random()
            val rand3 = new Random()
            var ref_res = 0L
            var a = 0L
            var b = 0L
            var op = 0
            var i = 0
            val q = Queue[SRT2_Test_Item]()
            while(i < 100){
                if(c.io.ready.peek().asBool.litToBoolean){
                    val item = q.dequeue()
                    ref_res = ref.calculate(item.a, item.b, item.op)
                    println(s"a: ${item.a.toHexString}, b: ${item.b.toHexString}, op: ${item.op}, ref: ${(ref_res & 0xFFFFFFFFL).toHexString}")
                    c.io.res.expect(ref_res & 0xFFFFFFFFL, s"a: ${item.a.toHexString}, b: ${item.b.toHexString}, op: ${item.op}, res: ${c.io.res.peek().toString}, ref: ${ref_res.toHexString}")
                    i += 1
                }
                if(!c.io.busy.peek().asBool.litToBoolean){
                    a = rand1.nextLong(0x100000000L)
                    b = rand2.nextLong(0x10000000L)
                    op = rand3.nextInt(4) + 4
                    q.enqueue(SRT2_Test_Item(a, b, op))
                    c.io.src1.poke(a.U)
                    c.io.src2.poke(b.U)
                    c.io.op.poke(op.U)
                }
                c.clock.step(1)
                
            }
        }
    }
}