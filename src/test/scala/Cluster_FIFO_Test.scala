import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.util._
import Adder._
import ALU_Op._

object FIFO_Dut{
    def ALU(src1: Int, src2: Int, op: ALU_Op.Type): Int = {
        // 用case语句在软件层面模拟一个ALU
        op match{
            case ADD => src1 + src2
            case SUB => src1 - src2
            case AND => src1 & src2
            case OR => src1 | src2
            case XOR => src1 ^ src2
            case NOR => ~(src1 | src2)
            case SLL => src1 << (src2 & 0x1F)
            case SRL => src1 >>> (src2 & 0x1F)
            case SRA => src1 >> (src2 & 0x1F)
            case SLT => if(src1 < src2) 1 else 0
            case SLTU => if((src1.toLong & 0xFFFFFFFFL) < (src2.toLong & 0xFFFFFFFFL)) 1 else 0
            case LUI => src2
        }
    }
}

class FIFO_Dut(n: Int = 8) {
    // 用软件模拟一个FIFO
    val fifo = scala.collection.mutable.Queue[Int]()
    // 压入n个数据
    def push(data: Vector[Int]): Unit = {
        for (d <- data){
            fifo.enqueue(d)
        }
    }
    // 弹出n个数据
    def pop(n: Int): Vector[Int] = {
        var res = Vector[Int]()
        for (i <- 0 until n){
            if (fifo.isEmpty) return res
            res = res :+ fifo.dequeue()
        }
        res
    }
}

class FIFO_Test extends AnyFlatSpec with ChiselScalatestTester{
    behavior of "ALU"
    it should "pass" in {
        test(new Cluster_FIFO(UInt(32.W), 8, 4, 3, false))
        .withAnnotations(Seq(WriteVcdAnnotation))
        { c =>
            val values = ALU_Op.all
            val fifo = new FIFO_Dut(8)
            // push 6 data for both dut and ref
            val push_vec = Vector.fill(6)(Random.nextInt(0x7FFFFFFF))
            c.io.enq(0).bits.poke(push_vec(0).U)
            c.io.enq(1).bits.poke(push_vec(1).U)
            c.io.enq(2).bits.poke(push_vec(2).U)
            c.io.enq(3).bits.poke(push_vec(3).U)
            
            c.io.enq(0).valid.poke(true.B)
            c.io.enq(1).valid.poke(true.B)
            c.io.enq(2).valid.poke(true.B)
            c.io.enq(3).valid.poke(false.B)
            c.clock.step(1)
            c.io.enq(0).valid.poke(true.B)
            c.io.enq(1).valid.poke(true.B)
            c.io.enq(2).valid.poke(false.B)
            c.io.enq(3).valid.poke(false.B)
            c.clock.step(1)
            c.io.enq(0).valid.poke(true.B)
            c.io.enq(1).valid.poke(false.B)
            c.io.enq(2).valid.poke(false.B)
            c.io.enq(3).valid.poke(false.B)
            c.clock.step(1)
            // c.clock.step(1)
        }
    }
}