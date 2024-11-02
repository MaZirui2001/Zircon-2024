import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.util._
import Adder._
import ALU_BR_Op._

object FIFO_Dut{
    def ALU(src1: Int, src2: Int, op: ALU_BR_Op.Type): Int = {
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
    behavior of "FIFO"
    it should "pass" in {
        test(new Cluster_FIFO(UInt(32.W), 16, 4, 3, false))
        .withAnnotations(Seq(WriteVcdAnnotation))
        { c =>
            val values = ALU_BR_Op.all
            val fifo = new FIFO_Dut(8)
            // push 6 data for both dut and ref
            var data = 1;
            for (i <- 0 until 20){
                if(c.io.enq(0).ready.peek().litToBoolean && i < 10){
                    c.io.enq(0).valid.poke(true.B)
                    c.io.enq(0).bits.poke(data.U)
                    data += 1
                    c.io.enq(1).valid.poke(true.B)
                    c.io.enq(1).bits.poke(data.U)
                    data += 1
                    c.io.enq(2).valid.poke(true.B)
                    c.io.enq(2).bits.poke(data.U)
                    data += 1
                    c.io.enq(3).valid.poke(Random.nextBoolean().B)
                    c.io.enq(3).bits.poke(data.U)
                    if(c.io.enq(3).valid.peek().litToBoolean){
                    data += 1
                    }
                }
                else if(i >= 10){
                    c.io.enq(0).valid.poke(false.B)
                    c.io.enq(1).valid.poke(false.B)
                    c.io.enq(2).valid.poke(false.B)
                    c.io.enq(3).valid.poke(false.B)
                }
                c.io.deq(0).ready.poke(true.B)
                c.io.deq(1).ready.poke(Random.nextBoolean().B)
                if(c.io.deq(1).ready.peek().litToBoolean){
                    c.io.deq(2).ready.poke(Random.nextBoolean().B)
                }else{
                    c.io.deq(2).ready.poke(false.B)
                }
                c.clock.step(1)


            }
        }
    }
}