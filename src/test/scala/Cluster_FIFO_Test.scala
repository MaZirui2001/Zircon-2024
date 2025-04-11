import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.util._


class FIFODut(n: Int = 8) {
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

class FIFOTest extends AnyFlatSpec with ChiselScalatestTester{
    behavior of "FIFO"
    it should "pass" in {
        test(new ClusterIndexFIFO(UInt(32.W), 16, 3, 4, 0, 0))
        .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation))
        { c =>
            val fifo = new FIFODut(8)
            // push 6 data for both dut and ref
            var data = 1;
            for (i <- 0 until 20){
                if(c.io.enq(0).ready.peek().litToBoolean && i < 10){
                    c.io.enq(0).valid.poke(true.B)
                    c.io.enq(0).bits.poke(data.U)
                    data += 1
                    c.io.enq(1).valid.poke(Random.nextBoolean().B)
                    c.io.enq(1).bits.poke(data.U)
                    if(c.io.enq(1).valid.peek().litToBoolean){
                        data += 1
                        c.io.enq(2).valid.poke(Random.nextBoolean().B)
                        c.io.enq(2).bits.poke(data.U)
                    }else{
                        c.io.enq(2).valid.poke(false.B)
                    }
                    if(c.io.enq(2).valid.peek().litToBoolean){
                        data += 1
                    }
                    // c.io.enq(3).valid.poke(Random.nextBoolean().B)
                    // c.io.enq(3).bits.poke(data.U)
                    // if(c.io.enq(3).valid.peek().litToBoolean){
                    // data += 1
                    // }
                }
                else if(i >= 10){
                    c.io.enq(0).valid.poke(false.B)
                    c.io.enq(1).valid.poke(false.B)
                    c.io.enq(2).valid.poke(false.B)
                    // c.io.enq(3).valid.poke(false.B)
                }
                c.io.deq(0).ready.poke(true.B)
                c.io.deq(1).ready.poke(Random.nextBoolean().B)
                if(c.io.deq(1).ready.peek().litToBoolean){
                    c.io.deq(2).ready.poke(Random.nextBoolean().B)
                }else{
                    c.io.deq(2).ready.poke(false.B)
                }
                if(c.io.deq(2).ready.peek().litToBoolean){
                    c.io.deq(3).ready.poke(Random.nextBoolean().B)
                }
                else{
                    c.io.deq(3).ready.poke(false.B)
                }
                c.clock.step(1)


            }
        }
    }
}