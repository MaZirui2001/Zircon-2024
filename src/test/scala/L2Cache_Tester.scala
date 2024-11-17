import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.util._
import CPU_Config.Cache._

class Test_Generator{
    // 生成n个在max和min之间的、指定长度的随机数
    def gen_addr(n: Int, min: Int, max: Int): List[Int] = {
        val res = List.fill(n)((Random.nextInt(max - min) + min))
        res
    }
    // 将测试写入文件
    def generate_tests(n: Int): Unit = {
        val writer = new java.io.PrintWriter("test.txt")
        // 生成icache访问有效性，范围0-1
        val icache_valid = List.fill(n)(Random.nextInt(2))
        // 生成icache的访问地址，范围0-1023
        val icache_addr = gen_addr(n, 0, 1023)
        // 生成icache的uncache信号，目前暂时全为0
        val icache_uncache = List.fill(n)(0)
        // 生成dcache的读有效性，范围0-1
        val dcache_read_valid = List.fill(n)(Random.nextInt(2))
        // 生成dcache的写有效性，注意只有在读有效性为1的时候才能写，范围0-1
        val dcache_write_valid = List.tabulate(n)(i => if(dcache_read_valid(i) == 1) Random.nextInt(2) else 0)
        // 生成dcache的地址，范围1024-2047
        val dcache_addr = gen_addr(n, 1024, 2047)
        // 生成dcache的uncache信号，目前暂时全为0
        val dcache_uncache = List.fill(n)(0)
        // 生成dcache的写数据，范围2^31-1
        val dcache_wdata = gen_addr(n, 0, 0x7fffffff)
        // 生成dcache的写长度，范围0-2
        val dcache_mype = gen_addr(n, 0, 3)

        // 将这些信号写入文件，每一组信号占一行，信号之间用空格分隔
        for(i <- 0 until n){
            writer.println(f"${icache_valid(i)} ${icache_addr(i)} ${icache_uncache(i)} ${dcache_read_valid(i)} ${dcache_write_valid(i)} ${dcache_addr(i)} ${dcache_uncache(i)} ${dcache_wdata(i)} ${dcache_mype(i)}")
        }
        writer.close()
    }
    // 从文件里读取测试
    def read_test(): Array[Array[UInt]] = {
        val source = scala.io.Source.fromFile("test.txt")
        val lines = source.getLines().toArray
        val res = Array.ofDim[UInt](lines.length, 9)
        for(i <- 0 until lines.length){
            val items = lines(i).split(" ")
            for(j <- 0 until 9){
                res(i)(j) = items(j).toInt.U
            }
        }
        res
    }

}


class L2Cache_Tester extends AnyFlatSpec with ChiselScalatestTester{
    val test_num = 10
    val memory = new AXI_Memory(true)
    val test_gen = new Test_Generator

    memory.initialize(2048, false)
    // test_gen.generate_tests(test_num)
    val tests = test_gen.read_test()

    behavior of "L2Cache"
    it should "pass" in {
        test(new L2Cache_Test).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation))
        { c =>
            var i_test_index = 0
            var d_test_index = 0
            var i = 0
            while((i_test_index < test_num || d_test_index < test_num ) && i < 2000){ 
                println(i_test_index, d_test_index)
                val w = memory.write(
                    c.io.axi.awaddr.peek(), 
                    c.io.axi.awlen.peek(), 
                    c.io.axi.awsize.peek(), 
                    c.io.axi.awburst.peek(), 
                    c.io.axi.wdata.peek(), 
                    c.io.axi.wstrb.peek(), 
                    c.io.axi.wlast.peek(), 
                    c.io.axi.awvalid.peek(), 
                    c.io.axi.wvalid.peek(),
                    c.io.axi.bready.peek(), 
                    i
                )
                val r = memory.read(
                    c.io.axi.araddr.peek(), 
                    c.io.axi.arlen.peek(), 
                    c.io.axi.arsize.peek(), 
                    c.io.axi.arburst.peek(), 
                    c.io.axi.arvalid.peek(), 
                    c.io.axi.rready.peek()
                )
                c.io.axi.arready.poke(r.arready)
                c.io.axi.awready.poke(w.awready)
                c.io.axi.bvalid.poke(w.bvalid)
                c.io.axi.rdata.poke(r.rdata)
                c.io.axi.rlast.poke(r.rlast)
                c.io.axi.rvalid.poke(r.rvalid)
                c.io.axi.wready.poke(w.wready)

                c.io.ic.rreq.poke(tests(i_test_index)(0))
                c.io.ic.paddr.poke(tests(i_test_index)(1))
                c.io.ic.uc_in.poke(tests(i_test_index)(2))
                c.io.dc.rreq.poke(tests(d_test_index)(3))
                c.io.dc.wreq.poke(tests(d_test_index)(4))
                c.io.dc.paddr_in.poke(tests(d_test_index)(5))
                c.io.dc.uc_in.poke(tests(d_test_index)(6))
                c.io.dc.wdata.poke(tests(d_test_index)(7))
                c.io.dc.mtype.poke(tests(d_test_index)(8))
                c.clock.step(1)
                if(!c.io.ic.miss.peek().litToBoolean){
                    if(i_test_index < test_num){
                        i_test_index += 1
                    }
                }
                if(!c.io.dc.miss.peek().litToBoolean){
                    if(d_test_index < test_num){
                        d_test_index += 1
                    }
                }
                i = i + 1
            }
        }
    }
}