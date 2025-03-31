// import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable.Queue
import scala.util._
import CPU_Config.Cache._
import Zircon_Util._
import os.write
import spire.math.UInt

object L2_Test_Config {
    val total_space             = 4096
    val icache_space_start      = 0
    val icache_space_end        = total_space / 2
    val dcache_space_start      = total_space / 2
    val dcache_space_end        = total_space
    val test_num                = 32768
}
case class L2_ICache_Test_Item(var rreq: Int, var paddr: Int, var uncache: Int)
case class L2_DCache_Test_Item(var rreq: Int, var wreq: Int, var paddr: Int, var uncache: Int, var wdata: BigInt, var mtype: Int)
case class L2_Test_Item(var ic: L2_ICache_Test_Item, var dc: L2_DCache_Test_Item)

class L2_Test_Generator{
    import L2_Test_Config._

    // 将测试写入文件
    def generate_tests: Unit = {
        val writer = new java.io.PrintWriter("testbench/l2test.txt")
        var test_list = List.fill(test_num)(L2_Test_Item(L2_ICache_Test_Item(0, 0, 0), L2_DCache_Test_Item(0, 0, 0, 0, 0, 0)))
        // 生成icache访问有效性，间隔有效性为1
        test_list.zipWithIndex.map{ case (item, i) => item.ic.rreq = if(i % 2 == 0) 1 else 0}
        // 生成icache的访问地址，范围0-1023
        test_list.zipWithIndex.map{ case (item, i) => item.ic.paddr = Random.nextInt(icache_space_end - icache_space_start) + icache_space_start}
        // 生成icache的uncache信号，目前暂时全为0
        test_list.zipWithIndex.map{ case (item, i) => item.ic.uncache = 0}
        // 生成dcache的读有效性，间隔有效性为1
        test_list.zipWithIndex.map{ case (item, i) => item.dc.rreq = if(i % 2 == 0) 1 else 0}
        // 生成dcache的写有效性，注意只有在读有效性为1的时候才能写，范围0-1
        test_list.zipWithIndex.map{ case (item, i) => item.dc.wreq = if(item.dc.rreq == 1) Random.nextInt(2) else 0}
        // 生成dcache的地址，范围1024-2047
        test_list.zipWithIndex.map{ case (item, i) => item.dc.paddr = Random.nextInt(dcache_space_end - dcache_space_start) + dcache_space_start}
        // 生成dcache的uncache信号，目前暂时全为0
        test_list.zipWithIndex.map{ case (item, i) => item.dc.uncache = 0}
        // 生成dcache的写数据，范围2^31-1
        test_list.zipWithIndex.map{ case (item, i) => item.dc.wdata = BigInt(Random.nextLong(0xFFFFFFFFL + 1) & 0xFFFFFFFFL)}
        // 生成dcache的mtype，范围0-3
        test_list.zipWithIndex.map{ case (item, i) => item.dc.mtype = (item.dc.paddr & 3) match{
            case 0 => Random.nextInt(3)
            case 1 => 0
            case 2 => Random.nextInt(2)
            case 3 => 0
        }}
        // 将这些信号写入文件，每一组信号占一行
        for(i <- 0 until test_num){
            writer.println(
                f"${test_list(i).ic.rreq}%x " +
                f"${test_list(i).ic.paddr}%x " +
                f"${test_list(i).ic.uncache}%x " +
                f"${test_list(i).dc.rreq}%x " +
                f"${test_list(i).dc.wreq}%x " +
                f"${test_list(i).dc.paddr}%x " +
                f"${test_list(i).dc.uncache}%x " +
                f"${test_list(i).dc.wdata}%x " +
                f"${test_list(i).dc.mtype}%x"
            )
        }
        writer.close()
    }
    // 从文件里读取测试
    def read_test(): Array[L2_Test_Item] = {
        val source = scala.io.Source.fromFile("testbench/l2test.txt")
        val lines = source.getLines().toArray
        val res = Array.fill(lines.length)(L2_Test_Item(L2_ICache_Test_Item(0, 0, 0), L2_DCache_Test_Item(0, 0, 0, 0, 0, 0)))
        for(i <- 0 until lines.length){
            val items = lines(i).split(" ")
            // 这里读入的是十六进制数字，需要转换成十进制
            for(j <- 0 until items.length) items(j) = Integer.parseUnsignedInt(items(j), 16).toString
            val ic = L2_ICache_Test_Item(items(0).toInt, items(1).toInt, items(2).toInt)
            val dc = L2_DCache_Test_Item(items(3).toInt, items(4).toInt, items(5).toInt, items(6).toInt, BigInt(items(7).toLong & 0xFFFFFFFFL), items(8).toInt)
            res(i) = L2_Test_Item(ic, dc)
        }
        res
    }
}


class L2Cache_Tester extends AnyFlatSpec with ChiselScalatestTester{
    val test_num = 32768
    val memory = new AXI_Memory(true)
    val test_gen = new L2_Test_Generator
    import L2_Test_Config._
    memory.initialize(total_space, false)
    // test_gen.generate_tests
    val tests = test_gen.read_test()
    println("start")

    behavior of "L2Cache"
    it should "pass" in {
        test(new L2Cache_Test).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation))
        { c =>
            var i_test_index = 0
            var d_test_index = 0
            var i = 0
            val icache_req_q = Queue[L2_ICache_Test_Item]()
            val dcache_req_q = Queue[L2_DCache_Test_Item]()
            while(i_test_index < test_num || d_test_index < test_num){ 
                // read and write pacakge get from axi
                val w = memory.write(
                    c.io.axi.peek(),
                    d_test_index
                )
                val r = memory.read(
                    c.io.axi.peek()
                )
                // write and read pacakge send to axi
                c.io.axi.arready.poke(r.arready)
                c.io.axi.awready.poke(w.awready)
                c.io.axi.bvalid.poke(w.bvalid)
                c.io.axi.rdata.poke(r.rdata.toInt)
                c.io.axi.rlast.poke(r.rlast)
                c.io.axi.rvalid.poke(r.rvalid)
                c.io.axi.wready.poke(w.wready)

                // motivation
                if(i_test_index < test_num){
                    c.io.ic.rreq.poke(tests(i_test_index).ic.rreq)
                    c.io.ic.paddr.poke(tests(i_test_index).ic.paddr)
                    c.io.ic.uncache.poke(tests(i_test_index).ic.uncache)
                }
                if(d_test_index < test_num){
                    c.io.dc.rreq.poke(tests(d_test_index).dc.rreq)
                    c.io.dc.wreq.poke(tests(d_test_index).dc.wreq)
                    c.io.dc.paddr.poke(tests(d_test_index).dc.paddr)
                    c.io.dc.uncache.poke(tests(d_test_index).dc.uncache)
                    c.io.dc.wdata.poke(tests(d_test_index).dc.wdata)
                    c.io.dc.mtype.poke(tests(d_test_index).dc.mtype)
                }

                // reference and update
                if(!c.io.ic.miss.peek().litToBoolean){
                    if(c.io.ic.rreq.peek().litToBoolean){
                        icache_req_q.enqueue(tests(i_test_index).ic)
                    }
                    if(i_test_index < test_num){
                        i_test_index += 1
                    }
                }
                if(!c.io.dc.miss.peek().litToBoolean){
                    if(c.io.dc.rreq.peek().litToBoolean){
                        dcache_req_q.enqueue(tests(d_test_index).dc)
                    }
                    if(d_test_index < test_num){
                        d_test_index += 1
                    }
                }
                // check icache result
                if(c.io.ic.rrsp.peek().litToBoolean){
                    val item = icache_req_q.dequeue()
                    val paddr = item.paddr
                    val paddr_debug = (paddr >> l1_offset) << l1_offset
                    var data = BigInt("0" * (32 * (l1_offset - 1)), 2)
                    for(j <- l1_offset - 2 until -1 by -1){
                        data = (data << 32) | (memory.debug_read(paddr_debug + 4 * j)._1 & 0xFFFFFFFFL)
                    }
                    var rmask = BigInt("1" * (32 * (l1_offset - 1)), 2)
                    c.io.ic.rline.expect(data & rmask, f"addr: ${paddr_debug}%x, last write: ${memory.debug_read(paddr_debug)._2}")
                }
                // check dcache result
                if(c.io.dc.rrsp.peek().litToBoolean || c.io.dc.wrsp.peek().litToBoolean){
                    val item = dcache_req_q.dequeue()
                    val paddr = item.paddr
                    val paddr_debug = (paddr >> l1_offset) << l1_offset
                    // write: debug write to the ref memory
                    if(c.io.dc.wrsp.peek().litToBoolean){
                        val paddr_align = (paddr >> 2) << 2
                        val wdata = item.wdata << ((paddr & 0x3) << 3)
                        val wstrb = ((1 << (1 << item.mtype)) - 1) << (paddr & 0x3)
                        memory.debug_write(paddr_align, wdata.toInt, wstrb, d_test_index)
                    }
                    var data = BigInt("0" * (32 * (l1_offset - 1)), 2)
                    for(j <- l1_offset - 2 until -1 by -1){
                        data = (data << 32) | (memory.debug_read(paddr_debug + 4 * j)._1 & 0xFFFFFFFFL)
                    }
                    var rmask = BigInt("1" * (32 * (l1_offset - 1)), 2)
                    c.io.dc.rline.expect(data & rmask, f"addr: ${paddr_debug}%x, last write: 1. ${memory.debug_read(paddr_debug)._2}, " +
                                                                                           f"2. ${memory.debug_read(paddr_debug + 1)._2}, " +
                                                                                           f"3. ${memory.debug_read(paddr_debug + 2)._2}, "  +
                                                                                           f"4. ${memory.debug_read(paddr_debug + 3)._2}")
                }

                c.clock.step(1)
                i = i + 1
            }
            println(s"icache test: $i_test_index, dcache test: $d_test_index, total cycles: $i")
        }
    }
}