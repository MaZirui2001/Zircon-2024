import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable.Queue
import scala.util._
import CPU_Config.Cache._
import Zircon_Util._
import os.write

object Cache_Test_Config {
    val total_space             = 4096
    val icache_space_start      = 0
    val icache_space_end        = total_space / 2
    val dcache_space_start      = total_space / 2
    val dcache_space_end        = total_space
    val test_num                = 32768
}

case class ICache_Test_Item()
case class DCache_Test_Item(var rreq: Int, var mtype: Int, var is_latest: Int, var wreq: Int, var wdata: BigInt, var vaddr: BigInt)
case class Cache_Test_Item(var ic: ICache_Test_Item, var dc: DCache_Test_Item)

class Cache_Test_Generator {
    import Cache_Test_Config._

    def generate_tests: Unit = {
        val writer = new java.io.PrintWriter("testbench/cachetest.txt")
        var test_list = List.fill(test_num)(Cache_Test_Item(ICache_Test_Item(), DCache_Test_Item(0, 0, 0, 0, 0, 0)))
        // generate test items
        for(i <- 0 until test_num){
            // dcache rreq、wwreq: 0-1
            val req = Random.nextInt(2) + 1
            test_list(i).dc.rreq = req % 2
            test_list(i).dc.wreq = req / 2
            // dcache vaddr: range in dcache space
            test_list(i).dc.vaddr = Random.nextInt(dcache_space_end - dcache_space_start) + dcache_space_start
            // dcache mtype: 如果vaddr最低两位是00，则mtype可以为0、1、2、4、5；如果vaddr最低两位是01，则mtype可以为0、4；如果vaddr最低两位是10，则mtype可以为0、1、4, 5；如果vaddr最低两位是11，则mtype可以为0、4
            // test_list(i).dc.mtype = List(0, 1, 2, 4, 5)(Random.nextInt(5))
            test_list(i).dc.mtype = (test_list(i).dc.vaddr & 0x3).toInt match {
                case 0 => List(0, 1, 2, 4, 5)(Random.nextInt(5))
                case 1 => List(0, 4)(Random.nextInt(2))
                case 2 => List(0, 1, 4, 5)(Random.nextInt(4))
                case 3 => List(0, 4)(Random.nextInt(2))
            }
            // dcache is_latest: 1
            test_list(i).dc.is_latest = 1
            // dcache wdata: 0-2^31-1
            test_list(i).dc.wdata = BigInt(Random.nextLong(0xFFFFFFFFL + 1) & 0xFFFFFFFFL)

        }
        // write to file
        for(item <- test_list){
            writer.println(
                f"${item.dc.rreq}%x " +
                f"${item.dc.mtype}%x " +
                f"${item.dc.is_latest}%x " +
                f"${item.dc.wreq}%x " +
                f"${item.dc.wdata}%x " +
                f"${item.dc.vaddr}%x"
            )
        }
        writer.close()
    }

    def read_tests(): Array[Cache_Test_Item] = {
        val source = scala.io.Source.fromFile("testbench/cachetest.txt")
        val lines = source.getLines().toArray
        val res = Array.fill(lines.length)(Cache_Test_Item(ICache_Test_Item(), DCache_Test_Item(0, 0, 0, 0, 0, 0)))
        for(i <- 0 until lines.length){
            val items = lines(i).split(" ")
            // 这里读入的是十六进制数字，需要转换成十进制
            for(j <- 0 until items.length) items(j) = Integer.parseUnsignedInt(items(j), 16).toString
            val ic = ICache_Test_Item()
            val dc = DCache_Test_Item(items(0).toInt, items(1).toInt, items(2).toInt, items(3).toInt, BigInt(items(4).toLong & 0xFFFFFFFFL), items(5).toInt)
            res(i) = Cache_Test_Item(ic, dc)
        }
        res
    }
}

class Cache_Tester extends AnyFlatSpec with ChiselScalatestTester{
    val memory = new AXI_Memory(true)
    val test_generator = new Cache_Test_Generator
    import Cache_Test_Config._
    println("initializing memory ...")
    memory.initialize(total_space, false)
    println("generating tests ...")
    test_generator.generate_tests
    println("loading tests from file ...")
    val tests = test_generator.read_tests()
    println("start testing ...")
    behavior of "Cache"
    it should "pass" in {   
        test(new DCache_Test).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation))
        { c =>
            var d_index = 0
            var i = 0
            val d_req_q = Queue[DCache_Test_Item]()
            val d_cmt_q = Queue[DCache_Test_Item]()
            var random_commit_delay = 0
            while(d_index < test_num){
                // read and write pacakge get from axi
                val w = memory.write(c.io.axi.peek(), d_index)
                val r = memory.read(c.io.axi.peek())
                // write and read pacakge send to axi
                c.io.axi.arready.poke(r.arready)
                c.io.axi.awready.poke(w.awready)
                c.io.axi.bvalid.poke(w.bvalid)
                c.io.axi.rdata.poke(r.rdata)
                c.io.axi.rlast.poke(r.rlast)
                c.io.axi.rvalid.poke(r.rvalid)
                c.io.axi.wready.poke(w.wready)

                // motivation
                val test = tests(d_index)
                c.io.d_pp.rreq.poke((if(c.io.d_pp.sb_full.peek().litToBoolean) 0 else test.dc.rreq))
                c.io.d_pp.mtype.poke(test.dc.mtype)
                c.io.d_pp.is_latest.poke(test.dc.is_latest)
                c.io.d_pp.wreq.poke((if(c.io.d_pp.sb_full.peek().litToBoolean) 0 else test.dc.wreq))
                c.io.d_pp.wdata.poke(test.dc.wdata)
                c.io.d_pp.vaddr.poke(test.dc.vaddr)
                if(d_index > 0){
                    // simulate the mmu behavior
                    val test_last = tests(d_index - 1)
                    c.io.d_mmu.paddr.poke(test_last.dc.vaddr)
                    c.io.d_mmu.uncache.poke(false.B)
                }
                // random delay
                random_commit_delay = (if(random_commit_delay > 0){ random_commit_delay - 1 } else{ Random.nextInt(4) })
                if(random_commit_delay == 0 && d_cmt_q.nonEmpty){
                    val req = d_cmt_q.dequeue()
                    if(req.wreq == 1){
                        c.io.d_cmt.st_cmt.poke(true.B)
                    } else {
                        c.io.d_cmt.st_cmt.poke(false.B)
                    }
                } else {
                    c.io.d_cmt.st_cmt.poke(false.B)
                }
                // debug queue 
                if(!c.io.d_pp.miss.peek().litToBoolean && !c.io.d_pp.sb_full.peek().litToBoolean){
                    if(c.io.d_pp.rreq.peek().litToBoolean || c.io.d_pp.wreq.peek().litToBoolean){
                        d_req_q.enqueue(test.dc)
                    }
                    if(d_index < test_num){
                        d_index += 1
                    }
                }
                // check the response
                if(c.io.d_pp.rrsp.peek().litToBoolean || c.io.d_pp.wrsp.peek().litToBoolean){
                    val req = d_req_q.dequeue()
                    // println(f"${req.vaddr}%x")
                    d_cmt_q.enqueue(req)
                    // debug
                    if(c.io.d_pp.wrsp.peek().litToBoolean){
                        val addr_align = (req.vaddr >> 2) << 2
                        val wdata = req.wdata << ((req.vaddr.toInt & 0x3) << 3)
                        val wstrb = ((1 << (1 << (req.mtype & 0x3))) - 1) << (req.vaddr.toInt & 0x3)
                        memory.debug_write(addr_align.toInt, wdata.toInt, wstrb, d_index-1)
                    } else if(c.io.d_pp.rrsp.peek().litToBoolean){
                        var data = BigInt("0" * 32, 2)
                        data = memory.debug_read(req.vaddr.toInt)._1 & 0xFFFFFFFFL
                        data = req.mtype match {
                            case 0 => if((data & 0x80) == 0) data & 0xFF else data | 0xFFFFFF00
                            case 1 => if((data & 0x8000) == 0) data & 0xFFFF else data | 0xFFFF0000
                            case 2 => data
                            case 4 => data & 0xFF
                            case 5 => data & 0xFFFF
                            case _ => data
                        }
                        c.io.d_pp.rdata.expect(
                            data & 0xFFFFFFFFL, f"idx: ${d_index}, addr: ${req.vaddr}%x, last write: 1. ${memory.debug_read(req.vaddr.toInt)._2}, " +
                                                                    f"2. ${memory.debug_read(req.vaddr.toInt + 1)._2}, " +
                                                                    f"3. ${memory.debug_read(req.vaddr.toInt + 2)._2}, "  +
                                                                    f"4. ${memory.debug_read(req.vaddr.toInt + 3)._2}"
                        )
                    }
                }
                c.clock.step(1)
                i += 1
            }
            println(s"dcache test: $d_index, total cycles: $i")
        }
    }
}