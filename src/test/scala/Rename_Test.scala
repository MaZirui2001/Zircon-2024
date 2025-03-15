import chisel3._
import chiseltest._
import chiseltest.simulator.VerilatorFlags
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable.Queue
import scala.util._
import CPU_Config.Decode._
import CPU_Config.RegisterFile._
import Zircon_Util._
import os.write
import scala.collection.parallel.CollectionConverters._


case class Rename_Test_Item(var rd_vld: Int, var rd: Int, var rj: Int, var rk: Int, var flush: Int)
case class Simple_ROB_Item(var rd_vld: Int, var rd: Int, var prd: Int, var pprd: Int)

object Rename_Test_Config {
    val test_num = 20000
}

class Rename_Test_Generator {
    import Rename_Test_Config._
    private val rand = new Random()

    def generate_tests: Unit = {
        val testbenchDir = os.pwd / "testbench"
        val testbenchPath = testbenchDir / "rename_test.txt"
        // 如果目录不存在，则创建
        if (!os.exists(testbenchDir)) {
            os.makeDir(testbenchDir)
        }
        val writer = new java.io.PrintWriter(testbenchPath.toString)
        // 每一个测试包含ndecode个测试项，ndeocde在Decode中有定义
        val test_array = new Array[Rename_Test_Item](test_num * ndecode)

        val results = (0 until test_num * ndecode).map { i =>
            val item = Rename_Test_Item(0, 0, 0, 0, 0)

            item.flush = if(rand.nextInt(20) == 0) 1 else 0
            item.rd_vld = if(item.flush == 0) rand.nextInt(2) else 0
            // 寄存器编号：0-31，rd_vld为false时，rd必须为0，rd_vld为true时，rd在1-31之间随机
            item.rd = if (item.rd_vld != 0) rand.nextInt(31) + 1 else 0
            item.rj = rand.nextInt(32)
            item.rk = rand.nextInt(32)


            item
        }.toArray

        Array.copy(results, 0, test_array, 0, test_num * ndecode)

        // 写入文件，注意每ndeocde个测试项写在一行，每个数据中间空格分割
        for (i <- 0 until ndecode * test_num) {
            writer.println(
                f"${test_array(i).rd_vld} " +
                f"${test_array(i).rd} " +
                f"${test_array(i).rj} " +
                f"${test_array(i).rk} " +
                f"${test_array(i).flush} "
            )
        }

        writer.close()
    }

    def read_tests: Array[Rename_Test_Item] = {
        val projectRoot = os.Path(System.getProperty("user.dir"))  // 获取项目根目录
        val testbenchPath = (projectRoot / "testbench" / "rename_test.txt").toString
        val source = scala.io.Source.fromFile(testbenchPath)
        val lines = source.getLines().toArray
        val res = Array.fill(lines.length)(Rename_Test_Item(0, 0, 0, 0, 0))
        for (i <- 0 until lines.length) {
            val items = lines(i).split(" ")
            for (j <- 0 until items.length) items(j) = Integer.parseUnsignedInt(items(j), 10).toString
            res(i) = Rename_Test_Item(items(0).toInt, items(1).toInt, items(2).toInt, items(3).toInt, items(4).toInt)
        }
        res
    }
}

class Rename_Ref{
    import CPU_Config.Commit._
    val free_list = Array.tabulate(npreg)(i =>
        (i / ndecode) + 1 + (i % ndecode) * (npreg / ndecode)
    )
    var head = 0
    var tail = 0
    var cmt = 0
    val rename_table = Array.fill(nlreg)(0)
    val commit_table = Array.fill(nlreg)(0)

    def write(rd: Int, rd_vld: Boolean): Int = {
        if(!rd_vld) return 0
        val preg = free_list(head)
        rename_table(rd) = preg
        head = (head + 1) % npreg
        preg
    }

    def read(rs: Int): Int = {
        rename_table(rs)
    }

    def get_free_list: Array[Int] = {
        free_list
    }
    
    def commit(item: Simple_ROB_Item): Unit = {
        if(item.rd_vld == 0) return
        if(item.pprd != 0){
            free_list(tail) = item.pprd
            tail = (tail + 1) % npreg
            cmt = (cmt + 1) % npreg
        }
        commit_table(item.rd) = item.prd
    }
    def flush(): Unit = {
        head = cmt
        Array.copy(commit_table, 0, rename_table, 0, rename_table.length)
    }
}

class Rename_Test extends AnyFlatSpec with ChiselScalatestTester {
    import Rename_Test_Config._
    import CPU_Config.Commit._
    val test_generator = new Rename_Test_Generator
    println("generating tests ...")
    test_generator.generate_tests
    println("loading tests from file ...")
    val tests = test_generator.read_tests
    println("start testing ...")
    behavior of "Rename"
    it should "pass" in {
        test(new Rename).withAnnotations(Seq(
            VerilatorBackendAnnotation,
            WriteVcdAnnotation,
            VerilatorFlags(Seq("-threads", "2"))
        )) { c =>
            val ref = new Rename_Ref
            val rob = Queue[Simple_ROB_Item]((Simple_ROB_Item(0, 0, 0, 0)))
            var index = 0
            val end = test_num * ndecode - ndecode
            val rand = new Random()
            val PRINT_INTERVAL = test_num / 10
            while(index < end) {
                // 比较free_list和rename_table
                for(i <- 0 until npreg){
                    c.io.dif.flst.free_list(i).expect(ref.get_free_list(i), 
                        f"idx: ${index}, free_list: ${i}"
                    )
                }
                for(i <- 0 until nlreg){
                    c.io.dif.srat.rename_table(i).expect(ref.read(i), 
                        f"idx: ${index}, rename_table: ${i}"
                    )
                }
                c.io.cmt.flst.enq.foreach(_.valid.poke(false))
                c.io.cmt.srat.rd_vld.foreach(_.poke(false))
                if(tests(index).flush == 1){
                    ref.flush()
                    rob.clear()
                    c.io.cmt.flst.flush.poke(true)
                    c.io.cmt.srat.flush.poke(true)
                    index += 1
                } else{
                    c.io.cmt.flst.flush.poke(false)
                    c.io.cmt.srat.flush.poke(false)
                    // 随机提交
                    val commit_en = rand.nextBoolean()
                    if(commit_en){
                        val commit_num = rand.nextInt(ncommit + 1)
                        for(i <- 0 until commit_num){
                            if(rob.nonEmpty){
                                val item = rob.dequeue()
                                ref.commit(item)
                                c.io.cmt.flst.enq(i).valid.poke(item.rd_vld != 0)
                                c.io.cmt.flst.enq(i).bits.poke(item.pprd)
                                c.io.cmt.srat.rd_vld(i).poke(item.rd_vld != 0)
                                c.io.cmt.srat.rd(i).poke(item.rd)
                                c.io.cmt.srat.prd(i).poke(item.prd)
                            }
                        }
                    }
                    if(c.io.fte.rinfo(0).ready.peek().litToBoolean){    
                        // 1. 随机选定本次测试的数量，范围0-ndecode
                        val ntest = rand.nextInt(ndecode)
                        // 2. 读取本次测试的测试项并激励
                        for(i <- 0 until ndecode){
                            if(i < ntest){
                                c.io.fte.rinfo(i).valid.poke(true)
                                c.io.fte.rinfo(i).bits.rd_vld.poke(tests(index + i).rd_vld)
                                c.io.fte.rinfo(i).bits.rd.poke(tests(index + i).rd)
                                c.io.fte.rinfo(i).bits.rj.poke(tests(index + i).rj)
                                c.io.fte.rinfo(i).bits.rk.poke(tests(index + i).rk)
                            } else {
                                c.io.fte.rinfo(i).valid.poke(false)
                            }
                        }
                        // 3. 逐个比较并写入新映射，并将老映射写入pprd_rob
                        for(i <- 0 until ntest){
                            c.io.fte.pinfo(i).pprd.expect(ref.read(tests(index+i).rd), 
                                f"idx: ${index + i}, rd: ${tests(index + i).rd}"
                            )
                            c.io.fte.pinfo(i).prj.expect(ref.read(tests(index+i).rj), 
                                f"idx: ${index + i}, rj: ${tests(index + i).rj}"
                            )
                            c.io.fte.pinfo(i).prk.expect(ref.read(tests(index+i).rk), 
                                f"idx: ${index + i}, rk: ${tests(index + i).rk}"
                            )
                            val pprd = ref.read(tests(index + i).rd)
                            val preg = ref.write(tests(index + i).rd, tests(index + i).rd_vld != 0)
                            rob.enqueue(Simple_ROB_Item(tests(index + i).rd_vld, tests(index + i).rd, preg, pprd))
                            c.io.fte.pinfo(i).prd.expect(preg, 
                                f"idx: ${index + i}, rd: ${tests(index + i).rd}"
                            )
                        }
                        index += ntest
                    }
                }
                c.clock.step(1)
                if(index % PRINT_INTERVAL == 0){
                    print(s"\rTest: ${index * 100 / (test_num * ndecode)}%")
                    Console.flush()
                }
            }
            println("\nTest completed!")
        }
    }
    
}
