// import chisel3._
// import chiseltest._
// import chiseltest.simulator.VerilatorFlags
// import org.scalatest.flatspec.AnyFlatSpec
// import scala.collection.mutable.Queue
// import scala.util._
// import ZirconConfig.Decode._
// import ZirconConfig.RegisterFile._
// import ZirconUtil._
// import os.write
// import scala.collection.parallel.CollectionConverters._


// case class RenameTestItem(var rdVld: Int, var rd: Int, var rj: Int, var rk: Int, var flush: Int)
// case class SimpleROBItem(var rdVld: Int, var rd: Int, var prd: Int, var pprd: Int)

// object RenameTestConfig {
//     val testNum = 20000
// }

// class RenameTestGenerator {
//     import RenameTestConfig._
//     private val rand = new Random()

//     def generateTests: Unit = {
//         val testbenchDir = os.pwd / "testbench"
//         val testbenchPath = testbenchDir / "renameTest.txt"
//         // 如果目录不存在，则创建
//         if (!os.exists(testbenchDir)) {
//             os.makeDir(testbenchDir)
//         }
//         val writer = new java.io.PrintWriter(testbenchPath.toString)
//         // 每一个测试包含ndecode个测试项，ndeocde在Decode中有定义
//         val testArray = new Array[RenameTestItem](testNum * ndcd)

//         val results = (0 until testNum * ndcd).map { i =>
//             val item = RenameTestItem(0, 0, 0, 0, 0)

//             item.flush = if(rand.nextInt(20) == 0) 1 else 0
//             item.rdVld = if(item.flush == 0) rand.nextInt(2) else 0
//             // 寄存器编号：0-31，rdVld为false时，rd必须为0，rdVld为true时，rd在1-31之间随机
//             item.rd = if (item.rdVld != 0) rand.nextInt(31) + 1 else 0
//             item.rj = rand.nextInt(32)
//             item.rk = rand.nextInt(32)


//             item
//         }.toArray

//         Array.copy(results, 0, testArray, 0, testNum * ndcd)

//         // 写入文件，注意每ndeocde个测试项写在一行，每个数据中间空格分割
//         for (i <- 0 until ndcd * testNum) {
//             writer.println(
//                 f"${testArray(i).rdVld} " +
//                 f"${testArray(i).rd} " +
//                 f"${testArray(i).rj} " +
//                 f"${testArray(i).rk} " +
//                 f"${testArray(i).flush} "
//             )
//         }

//         writer.close()
//     }

//     def readTests: Array[RenameTestItem] = {
//         val projectRoot = os.Path(System.getProperty("user.dir"))  // 获取项目根目录
//         val testbenchPath = (projectRoot / "testbench" / "renameTest.txt").toString
//         val source = scala.io.Source.fromFile(testbenchPath)
//         val lines = source.getLines().toArray
//         val res = Array.fill(lines.length)(RenameTestItem(0, 0, 0, 0, 0))
//         for (i <- 0 until lines.length) {
//             val items = lines(i).split(" ")
//             for (j <- 0 until items.length) items(j) = Integer.parseUnsignedInt(items(j), 10).toString
//             res(i) = RenameTestItem(items(0).toInt, items(1).toInt, items(2).toInt, items(3).toInt, items(4).toInt)
//         }
//         res
//     }
// }

// class RenameRef{
//     import ZirconConfig.Commit._
//     val fList = Array.tabulate(npreg)(i =>
//         (i / ndcd) + 1 + (i % ndcd) * (npreg / ndcd)
//     )
//     var head = 0
//     var tail = 0
//     var cmt = 0
//     val renameTable = Array.fill(nlreg)(0)
//     val commitTable = Array.fill(nlreg)(0)

//     def write(rd: Int, rdVld: Boolean): Int = {
//         if(!rdVld) return 0
//         val preg = fList(head)
//         renameTable(rd) = preg
//         head = (head + 1) % npreg
//         preg
//     }

//     def read(rs: Int): Int = {
//         renameTable(rs)
//     }

//     def getFreeList: Array[Int] = {
//         fList
//     }
    
//     def commit(item: SimpleROBItem): Unit = {
//         if(item.rdVld == 0) return
//         if(item.pprd != 0){
//             fList(tail) = item.pprd
//             tail = (tail + 1) % npreg
//             cmt = (cmt + 1) % npreg
//         }
//         commitTable(item.rd) = item.prd
//     }
//     def flush(): Unit = {
//         head = cmt
//         Array.copy(commitTable, 0, renameTable, 0, renameTable.length)
//     }
// }

// class RenameTest extends AnyFlatSpec with ChiselScalatestTester {
//     import RenameTestConfig._
//     import ZirconConfig.Commit._
//     val testGenerator = new RenameTestGenerator
//     println("generating tests ...")
//     testGenerator.generateTests
//     println("loading tests from file ...")
//     val tests = testGenerator.readTests
//     println("start testing ...")
//     behavior of "Rename"
//     it should "pass" in {
//         test(new Rename).withAnnotations(Seq(
//             VerilatorBackendAnnotation,
//             WriteVcdAnnotation,
//             VerilatorFlags(Seq("-threads", "2"))
//         )) { c =>
//             val ref = new RenameRef
//             val rob = Queue[SimpleROBItem]((SimpleROBItem(0, 0, 0, 0)))
//             var index = 0
//             val end = testNum * ndcd - ndcd
//             val rand = new Random()
//             val PRINTINTERVAL = testNum / 10
//             while(index < end) {
//                 // 比较fList和renameTable
//                 for(i <- 0 until npreg){
//                     c.io.dif.fList.fList(i).expect(ref.getFreeList(i), 
//                         f"idx: ${index}, fList: ${i}"
//                     )
//                 }
//                 for(i <- 0 until nlreg){
//                     c.io.dif.srat.renameTable(i).expect(ref.read(i), 
//                         f"idx: ${index}, renameTable: ${i}"
//                     )
//                 }
//                 c.io.cmt.fList.enq.foreach(_.valid.poke(false))
//                 c.io.cmt.srat.rdVld.foreach(_.poke(false))
//                 if(tests(index).flush == 1){
//                     ref.flush()
//                     rob.clear()
//                     c.io.cmt.fList.flush.poke(true)
//                     c.io.cmt.srat.flush.poke(true)
//                     index += 1
//                 } else{
//                     c.io.cmt.fList.flush.poke(false)
//                     c.io.cmt.srat.flush.poke(false)
//                     // 随机提交
//                     val commitEn = rand.nextBoolean()
//                     if(commitEn){
//                         val commitNum = rand.nextInt(ncommit + 1)
//                         for(i <- 0 until commitNum){
//                             if(rob.nonEmpty){
//                                 val item = rob.dequeue()
//                                 ref.commit(item)
//                                 c.io.cmt.fList.enq(i).valid.poke(item.rdVld != 0)
//                                 c.io.cmt.fList.enq(i).bits.poke(item.pprd)
//                                 c.io.cmt.srat.rdVld(i).poke(item.rdVld != 0)
//                                 c.io.cmt.srat.rd(i).poke(item.rd)
//                                 c.io.cmt.srat.prd(i).poke(item.prd)
//                             }
//                         }
//                     }
//                     if(c.io.fte.rinfo(0).ready.peek().litToBoolean){    
//                         // 1. 随机选定本次测试的数量，范围0-ndcd
//                         val ntest = rand.nextInt(ndcd)
//                         // 2. 读取本次测试的测试项并激励
//                         for(i <- 0 until ndcd){
//                             if(i < ntest){
//                                 c.io.fte.rinfo(i).valid.poke(true)
//                                 c.io.fte.rinfo(i).bits.rdVld.poke(tests(index + i).rdVld)
//                                 c.io.fte.rinfo(i).bits.rd.poke(tests(index + i).rd)
//                                 c.io.fte.rinfo(i).bits.rj.poke(tests(index + i).rj)
//                                 c.io.fte.rinfo(i).bits.rk.poke(tests(index + i).rk)
//                             } else {
//                                 c.io.fte.rinfo(i).valid.poke(false)
//                             }
//                         }
//                         // 3. 逐个比较并写入新映射，并将老映射写入pprdRob
//                         for(i <- 0 until ntest){
//                             c.io.fte.pinfo(i).pprd.expect(ref.read(tests(index+i).rd), 
//                                 f"idx: ${index + i}, rd: ${tests(index + i).rd}"
//                             )
//                             c.io.fte.pinfo(i).prj.expect(ref.read(tests(index+i).rj), 
//                                 f"idx: ${index + i}, rj: ${tests(index + i).rj}"
//                             )
//                             c.io.fte.pinfo(i).prk.expect(ref.read(tests(index+i).rk), 
//                                 f"idx: ${index + i}, rk: ${tests(index + i).rk}"
//                             )
//                             val pprd = ref.read(tests(index + i).rd)
//                             val preg = ref.write(tests(index + i).rd, tests(index + i).rdVld != 0)
//                             rob.enqueue(SimpleROBItem(tests(index + i).rdVld, tests(index + i).rd, preg, pprd))
//                             c.io.fte.pinfo(i).prd.expect(preg, 
//                                 f"idx: ${index + i}, rd: ${tests(index + i).rd}"
//                             )
//                         }
//                         index += ntest
//                     }
//                 }
//                 c.clock.step(1)
//                 if(index % PRINTINTERVAL == 0){
//                     print(s"\rTest: ${index * 100 / (testNum * ndcd)}%")
//                     Console.flush()
//                 }
//             }
//             println("\nTest completed!")
//         }
//     }
    
// }
