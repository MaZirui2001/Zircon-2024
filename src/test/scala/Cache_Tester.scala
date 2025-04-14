// // import chisel3._
// import spire.math.UInt
// import chiseltest._
// import chiseltest.simulator.VerilatorFlags
// import org.scalatest.flatspec.AnyFlatSpec
// import scala.collection.mutable.Queue
// import scala.util._
// import ZirconConfig.Cache._
// import ZirconConfig.Fetch._
// import ZirconUtil._
// import os.write
// import os.Path
// import java.io.File
// import scala.collection.parallel.CollectionConverters._

// object CacheTestConfig {
//     val totalSpace             = 8192
//     val icacheSpaceStart      = 0
//     val icacheSpaceEnd        = totalSpace / 2 - nfch * 4 // 临界区不能访问
//     val dcacheSpaceStart      = totalSpace / 2
//     val dcacheSpaceEnd        = totalSpace
//     val testNum                = 65536
// }

// case class ICacheTestItem(var rreq: Int, var vaddr: BigInt)
// case class DCacheTestItem(var rreq: Int, var mtype: Int, var wreq: Int, var wdata: BigInt, var vaddr: BigInt)
// case class CacheTestItem(var ic: ICacheTestItem, var dc: DCacheTestItem)

// class CacheTestGenerator {
//     import CacheTestConfig._

//     // 预缓存mtype选择列表
//     private val MTYPECHOICES_0 = Array(0, 1, 2, 4, 5)
//     private val MTYPECHOICES_1 = Array(0, 4)
//     private val MTYPECHOICES_2 = Array(0, 1, 4, 5)
//     private val MTYPECHOICES_3 = Array(0, 4)
    
//     // 使用共享的Random实例
//     private val rand = new Random()

//     def generateTests: Unit = {
//         val testbenchDir = os.pwd / "testbench"
//         val testbenchPath = testbenchDir / "cachetest.txt"
//         // 如果目录不存在，则创建
//         if (!os.exists(testbenchDir)) {
//             os.makeDir(testbenchDir)
//         }
//         val writer = new java.io.PrintWriter(testbenchPath.toString)
//         val testArray = new Array[CacheTestItem](testNum)
        
//         // 移除.par，使用普通的map
//         val results = (0 until testNum).map { i =>
//             val item = CacheTestItem(ICacheTestItem(0, 0), DCacheTestItem(0, 0, 0, 0, 0))

//             // icache rreq: 0-1
//             item.ic.rreq = rand.nextInt(2)
//             // icache vaddr: range in icache but 4 byte aligned
//             item.ic.vaddr = rand.nextInt(icacheSpaceEnd - icacheSpaceStart) + icacheSpaceStart
//             item.ic.vaddr = (item.ic.vaddr >> 2) << 2

//             // dcache rreq、wreq: 0-1
//             val req = rand.nextInt(2) + 1
//             item.dc.rreq = req % 2
//             item.dc.wreq = req / 2
            
//             // dcache vaddr: range in dcache space
//             item.dc.vaddr = rand.nextInt(dcacheSpaceEnd - dcacheSpaceStart) + dcacheSpaceStart
            
//             // 优化mtype选择逻辑
//             item.dc.mtype = (item.dc.vaddr & 0x3).toInt match {
//                 case 0 => MTYPECHOICES_0(rand.nextInt(MTYPECHOICES_0.length))
//                 case 1 => MTYPECHOICES_1(rand.nextInt(MTYPECHOICES_1.length))
//                 case 2 => MTYPECHOICES_2(rand.nextInt(MTYPECHOICES_2.length))
//                 case 3 => MTYPECHOICES_3(rand.nextInt(MTYPECHOICES_3.length))
//             }
            
//             // dcache wdata: 0-2^31-1
//             item.dc.wdata = BigInt(rand.nextLong(0xFFFFFFFFL + 1) & 0xFFFFFFFFL)

//             item
//         }.toArray
        
//         Array.copy(results, 0, testArray, 0, testNum)
        
//         // 写入文件
//         for(i <- 0 until testNum) {
//             writer.println(
//                 f"${testArray(i).ic.rreq}%x " +
//                 f"${testArray(i).ic.vaddr}%x " +
//                 f"${testArray(i).dc.rreq}%x " +
//                 f"${testArray(i).dc.mtype}%x " +
//                 f"${testArray(i).dc.wreq}%x " +
//                 f"${testArray(i).dc.wdata}%x " +
//                 f"${testArray(i).dc.vaddr}%x"
//             )
//         }
        
//         writer.close()
//     }

//     def readTests(): Array[CacheTestItem] = {
//         val projectRoot = os.Path(System.getProperty("user.dir"))  // 获取项目根目录
//         val testbenchPath = (projectRoot / "testbench" / "cachetest.txt").toString
//         val source = scala.io.Source.fromFile(testbenchPath)
//         val lines = source.getLines().toArray
//         val res = Array.fill(lines.length)(CacheTestItem(ICacheTestItem(0, 0), DCacheTestItem(0, 0, 0, 0, 0)))
//         for(i <- 0 until lines.length){
//             val items = lines(i).split(" ")
//             // 这里读入的是十六进制数字，需要转换成十进制
//             for(j <- 0 until items.length) items(j) = Integer.parseUnsignedInt(items(j), 16).toString
//             val ic = ICacheTestItem(items(0).toInt, BigInt(items(1).toLong & 0xFFFFFFFFL))
//             val dc = DCacheTestItem(items(2).toInt, items(3).toInt, items(4).toInt, BigInt(items(5).toLong & 0xFFFFFFFFL), items(6).toInt)
//             res(i) = CacheTestItem(ic, dc)
//         }
//         res
//     }
// }

// class CacheTester extends AnyFlatSpec with ChiselScalatestTester{
//     val memory = new AXIMemory(true)
//     val testGenerator = new CacheTestGenerator
//     import CacheTestConfig._
//     println("initializing memory ...")
//     memory.initialize(totalSpace, false)
//     println("generating tests ...")
//     testGenerator.generateTests
//     println("loading tests from file ...")
//     val tests = testGenerator.readTests()
//     println("start testing ...")
//     behavior of "Cache"
//     it should "pass" in {   
//         test(new CacheTest).withAnnotations(Seq(
//             VerilatorBackendAnnotation,
//             // WriteVcdAnnotation,
//             VerilatorFlags(Seq("-threads", "2"))
//         )) { c =>
//             var iIndex = 0
//             var dIndex = 0
//             var i = 0
//             val iReqQ = Queue[ICacheTestItem]()
//             val dReqQ = Queue[DCacheTestItem]()
//             val dCmtQ = Queue[DCacheTestItem]()
            
//             val PRINTINTERVAL = testNum / 10
//             var randomCommitDelay = 0
//             val rand = new Random()
            
//             while(iIndex < testNum || dIndex < testNum) {
//                 val w = memory.write(c.io.axi.peek(), dIndex)
//                 val r = memory.read(c.io.axi.peek())
//                 c.io.axi.arready.poke(r.arready)
//                 c.io.axi.awready.poke(w.awready)
//                 c.io.axi.bvalid.poke(w.bvalid)
//                 c.io.axi.rdata.poke(r.rdata.toLong)
//                 c.io.axi.rlast.poke(r.rlast)
//                 c.io.axi.rvalid.poke(r.rvalid)
//                 c.io.axi.wready.poke(w.wready)
//                 // icache test
//                 if(iIndex < testNum){
//                     val iTest = tests(iIndex)
//                     c.io.iPP.rreq.poke(iTest.ic.rreq)
//                     c.io.iPP.vaddr.poke(iTest.ic.vaddr)
//                     if(iIndex > 0){
//                         val iTestLast = tests(iIndex - 1)
//                         c.io.iMmu.paddr.poke(iTestLast.ic.vaddr)
//                         c.io.iMmu.uncache.poke(false)
//                     }
//                     if(!c.io.iPP.miss.peek().litToBoolean){
//                         if(c.io.iPP.rreq.peek().litToBoolean){
//                             iReqQ.enqueue(iTest.ic)
//                         }
//                         iIndex += 1
//                     }
//                     if(c.io.iPP.rrsp.peek().litToBoolean){
//                         val req = iReqQ.dequeue()
//                         for(j <- 0 until nfch){
//                             val data = memory.debugRead((req.vaddr + (j * 4)).toInt)._1 & 0xFFFFFFFFL
//                             c.io.iPP.rdata(j).expect(data, f"idx: ${iIndex}, addr: ${req.vaddr}%x, fetchOffset: ${j}")

//                         }
                    
//                     }
//                 }
//                 // dcache test
//                 if(dIndex < testNum){
//                     val dTest = tests(dIndex)
//                     c.io.dPP.rreq.poke((if(c.io.dPP.sbFull.peek().litToBoolean) 0 else dTest.dc.rreq))
//                     c.io.dPP.mtype.poke(dTest.dc.mtype)
//                     c.io.dPP.isLatest.poke(true)
//                     c.io.dPP.wreq.poke((if(c.io.dPP.sbFull.peek().litToBoolean) 0 else dTest.dc.wreq))
//                     c.io.dPP.wdata.poke(dTest.dc.wdata)
//                     c.io.dPP.vaddr.poke(dTest.dc.vaddr)
//                     if(dIndex > 0){
//                         val dTestLast = tests(dIndex - 1)
//                         c.io.dMmu.paddr.poke(dTestLast.dc.vaddr)
//                         c.io.dMmu.uncache.poke(false)
//                     }
//                     randomCommitDelay = if(randomCommitDelay > 0) { randomCommitDelay - 1 } else { rand.nextInt(4) }
//                     if(randomCommitDelay == 0 && dCmtQ.nonEmpty){
//                         val req = dCmtQ.dequeue()
//                         if(req.wreq == 1){
//                             c.io.dCmt.stCmt.poke(true)
//                         } else {
//                             c.io.dCmt.stCmt.poke(false)
//                         }
//                     } else {
//                         c.io.dCmt.stCmt.poke(false)
//                     }
//                     if(!c.io.dPP.miss.peek().litToBoolean && !c.io.dPP.sbFull.peek().litToBoolean){
//                         if(c.io.dPP.rreq.peek().litToBoolean || c.io.dPP.wreq.peek().litToBoolean){
//                             dReqQ.enqueue(dTest.dc)
//                         }
//                         dIndex += 1
//                     }
//                     if(c.io.dPP.rrsp.peek().litToBoolean || c.io.dPP.wrsp.peek().litToBoolean){
//                         val req = dReqQ.dequeue()
//                         dCmtQ.enqueue(req)
//                         if(c.io.dPP.wrsp.peek().litToBoolean){
//                             val addrAlign = (req.vaddr >> 2) << 2
//                             val wdata = req.wdata << ((req.vaddr.toInt & 0x3) << 3)
//                             val wstrb = ((1 << (1 << (req.mtype & 0x3))) - 1) << (req.vaddr.toInt & 0x3)
//                             memory.debugWrite(addrAlign.toInt, wdata.toInt, wstrb, dIndex-1)
//                         } else if(c.io.dPP.rrsp.peek().litToBoolean){
//                             var data = BigInt("0" * 32, 2)
//                             data = memory.debugRead(req.vaddr.toInt)._1 & 0xFFFFFFFFL
//                             data = req.mtype match {
//                                 case 0 => if((data & 0x80) == 0) data & 0xFF else data | 0xFFFFFF00
//                                 case 1 => if((data & 0x8000) == 0) data & 0xFFFF else data | 0xFFFF0000
//                                 case 2 => data
//                                 case 4 => data & 0xFF
//                                 case 5 => data & 0xFFFF
//                                 case _ => data
//                             }
//                             c.io.dPP.rdata.expect(
//                                 data & 0xFFFFFFFFL, f"idx: ${dIndex}, addr: ${req.vaddr}%x, last write: 1. ${memory.debugRead(req.vaddr.toInt)._2}, " +
//                                                                         f"2. ${memory.debugRead(req.vaddr.toInt + 1)._2}, " +
//                                                                         f"3. ${memory.debugRead(req.vaddr.toInt + 2)._2}, "  +
//                                                                         f"4. ${memory.debugRead(req.vaddr.toInt + 3)._2}"
//                             )
//                         }
//                     }
//                 }
//                 c.clock.step(1)
//                 i += 1

//                 if (iIndex % PRINTINTERVAL == 0 || dIndex % PRINTINTERVAL == 0) {
//                     print(s"\rICache: ${iIndex * 100 / testNum}% DCache: ${dIndex * 100 / testNum}%")
//                     Console.flush()  // 确保立即显示
//                 }
//             }
            
//             println("\nTest completed!")
//             println(s"cache test: $dIndex, total cycles: $i")
//         }
//     }
// }