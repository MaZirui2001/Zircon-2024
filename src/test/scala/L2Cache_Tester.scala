// // import chisel3._
// import chiseltest._
// import org.scalatest.flatspec.AnyFlatSpec
// import scala.collection.mutable.Queue
// import scala.util._
// import ZirconConfig.Cache._
// import ZirconUtil._
// import os.write
// import spire.math.UInt

// object L2TestConfig {
//     val totalSpace             = 4096
//     val icacheSpaceStart      = 0
//     val icacheSpaceEnd        = totalSpace / 2
//     val dcacheSpaceStart      = totalSpace / 2
//     val dcacheSpaceEnd        = totalSpace
//     val testNum                = 32768
// }
// case class L2ICacheTestItem(var rreq: Int, var paddr: Int, var uncache: Int)
// case class L2DCacheTestItem(var rreq: Int, var wreq: Int, var paddr: Int, var uncache: Int, var wdata: BigInt, var mtype: Int)
// case class L2TestItem(var ic: L2ICacheTestItem, var dc: L2DCacheTestItem)

// class L2TestGenerator{
//     import L2TestConfig._

//     // 将测试写入文件
//     def generateTests: Unit = {
//         val writer = new java.io.PrintWriter("testbench/l2test.txt")
//         var testList = List.fill(testNum)(L2TestItem(L2ICacheTestItem(0, 0, 0), L2DCacheTestItem(0, 0, 0, 0, 0, 0)))
//         // 生成icache访问有效性，间隔有效性为1
//         testList.zipWithIndex.map{ case (item, i) => item.ic.rreq = if(i % 2 == 0) 1 else 0}
//         // 生成icache的访问地址，范围0-1023
//         testList.zipWithIndex.map{ case (item, i) => item.ic.paddr = Random.nextInt(icacheSpaceEnd - icacheSpaceStart) + icacheSpaceStart}
//         // 生成icache的uncache信号，目前暂时全为0
//         testList.zipWithIndex.map{ case (item, i) => item.ic.uncache = 0}
//         // 生成dcache的读有效性，间隔有效性为1
//         testList.zipWithIndex.map{ case (item, i) => item.dc.rreq = if(i % 2 == 0) 1 else 0}
//         // 生成dcache的写有效性，注意只有在读有效性为1的时候才能写，范围0-1
//         testList.zipWithIndex.map{ case (item, i) => item.dc.wreq = if(item.dc.rreq == 1) Random.nextInt(2) else 0}
//         // 生成dcache的地址，范围1024-2047
//         testList.zipWithIndex.map{ case (item, i) => item.dc.paddr = Random.nextInt(dcacheSpaceEnd - dcacheSpaceStart) + dcacheSpaceStart}
//         // 生成dcache的uncache信号，目前暂时全为0
//         testList.zipWithIndex.map{ case (item, i) => item.dc.uncache = 0}
//         // 生成dcache的写数据，范围2^31-1
//         testList.zipWithIndex.map{ case (item, i) => item.dc.wdata = BigInt(Random.nextLong(0xFFFFFFFFL + 1) & 0xFFFFFFFFL)}
//         // 生成dcache的mtype，范围0-3
//         testList.zipWithIndex.map{ case (item, i) => item.dc.mtype = (item.dc.paddr & 3) match{
//             case 0 => Random.nextInt(3)
//             case 1 => 0
//             case 2 => Random.nextInt(2)
//             case 3 => 0
//         }}
//         // 将这些信号写入文件，每一组信号占一行
//         for(i <- 0 until testNum){
//             writer.println(
//                 f"${testList(i).ic.rreq}%x " +
//                 f"${testList(i).ic.paddr}%x " +
//                 f"${testList(i).ic.uncache}%x " +
//                 f"${testList(i).dc.rreq}%x " +
//                 f"${testList(i).dc.wreq}%x " +
//                 f"${testList(i).dc.paddr}%x " +
//                 f"${testList(i).dc.uncache}%x " +
//                 f"${testList(i).dc.wdata}%x " +
//                 f"${testList(i).dc.mtype}%x"
//             )
//         }
//         writer.close()
//     }
//     // 从文件里读取测试
//     def readTest(): Array[L2TestItem] = {
//         val source = scala.io.Source.fromFile("testbench/l2test.txt")
//         val lines = source.getLines().toArray
//         val res = Array.fill(lines.length)(L2TestItem(L2ICacheTestItem(0, 0, 0), L2DCacheTestItem(0, 0, 0, 0, 0, 0)))
//         for(i <- 0 until lines.length){
//             val items = lines(i).split(" ")
//             // 这里读入的是十六进制数字，需要转换成十进制
//             for(j <- 0 until items.length) items(j) = Integer.parseUnsignedInt(items(j), 16).toString
//             val ic = L2ICacheTestItem(items(0).toInt, items(1).toInt, items(2).toInt)
//             val dc = L2DCacheTestItem(items(3).toInt, items(4).toInt, items(5).toInt, items(6).toInt, BigInt(items(7).toLong & 0xFFFFFFFFL), items(8).toInt)
//             res(i) = L2TestItem(ic, dc)
//         }
//         res
//     }
// }


// class L2CacheTester extends AnyFlatSpec with ChiselScalatestTester{
//     val testNum = 32768
//     val memory = new AXIMemory(true)
//     val testGen = new L2TestGenerator
//     import L2TestConfig._
//     memory.initialize(totalSpace, false)
//     // testGen.generateTests
//     val tests = testGen.readTest()
//     println("start")

//     behavior of "L2Cache"
//     it should "pass" in {
//         test(new L2CacheTest).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation))
//         { c =>
//             var iTestIndex = 0
//             var dTestIndex = 0
//             var i = 0
//             val icacheReqQ = Queue[L2ICacheTestItem]()
//             val dcacheReqQ = Queue[L2DCacheTestItem]()
//             while(iTestIndex < testNum || dTestIndex < testNum){ 
//                 // 创建信号包装器
//                 val axiSignals = new AXISignals()
//                 // 填充所有信号值
//                 axiSignals.araddr = c.io.axi.araddr.peek().litValue.toInt
//                 axiSignals.arburst = c.io.axi.arburst.peek().litValue.toInt
//                 axiSignals.arlen = c.io.axi.arlen.peek().litValue.toInt
//                 axiSignals.arsize = c.io.axi.arsize.peek().litValue.toInt
//                 axiSignals.arvalid = c.io.axi.arvalid.peek().litToBoolean
//                 
//                 axiSignals.awaddr = c.io.axi.awaddr.peek().litValue.toInt
//                 axiSignals.awburst = c.io.axi.awburst.peek().litValue.toInt
//                 axiSignals.awlen = c.io.axi.awlen.peek().litValue.toInt
//                 axiSignals.awsize = c.io.axi.awsize.peek().litValue.toInt
//                 axiSignals.awvalid = c.io.axi.awvalid.peek().litToBoolean
//                 
//                 axiSignals.bready = c.io.axi.bready.peek().litToBoolean
//                 
//                 axiSignals.rready = c.io.axi.rready.peek().litToBoolean
//                 
//                 axiSignals.wdata = c.io.axi.wdata.peek().litValue.toInt
//                 axiSignals.wlast = c.io.axi.wlast.peek().litToBoolean
//                 axiSignals.wstrb = c.io.axi.wstrb.peek().litValue.toInt
//                 axiSignals.wvalid = c.io.axi.wvalid.peek().litToBoolean
//
//                 // read and write pacakge get from axi
//                 val w = memory.write(
//                     axiSignals,
//                     dTestIndex
//                 )
//                 val r = memory.read(
//                     axiSignals
//                 )
//                 // write and read pacakge send to axi
//                 c.io.axi.arready.poke(r.arready)
//                 c.io.axi.awready.poke(w.awready)
//                 c.io.axi.bvalid.poke(w.bvalid)
//                 c.io.axi.rdata.poke(r.rdata.toInt)
//                 c.io.axi.rlast.poke(r.rlast)
//                 c.io.axi.rvalid.poke(r.rvalid)
//                 c.io.axi.wready.poke(w.wready)

//                 // motivation
//                 if(iTestIndex < testNum){
//                     c.io.ic.rreq.poke(tests(iTestIndex).ic.rreq)
//                     c.io.ic.paddr.poke(tests(iTestIndex).ic.paddr)
//                     c.io.ic.uncache.poke(tests(iTestIndex).ic.uncache)
//                 }
//                 if(dTestIndex < testNum){
//                     c.io.dc.rreq.poke(tests(dTestIndex).dc.rreq)
//                     c.io.dc.wreq.poke(tests(dTestIndex).dc.wreq)
//                     c.io.dc.paddr.poke(tests(dTestIndex).dc.paddr)
//                     c.io.dc.uncache.poke(tests(dTestIndex).dc.uncache)
//                     c.io.dc.wdata.poke(tests(dTestIndex).dc.wdata)
//                     c.io.dc.mtype.poke(tests(dTestIndex).dc.mtype)
//                 }

//                 // reference and update
//                 if(!c.io.ic.miss.peek().litToBoolean){
//                     if(c.io.ic.rreq.peek().litToBoolean){
//                         icacheReqQ.enqueue(tests(iTestIndex).ic)
//                     }
//                     if(iTestIndex < testNum){
//                         iTestIndex += 1
//                     }
//                 }
//                 if(!c.io.dc.miss.peek().litToBoolean){
//                     if(c.io.dc.rreq.peek().litToBoolean){
//                         dcacheReqQ.enqueue(tests(dTestIndex).dc)
//                     }
//                     if(dTestIndex < testNum){
//                         dTestIndex += 1
//                     }
//                 }
//                 // check icache result
//                 if(c.io.ic.rrsp.peek().litToBoolean){
//                     val item = icacheReqQ.dequeue()
//                     val paddr = item.paddr
//                     val paddrDebug = (paddr >> l1Offset) << l1Offset
//                     var data = BigInt("0" * (32 * (l1Offset - 1)), 2)
//                     for(j <- l1Offset - 2 until -1 by -1){
//                         data = (data << 32) | (memory.debugRead(paddrDebug + 4 * j)._1 & 0xFFFFFFFFL)
//                     }
//                     var rmask = BigInt("1" * (32 * (l1Offset - 1)), 2)
//                     c.io.ic.rline.expect(data & rmask, f"addr: ${paddrDebug}%x, last write: ${memory.debugRead(paddrDebug)._2}")
//                 }
//                 // check dcache result
//                 if(c.io.dc.rrsp.peek().litToBoolean || c.io.dc.wrsp.peek().litToBoolean){
//                     val item = dcacheReqQ.dequeue()
//                     val paddr = item.paddr
//                     val paddrDebug = (paddr >> l1Offset) << l1Offset
//                     // write: debug write to the ref memory
//                     if(c.io.dc.wrsp.peek().litToBoolean){
//                         val paddrAlign = (paddr >> 2) << 2
//                         val wdata = item.wdata << ((paddr & 0x3) << 3)
//                         val wstrb = ((1 << (1 << item.mtype)) - 1) << (paddr & 0x3)
//                         memory.debugWrite(paddrAlign, wdata.toInt, wstrb, dTestIndex)
//                     }
//                     var data = BigInt("0" * (32 * (l1Offset - 1)), 2)
//                     for(j <- l1Offset - 2 until -1 by -1){
//                         data = (data << 32) | (memory.debugRead(paddrDebug + 4 * j)._1 & 0xFFFFFFFFL)
//                     }
//                     var rmask = BigInt("1" * (32 * (l1Offset - 1)), 2)
//                     c.io.dc.rline.expect(data & rmask, f"addr: ${paddrDebug}%x, last write: 1. ${memory.debugRead(paddrDebug)._2}, " +
//                                                                                            f"2. ${memory.debugRead(paddrDebug + 1)._2}, " +
//                                                                                            f"3. ${memory.debugRead(paddrDebug + 2)._2}, "  +
//                                                                                            f"4. ${memory.debugRead(paddrDebug + 3)._2}")
//                 }

//                 c.clock.step(1)
//                 i = i + 1
//             }
//             println(s"icache test: $iTestIndex, dcache test: $dTestIndex, total cycles: $i")
//         }
//     }
// }