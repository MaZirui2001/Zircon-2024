import spire.math.UInt

import chiseltest._
import ZirconConfig.Commit.ncommit
import ZirconConfig.RegisterFile.npreg

import scala.concurrent.{ExecutionContext, Future, Await}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.{Success, Failure}
import java.io.{PrintStream, FileOutputStream}
import java.lang.management.{ManagementFactory, ThreadInfo, ThreadMXBean}


class Emulator{
    private val baseAddr    = UInt(0x80000000)
    private val memory      = new AXIMemory(true)
    private val rnmTable    = Array.fill(32)(0)
    private val simulator   = new Simulator
    private val statistic   = new Statistic
    private var memAccessTime = 0L
    // private val cpu         = new CPU

    // debug
    val iring = new RingBuffer[(Int, Int, Int, Int)](8)
    private var cyclesFromLastCommit = 0

    // 缓存常用值，避免重复计算
    private val stallThreshold = 1000
    private val endInstruction = 0x80000000

    // var cycle = 0

    def simEnd(instruction: Int): Boolean = {
        instruction == endInstruction
    }
    def stallForTooLong(): Boolean = {
        cyclesFromLastCommit >= stallThreshold
    }

    /* difftest */
    def difftestPC(pcDut: Int): Boolean = {
        val pcRef = simulator.pcDump()
        if(pcRef != pcDut){
            println(s"PC mismatch at ref: ${pcRef.toInt.toHexString}, dut: ${pcDut.toInt.toHexString}")
            return false
        }
        true
    }
    def difftestRF(rdIdx: Int, rdDataDut: Int, pcDut: Int): Boolean = {
        val rfRef = simulator.rfDump()
        if(rfRef(rdIdx) != rdDataDut){
            println(s"RF mismatch at pc ${pcDut.toInt.toHexString}, reg ${rdIdx.toInt}(preg: ${rnmTable(rdIdx.toInt).toInt}), ref: ${rfRef(rdIdx.toInt).toLong.toHexString}, dut: ${rdDataDut.toLong.toHexString}")
            return false
        }
        true
    }
    def difftestStep(rdIdx: Int, rdDataDut: Int, pcDut: Int, step: Int = 1): Boolean = {
        if(!difftestPC(pcDut)){
            return false
        }
        for(i <- 0 until step){
            simulator.step(1)
        }
        if(!difftestRF(rdIdx, rdDataDut, pcDut)){
            return false
        }
        true
    }
    
    def rnmTableUpdate(rd: Int, prd: Int): Unit = {
        rnmTable(rd) = prd
    }

    def step(cpu: CPU, num: Int = 1): Int = {
        var start = 0L
        var end = 0L
        // statistic.addCycles(num)
        
        // 创建一个线程，等待其他线程都结束后，动态显示Total Cycles和IPC
        Future{
            while(true){
                print(s"\rTotal cycles: ${statistic.getTotalCycles()}, " +
                      s"IPC: ${statistic.getIpc()}, " +
                      s"Mem access time: ${memAccessTime / 1000000000} s")
                Thread.sleep(1000)
            }
        }
        var n = num
        while(n != 0){
            n -= 1
            statistic.addCycles(1)
            // commit check
            for(i <- 0 until ncommit){
                if(stallForTooLong()){
                    return -3
                }
                val cmt     = cpu.io.dbg.get.cmt.deq(i).bits
                val dbg     = cpu.io.dbg.get

                if(cpu.io.dbg.get.cmt.deq(i).valid.peek().litToBoolean){
                    cyclesFromLastCommit = 0
                    val cmtFteRd   = cmt.fte.rd.peek().litValue.toInt
                    val cmtFtePC   = cmt.fte.pc.peek().litValue.toInt
                    val cmtFteInst = cmt.fte.inst.peek().litValue.toInt
                    val cmtFtePrd  = cmt.fte.prd.peek().litValue.toInt
                    val cmtFtePred = cmt.fte.predType.peek().litValue.toInt
                    val cmtFtePredFail = cmt.bke.predFail.peek().litToBoolean
                    // iring: record the instruction
                    iring.push((cmtFtePC, cmtFteInst, cmtFteRd, cmtFtePrd))
                    // statistic:
                    statistic.addInsts(1)
                    // jump
                    statistic.addJump(cmtFtePred, cmtFtePredFail)
                    // cache
                    // end check
                    if(simEnd(cmtFteInst)){
                        statistic.addCacheVisit(
                            dbg.fte.ic.visit.peek().litValue.toInt, dbg.fte.ic.hit.peek().litValue.toInt, 
                            dbg.bke.lsPP.dc(0).visit.peek().litValue.toInt, dbg.bke.lsPP.dc(0).hit.peek().litValue.toInt,
                            dbg.bke.lsPP.dc(1).visit.peek().litValue.toInt, dbg.bke.lsPP.dc(1).hit.peek().litValue.toInt,
                            dbg.l2(0).visit.peek().litValue.toInt, dbg.l2(0).hit.peek().litValue.toInt,
                            dbg.l2(1).visit.peek().litValue.toInt, dbg.l2(1).hit.peek().litValue.toInt
                        )
                        statistic.addFrontendBlockCycle(cpu)
                        statistic.addDispatchBlockCycle(cpu)
                        statistic.addBackendBlockCycle(cpu)
                        statistic.addInstNums(simulator.instRecorderDump())
                        return (if(dbg.rf.rf(rnmTable(10)).peek().litValue.toInt == 0) 0 else -1)
                    }
                    // rnm table update
                    rnmTableUpdate(cmtFteRd, cmtFtePrd)
                    // difftest
                    val rdDataDut = dbg.rf.rf(rnmTable(cmtFteRd)).peek().litValue.toInt
                    if(!difftestStep(cmtFteRd, rdDataDut, cmtFtePC)){
                        return -2
                    }
                }

            }
            // memory bus
            val w = memory.write(cpu, statistic.getTotalCycles())
            val r = memory.read(cpu)

            cpu.io.axi.arready.poke(r.arready)
            cpu.io.axi.awready.poke(w.awready)
            cpu.io.axi.bvalid.poke(w.bvalid)
            cpu.io.axi.rdata.poke(r.rdata.toLong)
            cpu.io.axi.rlast.poke(r.rlast)
            cpu.io.axi.rvalid.poke(r.rvalid)
            cpu.io.axi.wready.poke(w.wready)

            cyclesFromLastCommit += 1
            cpu.clock.step(1)
            // cycle += 1
        }
        return 1
    }
    /* load the image from the file */
    def memInit(filename: String): Unit = {
        memory.loadFromFile(filename, baseAddr)
        simulator.memInit(filename)
    }
    def statisticInit(testRunDir: String, imgName: String): Unit = {
        statistic.setImgName(imgName)
        statistic.setReportPath(testRunDir, imgName)
    }


    /* print the instruction ring buffer */
    def printIRing(): Unit = {
        println("指令环缓冲区:")
        val iring = this.iring.toArray
        for (i <- 0 until iring.length) {
            // 十六进制补充前导0到32位
            println(f"${iring(i)._1.toInt.toHexString.reverse.padTo(8, '0').reverse.mkString}: " +
                f"${iring(i)._2.toInt.toHexString.reverse.padTo(8, '0').reverse.mkString} " +
                f"${iring(i)._3.toInt.toHexString.reverse.padTo(2, '0').reverse.mkString} " +
                f"${iring(i)._4.toInt.toHexString.reverse.padTo(2, '0').reverse.mkString}")
        }
    }
    
    def printStatistic(): Unit = {
        printIRing()
        println(s"Total cycles: ${statistic.getTotalCycles()}, Total insts: ${statistic.getTotalInsts()}, IPC: ${statistic.getIpc()}")
        statistic.makeMarkdownReport()
    }

}