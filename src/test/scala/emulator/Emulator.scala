import spire.math.UInt

import chiseltest._
import ZirconConfig.Commit.ncommit
import ZirconConfig.RegisterFile.npreg

class Emulator{
    private val baseAddr    = UInt(0x80000000)
    private val memory      = new AXIMemory(true)
    private val rnmTable    = Array.fill(32)(UInt(0))
    private val simulator   = new Simulator
    private val statistic   = new Statistic
    // private val cpu         = new CPU

    // debug
    val iring = new RingBuffer[(UInt, UInt, UInt, UInt)](8)
    private var cyclesFromLastCommit = 0

    // 缓存常用值，避免重复计算
    private val stallThreshold = 1000
    private val endInstruction = UInt(0x80000000)

    // var cycle = 0

    def simEnd(instruction: UInt): Boolean = {
        instruction == endInstruction
    }
    def stallForTooLong(): Boolean = {
        cyclesFromLastCommit >= stallThreshold
    }

    /* difftest */
    def difftestPC(pcDut: UInt): Boolean = {
        val pcRef = simulator.pcDump()
        if(pcRef != pcDut){
            println(s"PC mismatch at ref: ${pcRef.toInt.toHexString}, dut: ${pcDut.toInt.toHexString}")
            return false
        }
        true
    }
    def difftestRF(rdIdx: UInt, rdDataDut: UInt, pcDut: UInt): Boolean = {
        val rfRef = simulator.rfDump()
        if(rfRef(rdIdx.toInt) != rdDataDut){
            println(s"RF mismatch at pc ${pcDut.toInt.toHexString}, reg ${rdIdx.toInt}(preg: ${rnmTable(rdIdx.toInt).toInt}), ref: ${rfRef(rdIdx.toInt).toLong.toHexString}, dut: ${rdDataDut.toLong.toHexString}")
            return false
        }
        true
    }
    def difftestStep(rdIdx: UInt, rdDataDut: UInt, pcDut: UInt, step: Int = 1): Boolean = {
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
    
    def rnmTableUpdate(rd: UInt, prd: UInt): Unit = {
        rnmTable(rd.toInt) = prd
    }

    def step(cpu: CPU, num: Int = 1): Int = {
        statistic.addCycles(num)
        for(_ <- 0 until num){
            // commit check
            for(i <- 0 until ncommit){
                if(stallForTooLong()){
                    return -3
                }
                val cmt = cpu.io.dbg.cmt.deq(i).bits.peek()
                val dbg = cpu.io.dbg.peek() 
                if(cpu.io.dbg.cmt.deq(i).valid.peek().litToBoolean){
                    cyclesFromLastCommit = 0
                    iring.push((UInt(cmt.fte.pc.litValue.toLong), UInt(cmt.fte.inst.litValue.toLong), UInt(cmt.fte.rd.litValue.toLong), UInt(cmt.fte.prd.litValue.toLong)))
                    statistic.addInsts(1)
                    // println(s"${cmt.fte.pc.litValue.toLong.toHexString}: ${cmt.fte.rd.litValue.toLong.toHexString} ${cmt.fte.prd.litValue.toLong.toHexString}")
                    if(simEnd(UInt(cmt.fte.inst.litValue.toLong))){
                        println(s"Total cycles: ${statistic.getTotalCycles()}, Total insts: ${statistic.getTotalInsts()}, IPC: ${statistic.getIpc()}")
                        return (if(UInt(dbg.rf.rf(rnmTable(10).toInt).litValue.toLong) == UInt(0)) 0 else -1)
                    }
                    // update state
                    rnmTableUpdate(UInt(cmt.fte.rd.litValue.toLong), UInt(cmt.fte.prd.litValue.toLong))
                    val rdIdx = UInt(cmt.fte.rd.litValue.toLong)
                    val rdDataDut = UInt(dbg.rf.rf(rnmTable(rdIdx.toInt).toInt).litValue.toLong)
                    val pcDut = UInt(cmt.fte.pc.litValue.toLong)
                    val testRes = difftestStep(rdIdx, rdDataDut, pcDut)
                    if(!testRes){
                        return -2
                    }
                }
            }
            // memory bus
            val w = memory.write(cpu.io.axi.peek(), statistic.getTotalCycles())
            val r = memory.read(cpu.io.axi.peek())
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
    }

}