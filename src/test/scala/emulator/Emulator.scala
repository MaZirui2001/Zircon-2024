import spire.math.UInt

import chiseltest._
import CPU_Config.Commit.ncommit
import CPU_Config.RegisterFile.npreg

class Emulator{
    private val base_addr = UInt(0x80000000)
    private val memory      = new AXI_Memory(true)
    private val rnm_table   = Array.fill(32)(UInt(0))
    private val simulator   = new Simulator
    // private val cpu         = new CPU

    // debug
    val iring = new RingBuffer[(UInt, UInt)](8)
    var cycle = 0

    def sim_end(instruction: UInt): Boolean = {
        instruction == UInt(0x80000000)
    }

    /* difftest */
    def difftest_pc(pc_dut: UInt): Boolean = {
        val pc_ref = simulator.pc_dump()
        if(pc_ref != pc_dut){
            println(s"PC mismatch at ref: ${pc_ref.toInt.toHexString}, dut: ${pc_dut.toInt.toHexString}")
            return false
        }
        true
    }
    def difftest_rf(rd_idx: UInt, rd_data_dut: UInt, pc_dut: UInt): Boolean = {
        val rf_ref = simulator.rf_dump()
        if(rf_ref(rd_idx.toInt) != rd_data_dut){
            println(s"RF mismatch at pc ${pc_dut.toInt.toHexString}, reg ${rd_idx.toInt}(preg: ${rnm_table(rd_idx.toInt).toInt}), ref: ${rf_ref(rd_idx.toInt).toLong.toHexString}, dut: ${rd_data_dut.toLong.toHexString}")
            return false
        }
        true
    }
    def difftest_step(rd_idx: UInt, rd_data_dut: UInt, pc_dut: UInt, step: Int = 1): Boolean = {
        if(!difftest_pc(pc_dut)){
            return false
        }
        for(i <- 0 until step){
            simulator.step(1)
        }
        if(!difftest_rf(rd_idx, rd_data_dut, pc_dut)){
            return false
        }
        true
    }

    
    def rnm_table_update(rd: UInt, prd: UInt): Unit = {
        rnm_table(rd.toInt) = prd
    }
    def step(cpu: CPU, num: Int = 1): Int = {
        for(_ <- 0 until num){
            // commit check
            for(i <- 0 until ncommit){
                val cmt = cpu.io.dbg.cmt.deq(i).bits.peek()
                val dbg = cpu.io.dbg.peek()
                if(cpu.io.dbg.cmt.deq(i).valid.peek().litToBoolean){
                    iring.push((UInt(cmt.fte.pc.litValue.toLong), UInt(cmt.fte.inst.litValue.toLong)))
                    println(s"${cmt.fte.pc.litValue.toLong.toHexString}: ${cmt.fte.rd.litValue.toLong.toHexString} ${cmt.fte.prd.litValue.toLong.toHexString}")
                    if(sim_end(UInt(cmt.fte.inst.litValue.toLong))){
                        return (if(UInt(dbg.rf.rf(rnm_table(10).toInt).litValue.toLong) == UInt(0)) 0 else -1)
                    }
                    // update state
                    rnm_table_update(UInt(cmt.fte.rd.litValue.toLong), UInt(cmt.fte.prd.litValue.toLong))
                    val rd_idx = UInt(cmt.fte.rd.litValue.toLong)
                    val rd_data_dut = UInt(dbg.rf.rf(rnm_table(rd_idx.toInt).toInt).litValue.toLong)
                    val pc_dut = UInt(cmt.fte.pc.litValue.toLong)
                    val test_res = difftest_step(rd_idx, rd_data_dut, pc_dut)
                    if(!test_res){
                        return -2
                    }
                }
            }
            // memory bus
            val w = memory.write(cpu.io.axi.peek(), cycle)
            val r = memory.read(cpu.io.axi.peek())
            cpu.io.axi.arready.poke(r.arready)
            cpu.io.axi.awready.poke(w.awready)
            cpu.io.axi.bvalid.poke(w.bvalid)
            cpu.io.axi.rdata.poke(r.rdata.toLong)
            cpu.io.axi.rlast.poke(r.rlast)
            cpu.io.axi.rvalid.poke(r.rvalid)
            cpu.io.axi.wready.poke(w.wready)
            cpu.clock.step(1)
            cycle += 1
        }
        return 1
    }
    /* load the image from the file */
    def mem_init(filename: String): Unit = {
        memory.loadFromFile(filename, base_addr)
        simulator.mem_init(filename)
    }
}