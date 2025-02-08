import chisel3._
import chisel3.util._
import scala.collection.mutable

object ReadState extends Enumeration {
    val IDLE, AR, R = Value
}
object WriteState extends Enumeration {
    val IDLE, AW, W, B = Value
}

class Memory_Item(var data: Int, var last_write: Array[Int])
class AXI_Read_Config(var araddr: Int, var arlen: Int, var arsize: Int, var arburst: Int, var state: ReadState.Value)
class AXI_Read_Item(var arready: Bool, var rdata: UInt, var rvalid: Bool, var rlast: Bool)
class AXI_Write_Config(var awaddr: Int, var awlen: Int, var awsize: Int, var awburst: Int, var wstrb: Int, var wlast: Boolean, var state: WriteState.Value)
class AXI_Write_Item(var awready: Bool, var wready: Bool, var bvalid: Bool)


class AXI_Memory(rand_delay: Boolean){
    private var mem: mutable.Map[Int, Memory_Item] = mutable.Map()
    private var mem_ref: mutable.Map[Int, Memory_Item] = mutable.Map()
    var readConfig: AXI_Read_Config = new AXI_Read_Config(0, 0, 0, 0, ReadState.IDLE)
    var writeConfig: AXI_Write_Config = new AXI_Write_Config(0, 0, 0, 0, 0, false, WriteState.IDLE)

    // read from the memory
    def read(
        // araddr: UInt, 
        // arlen: UInt, 
        // arsize: UInt, 
        // arburst: UInt,
        // arvalid: Bool,
        // rready: Bool
        axi: AXI_IO
    ): AXI_Read_Item = {
        var readItem = new AXI_Read_Item(false.B, 0.U, false.B, false.B)
        readConfig.state match {
            // record the read configuration
            case ReadState.IDLE => {
                assert(axi.arsize.litValue.toInt <= 2, "The minimum size of the data is 4 bytes")
                if(axi.arvalid.litToBoolean){
                    readConfig = new AXI_Read_Config(
                        axi.araddr.litValue.toInt,
                        axi.arlen.litValue.toInt, 
                        1 << axi.arsize.litValue.toInt, 
                        axi.arburst.litValue.toInt, 
                        ReadState.AR
                    )
                }
            }
            // address handshaking
            case ReadState.AR => {
                // random delay
                readItem.arready = (if(rand_delay){ (scala.util.Random.nextInt(4) != 0).B } else{ true.B })
                // println("arready: " + readItem.arready.litToBoolean + " arvalid: " + arvalid.litToBoolean)
                if(readItem.arready.litToBoolean && axi.arvalid.litToBoolean){
                    readConfig.state = ReadState.R
                }
            }
            case ReadState.R => {
                // random delay
                readItem.rvalid = (if(rand_delay){ (scala.util.Random.nextInt(4) != 0).B } else{ true.B })
                // if random delay is not enabled, the read data is ready
                if(readItem.rvalid.litToBoolean){
                    val word_addr = readConfig.araddr / 4
                    val word_offset = readConfig.araddr % 4
                    // assert the address is in the memory
                    assert(mem.contains(word_addr), f"Address ${readConfig.araddr}%x is not in the memory")
                    readItem.rdata = ((mem(word_addr).data >> (word_offset * 8) ) & 0xFFFFFFFFL).U
                    // check if the last read
                    if(readConfig.arlen == 0){
                        readItem.rlast = true.B
                        if(axi.rready.litToBoolean){
                            readConfig.state = ReadState.IDLE
                        }
                    }else{
                        if(axi.rready.litToBoolean){
                            readConfig.arlen = readConfig.arlen - 1
                            readConfig.araddr = readConfig.araddr + readConfig.arsize
                        }
                    }
                }
            }
        }
        readItem
    }
    // write to the memory
    def write(
        // awaddr: UInt, 
        // awlen: UInt, 
        // awsize: UInt, 
        // awburst: UInt,
        // wdata: UInt,
        // wstrb: UInt,
        // wlast: Bool,
        // awvalid: Bool,
        // wvalid: Bool,
        // bready: Bool,
        axi: AXI_IO,
        cycle: Int
    ): AXI_Write_Item = {
        var writeItem = new AXI_Write_Item(false.B, false.B, false.B)
        writeConfig.state match {
            // record the write configuration
            case WriteState.IDLE => {
                assert(axi.arsize.litValue.toInt <= 2, "The minimum size of the data is 4 bytes")
                if(axi.awvalid.litToBoolean){
                    writeConfig = new AXI_Write_Config(
                        axi.awaddr.litValue.toInt,
                        axi.awlen.litValue.toInt, 
                        1 << axi.awsize.litValue.toInt, 
                        axi.awburst.litValue.toInt, 
                        axi.wstrb.litValue.toInt, 
                        axi.wlast.litToBoolean, 
                        WriteState.AW
                    )
                }
            }
            // address handshaking
            case WriteState.AW => {
                // random delay
                writeItem.awready = (if(rand_delay){ (scala.util.Random.nextInt(4) != 0).B } else{ true.B })
                if(writeItem.awready.litToBoolean && axi.awvalid.litToBoolean){
                    writeConfig.state = WriteState.W
                }
            }
            case WriteState.W => {
                // random delay
                writeItem.wready = (if(rand_delay){ (scala.util.Random.nextInt(4) != 0).B } else{ true.B })
                if(writeItem.wready.litToBoolean){
                    val word_addr = writeConfig.awaddr / 4
                    val word_offset = writeConfig.awaddr % 4
                    if(!mem.contains(word_addr)){
                        mem(word_addr) = new Memory_Item(0, Array.fill(4)(0))
                    }
                    if(axi.wlast.litToBoolean){
                        writeConfig.state = WriteState.B
                    }
                    // write into the memory
                    var wmask = (0xff << (word_offset * 8))
                    var wdata_shift = (axi.wdata.litValue.toInt << (word_offset * 8))
                    (0 until writeConfig.awsize).foreach{ i =>
                        if((writeConfig.wstrb & (1 << i)) != 0){
                            mem(word_addr).data = (mem(word_addr).data & ~wmask | wdata_shift & wmask)
                            mem(word_addr).last_write(word_offset+i) = cycle
                        }
                        wmask = wmask << 8
                    }
                    writeConfig.awaddr = writeConfig.awaddr + writeConfig.awsize
                }
            }
            case WriteState.B => {
                // random delay
                writeItem.bvalid = if(rand_delay){ ( scala.util.Random.nextInt(4) != 0).B } else{ true.B }
                if(writeItem.bvalid.litToBoolean && axi.bready.litToBoolean){
                    writeConfig.state = WriteState.IDLE
                }
            }
        }
        writeItem
    }

    def debug_read(addr: Int): (Int, Int) = {
        val word_addr = addr / 4
        val word_offset = addr % 4
        assert(mem_ref.contains(word_addr), f"Address ${addr}%x is not in the memory")
        ((mem_ref(word_addr).data >> (word_offset * 8)), mem_ref(word_addr).last_write(word_offset))
    }
    def debug_write(addr: Int, wdata: Int, wstrb: Int, cycle: Int): Unit = {
        assert(addr % 4 == 0, "The address must be aligned to 4 bytes")
        val word_addr = addr / 4
        val word_offset = addr % 4
        if(!mem.contains(word_addr)){
            mem_ref(word_addr) = new Memory_Item(0, Array.fill(4)(0))
        }
        // println(f"addr: ${addr}%x, data: ${wdata}%x, strb: ${wstrb}%x")
        (0 until 4).foreach{ i =>
            if((wstrb & (1 << i)) != 0){
                mem_ref(word_addr).data = (mem_ref(word_addr).data & ~(0xff << (i * 8)) | wdata.toInt & (0xff << (i * 8)))
                mem_ref(word_addr).last_write(i) = cycle
                // if(word_addr == 0x58c >> 2) println(cycle)
            }
        }
    }
    def initialize(size: Int, load: Boolean): Unit = {
        for(i <- 0 until size){
            mem(i) = new Memory_Item((i*4), Array.fill(4)(0))
            mem_ref(i) = new Memory_Item((i*4), Array.fill(4)(0))
        }
    }
}