import chisel3._
import chisel3.util._
import scala.collection.mutable

object ReadState extends Enumeration {
    val IDLE, AR, R = Value
}
object WriteState extends Enumeration {
    val IDLE, AW, W, B = Value
}

case class Memory_Item(data: UInt, last_write: Array[Int])
case class AXI_Read_Config(araddr: Int, arlen: Int, arsize: Int, arburst: Int, state: ReadState.Value)
case class AXI_Read_Item(arready: Bool, rdata: UInt, rvalid: Bool, rlast: Bool)
case class AXI_Write_Config(awaddr: Int, awlen: Int, awsize: Int, awburst: Int, wstrb: Int, wlast: Boolean, state: WriteState.Value)
case class AXI_Write_Item(awready: Bool, wready: Bool, bvalid: Bool)


class AXI_Memory(rand_delay: Boolean){
    private val mem: mutable.Map[Int, Memory_Item] = mutable.Map()
    var readConfig: AXI_Read_Config = AXI_Read_Config(0, 0, 0, 0, ReadState.IDLE)
    var writeConfig: AXI_Write_Config = AXI_Write_Config(0, 0, 0, 0, 0, false, WriteState.IDLE)

    // read from the memory
    def read(
        araddr: UInt, 
        arlen: UInt, 
        arsize: UInt, 
        arburst: UInt,
        arvalid: Bool,
        rready: Bool
    ): AXI_Read_Item = {
        var readItem = AXI_Read_Item(false.B, 0.U, false.B, false.B)
        readConfig.state match {
            // record the read configuration
            case ReadState.IDLE => {
                assert(arsize.litValue.toInt <= 2, "The minimum size of the data is 4 bytes")
                if(arvalid.litToBoolean){
                    readConfig = AXI_Read_Config(
                        araddr.litValue.toInt,
                        arlen.litValue.toInt, 
                        1 << arsize.litValue.toInt, 
                        arburst.litValue.toInt, 
                        ReadState.AR
                    )
                }
            }
            // address handshaking
            case ReadState.AR => {
                // random delay
                readItem.arready := (if(rand_delay){ (scala.util.Random.nextInt(4) != 0).B } else{ true.B })
                if(readItem.arready.litToBoolean && arvalid.litToBoolean){
                    readConfig = readConfig.copy(state = ReadState.R)
                }
            }
            case ReadState.R => {
                // random delay
                readItem.rvalid := (if(rand_delay){ (scala.util.Random.nextInt(4) != 0).B } else{ true.B })
                // if random delay is not enabled, the read data is ready
                if(readItem.rvalid.litToBoolean){
                    val word_addr = readConfig.araddr / 4
                    val word_offset = readConfig.araddr % 4
                    // assert the address is in the memory
                    assert(mem.contains(word_addr), f"Address ${readConfig.araddr}%x is not in the memory")
                    readItem.rdata := mem(word_addr).data >> (word_offset * 8)
                    // check if the last read
                    if(readConfig.arlen == 0){
                        readItem.rlast := true.B
                        if(rready.litToBoolean){
                            readConfig = readConfig.copy(state = ReadState.IDLE)
                        }
                    }else{
                        if(rready.litToBoolean){
                            readConfig = readConfig.copy(arlen = readConfig.arlen - 1)
                            readConfig = readConfig.copy(araddr = readConfig.araddr + readConfig.arsize)
                        }
                    }
                }
            }
        }
        readItem
    }

    // write to the memory
    def write(
        awaddr: UInt, 
        awlen: UInt, 
        awsize: UInt, 
        awburst: UInt,
        wdata: UInt,
        wstrb: UInt,
        wlast: Bool,
        awvalid: Bool,
        wvalid: Bool,
        bready: Bool,
        cycle: Int
    ): AXI_Write_Item = {
        var writeItem = AXI_Write_Item(false.B, false.B, false.B)
        writeConfig.state match {
            // record the write configuration
            case WriteState.IDLE => {
                assert(awsize.litValue.toInt <= 2, "The minimum size of the data is 4 bytes")
                if(awvalid.litToBoolean){
                    writeConfig = AXI_Write_Config(
                        awaddr.litValue.toInt,
                        awlen.litValue.toInt, 
                        1 << awsize.litValue.toInt, 
                        awburst.litValue.toInt, 
                        wstrb.litValue.toInt, 
                        wlast.litToBoolean, 
                        WriteState.AW
                    )
                }
            }
            // address handshaking
            case WriteState.AW => {
                // random delay
                writeItem.awready := (if(rand_delay){ (scala.util.Random.nextInt(4) != 0).B } else{ true.B })
                if(writeItem.awready.litToBoolean && awvalid.litToBoolean){
                    writeConfig = writeConfig.copy(state = WriteState.W)
                }
            }
            case WriteState.W => {
                // random delay
                writeItem.wready := (if(rand_delay){ (scala.util.Random.nextInt(4) != 0).B } else{ true.B })
                if(writeItem.wready.litToBoolean){
                    val word_addr = writeConfig.awaddr / 4
                    val word_offset = writeConfig.awaddr % 4
                    if(!mem.contains(word_addr)){
                        mem(word_addr) = Memory_Item(0.U, Array.fill(4)(0))
                    }
                    if(wlast.litToBoolean){
                        writeConfig = writeConfig.copy(state = WriteState.B)
                    }
                    // write into the memory
                    var wmask = 0xff.U << (word_offset * 8).U
                    var wdata_shift = wdata << (word_offset * 8).U
                    (0 until writeConfig.awsize).foreach{ i =>
                        if((writeConfig.wstrb & (1 << i)) != 0){
                            mem(word_addr).data := mem(word_addr).data & ~wmask | wdata_shift & wmask
                            mem(word_addr).last_write(word_offset+i) = cycle
                        }
                        wmask = wmask << 8.U
                    }
                    writeConfig = writeConfig.copy(awaddr = writeConfig.awaddr + writeConfig.awsize)
                }
            }
            case WriteState.B => {
                // random delay
                writeItem.bvalid := (if(rand_delay){ ( scala.util.Random.nextInt(4) != 0).B } else{ true.B })
                if(writeItem.bvalid.litToBoolean && bready.litToBoolean){
                    writeConfig = writeConfig.copy(state = WriteState.IDLE)
                }
            }
        }
        writeItem
    }

    def debug_read(addr: Int): UInt = {
        val word_addr = addr / 4
        val word_offset = addr % 4
        assert(mem.contains(word_addr), f"Address ${addr}%x is not in the memory")
        mem(word_addr).data >> (word_offset * 8)
    }
    def debug_write(addr: Int, wdata: UInt): Unit = {
        assert(addr % 4 == 0, "The address must be aligned to 4 bytes")
        val word_addr = addr / 4
        if(!mem.contains(word_addr)){
            mem(word_addr) = Memory_Item(0.U, Array.fill(4)(0))
        }
        mem(word_addr).data := wdata
        (0 until 4).foreach{ i =>
            mem(word_addr).last_write(i) = 0
        }
    }
    def initialize(size: Int, load: Boolean): Unit = {
        if(load){
            // 从文件里读取数据
            val source = scala.io.Source.fromFile("memory.txt")
            val lines = source.getLines().toArray
            for(i <- 0 until size){
                mem(i) = Memory_Item(lines(i).toInt.U, Array.fill(4)(0))
            }
        }else{
            for(i <- 0 until size){
                mem(i) = Memory_Item(i.U, Array.fill(4)(0))
            }
            // 将这些数据保存到文件里
            val writer = new java.io.PrintWriter("memory.txt")
            for(i <- 0 until size){
                writer.println(mem(i).data.litValue.toString(16))
            }
        }
    }
}