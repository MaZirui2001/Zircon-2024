// import chisel3._
import spire.math.UInt
import scala.collection.mutable
import scala.util.Random

object ReadState extends Enumeration {
    val IDLE, AR, R = Value
}
object WriteState extends Enumeration {
    val IDLE, AW, W, B = Value
}

class Memory_Item(var data: UInt, var last_write: Array[Int])
class AXI_Read_Config(var araddr: Int, var arlen: Int, var arsize: Int, var arburst: Int, var state: ReadState.Value)
class AXI_Read_Item(var arready: Boolean, var rdata: UInt, var rvalid: Boolean, var rlast: Boolean)
class AXI_Write_Config(var awaddr: Int, var awlen: Int, var awsize: Int, var awburst: Int, var wstrb: Int, var wlast: Boolean, var state: WriteState.Value)
class AXI_Write_Item(var awready: Boolean, var wready: Boolean, var bvalid: Boolean)


class AXI_Memory(rand_delay: Boolean){
    private var mem: mutable.Map[UInt, Memory_Item] = mutable.Map()
    private var mem_ref: mutable.Map[UInt, Memory_Item] = mutable.Map()
    
    // 2. 缓存一些常用的掩码和移位值
    private val BYTE_MASK = 0xffL
    private val WORD_MASK = 0xffffffffL
    
    var readConfig: AXI_Read_Config = new AXI_Read_Config(0, 0, 0, 0, ReadState.IDLE)
    var writeConfig: AXI_Write_Config = new AXI_Write_Config(0, 0, 0, 0, 0, false, WriteState.IDLE)

    // 添加共享的Random对象
    private val rand = new Random()

    // read from the memory
    def read(
        axi: AXI_IO
    ): AXI_Read_Item = {
        val readItem = new AXI_Read_Item(false, UInt(0), false, false)
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
                // 使用共享的rand对象
                readItem.arready = if(rand_delay) (rand.nextInt(4) != 0) else true
                // println("arready: " + readItem.arready.litToBoolean + " arvalid: " + arvalid.litToBoolean)
                if(readItem.arready && axi.arvalid.litToBoolean){
                    readConfig.state = ReadState.R
                }
            }
            case ReadState.R => {
                // 使用共享的rand对象
                readItem.rvalid = if(rand_delay) (rand.nextInt(4) != 0) else true
                if(readItem.rvalid) {
                    val word_addr = UInt(readConfig.araddr) >> 2  // 使用右移替代除法
                    val word_offset = readConfig.araddr & 0x3  // 使用与操作替代取模
                    val shift_amount = word_offset << 3
                    if(!mem.contains(word_addr)){
                        mem(word_addr) = new Memory_Item(UInt(0), Array.fill(4)(0))
                    }
                    readItem.rdata = ((mem(word_addr).data >> shift_amount))
                    // check if the last read
                    if(readConfig.arlen == 0){
                        readItem.rlast = true
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
        axi: AXI_IO,
        cycle: Int
    ): AXI_Write_Item = {
        val writeItem = new AXI_Write_Item(false, false, false)
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
                // 使用共享的rand对象
                writeItem.awready = if(rand_delay) (rand.nextInt(4) != 0) else true
                if(writeItem.awready && axi.awvalid.litToBoolean){
                    writeConfig.state = WriteState.W
                }
            }
            case WriteState.W => {
                writeItem.wready = if(rand_delay) (rand.nextInt(4) != 0) else true
                if(writeItem.wready) {
                    val word_addr = UInt(writeConfig.awaddr) >> 2
                    val word_offset = writeConfig.awaddr & 0x3
                    val shift_amount = word_offset << 3
                    val wstrb = writeConfig.wstrb << word_offset
                    
                    if(!mem.contains(word_addr)) {
                        // println(s"word_addr: ${word_addr.toInt}")
                        mem(word_addr) = new Memory_Item(UInt(0), Array.fill(4)(0))
                    }
                    
                    if(writeConfig.wstrb != 0) {
                        val wdata_shift = axi.wdata.litValue.toInt << shift_amount
                        (0 until 4).foreach{ i =>
                            if((wstrb & (1 << i)) != 0){
                                mem(word_addr).data = (mem(word_addr).data & UInt(~(0xff << (i * 8))) | UInt(wdata_shift) & UInt(0xff << (i * 8)))
                                mem(word_addr).last_write(i) = cycle
                            }
                        }
                        // val wmask = BYTE_MASK << shift_amount
                        // mem(word_addr).data = (mem(word_addr).data & ~wmask) | (wdata_shift & wmask)
                        // println(wdata_shift.toHexString)
                        mem(word_addr).last_write(word_offset) = cycle
                    }
                    
                    if(axi.wlast.litToBoolean) {
                        writeConfig.state = WriteState.B
                    }
                    writeConfig.awaddr = writeConfig.awaddr + writeConfig.awsize
                }
            }
            case WriteState.B => {
                // 使用共享的rand对象
                writeItem.bvalid = if(rand_delay) (rand.nextInt(4) != 0) else true
                if(writeItem.bvalid && axi.bready.litToBoolean){
                    writeConfig.state = WriteState.IDLE
                }
            }
        }
        writeItem
    }

    def debug_read(addr: Int): (Int, Int) = {
        val word_addr = UInt(addr) / UInt(4)
        val word_offset = addr % 4
        assert(mem_ref.contains(word_addr), f"Address ${addr}%x is not in the memory")
        ((mem_ref(word_addr).data >> (word_offset * 8)).toInt, mem_ref(word_addr).last_write(word_offset))
    }
    def debug_write(addr: Int, wdata: Int, wstrb: Int, cycle: Int): Unit = {
        assert(addr % 4 == 0, "The address must be aligned to 4 bytes")
        val word_addr = UInt(addr) / UInt(4)
        val word_offset = addr % 4
        if(!mem.contains(word_addr)){
            mem_ref(word_addr) = new Memory_Item(UInt(0), Array.fill(4)(0))
        }
        // println(f"addr: ${addr}%x, data: ${wdata}%x, strb: ${wstrb}%x")
        (0 until 4).foreach{ i =>
            if((wstrb & (1 << i)) != 0){
                mem_ref(word_addr).data = (mem_ref(word_addr).data & UInt(~(0xff << (i * 8))) | UInt(wdata) & UInt(0xff << (i * 8)))
                mem_ref(word_addr).last_write(i) = cycle
                // if(word_addr == 0x58c >> 2) println(cycle)
            }
        }
    }
    def initialize(size: Int, load: Boolean): Unit = {
        // 使用Array.tabulate更高效地初始化
        for(i <- 0 until size) {
            mem(UInt(i)) = new Memory_Item(UInt(i << 2), Array.fill(4)(0))
            mem_ref(UInt(i)) = new Memory_Item(UInt(i << 2), Array.fill(4)(0))
        }
    }
    def loadFromFile(filename: String, baseAddr: UInt): Unit = {
        if(filename == "" || filename == null) {
            // println("没有提供镜像文件路径，使用默认镜像")
            mem(baseAddr) = new Memory_Item(UInt(0x80000000L), new Array[Int](4))
            return
        }
        import java.nio.file.{Files, Paths}
        val bytes = Files.readAllBytes(Paths.get(filename))
        // 将四个字节转换为UInt
        var currentAddr = baseAddr >> 2
        val uints = bytes.grouped(4).map(group => {
            val value = group.zipWithIndex.map { case (b, i) => (b & 0xFF).toLong << (i * 8) }.sum
            UInt(value)
        })
        uints.foreach(uint => {
            mem(currentAddr) = new Memory_Item(uint, new Array[Int](4))
            mem_ref(currentAddr) = new Memory_Item(uint, new Array[Int](4))
            currentAddr = currentAddr + UInt(1)
        })
    }
}