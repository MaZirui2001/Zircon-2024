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

class MemoryItem(var data: UInt, var lastWrite: Array[Int])
class AXIReadConfig(var araddr: Int, var arlen: Int, var arsize: Int, var arburst: Int, var state: ReadState.Value)
class AXIReadItem(var arready: Boolean, var rdata: UInt, var rvalid: Boolean, var rlast: Boolean)
class AXIWriteConfig(var awaddr: Int, var awlen: Int, var awsize: Int, var awburst: Int, var wstrb: Int, var wlast: Boolean, var state: WriteState.Value)
class AXIWriteItem(var awready: Boolean, var wready: Boolean, var bvalid: Boolean)


class AXIMemory(randDelay: Boolean){
    private var mem: mutable.Map[UInt, MemoryItem] = mutable.Map()
    private var memRef: mutable.Map[UInt, MemoryItem] = mutable.Map()
    private val device      = new Device

    
    // 2. 缓存一些常用的掩码和移位值
    private val BYTEMASK = 0xffL
    private val WORDMASK = 0xffffffffL
    
    var readConfig: AXIReadConfig = new AXIReadConfig(0, 0, 0, 0, ReadState.IDLE)
    var writeConfig: AXIWriteConfig = new AXIWriteConfig(0, 0, 0, 0, 0, false, WriteState.IDLE)

    // 添加共享的Random对象
    private val rand = new Random()

    // read from the memory
    def read(
        axi: AXIIO
    ): AXIReadItem = {
        val readItem = new AXIReadItem(false, UInt(0), false, false)
        readConfig.state match {
            // record the read configuration
            case ReadState.IDLE => {
                assert(axi.arsize.litValue.toInt <= 2, "The minimum size of the data is 4 bytes")
                if(axi.arvalid.litToBoolean){
                    readConfig = new AXIReadConfig(
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
                readItem.arready = if(randDelay) (rand.nextInt(4) != 0) else true
                // println("arready: " + readItem.arready.litToBoolean + " arvalid: " + arvalid.litToBoolean)
                if(readItem.arready && axi.arvalid.litToBoolean){
                    readConfig.state = ReadState.R
                }
            }
            case ReadState.R => {
                // 使用共享的rand对象
                readItem.rvalid = if(randDelay) (rand.nextInt(4) != 0) else true
                if(readItem.rvalid) {
                    val wordAddr = UInt(readConfig.araddr) >> 2  // 使用右移替代除法
                    val wordOffset = readConfig.araddr & 0x3  // 使用与操作替代取模
                    val shiftAmount = wordOffset << 3
                    if(!mem.contains(wordAddr)){
                        mem(wordAddr) = new MemoryItem(UInt(0), Array.fill(4)(0))
                    }
                    readItem.rdata = ((mem(wordAddr).data >> shiftAmount))
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
        axi: AXIIO,
        cycle: Int
    ): AXIWriteItem = {
        val writeItem = new AXIWriteItem(false, false, false)
        writeConfig.state match {
            // record the write configuration
            case WriteState.IDLE => {
                assert(axi.arsize.litValue.toInt <= 2, "The minimum size of the data is 4 bytes")
                if(axi.awvalid.litToBoolean){
                    writeConfig = new AXIWriteConfig(
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
                writeItem.awready = if(randDelay) (rand.nextInt(4) != 0) else true
                if(writeItem.awready && axi.awvalid.litToBoolean){
                    writeConfig.state = WriteState.W
                }
            }
            case WriteState.W => {
                writeItem.wready = if(randDelay) (rand.nextInt(4) != 0) else true
                if(writeItem.wready) {
                    val wordAddr = UInt(writeConfig.awaddr) >> 2
                    val wordOffset = writeConfig.awaddr & 0x3
                    val shiftAmount = wordOffset << 3
                    val wstrb = writeConfig.wstrb << wordOffset

                    if((wordAddr >> 26) == UInt(0xa)) {
                        // println(s"write to device: ${writeConfig.awaddr}, ${axi.wdata.litValue.toInt}")
                        device.write(UInt(writeConfig.awaddr), axi.wdata.litValue.toByte)
                    } else {
                        if(!mem.contains(wordAddr)) {
                        // println(s"wordAddr: ${wordAddr.toInt}")
                            mem(wordAddr) = new MemoryItem(UInt(0), Array.fill(4)(0))
                        }
                        if(writeConfig.wstrb != 0) {
                            val wdataShift = axi.wdata.litValue.toInt << shiftAmount
                            (0 until 4).foreach{ i =>
                                if((wstrb & (1 << i)) != 0){
                                    mem(wordAddr).data = (mem(wordAddr).data & UInt(~(0xff << (i * 8))) | UInt(wdataShift) & UInt(0xff << (i * 8)))
                                    mem(wordAddr).lastWrite(i) = cycle
                                }
                            }
                            // val wmask = BYTEMASK << shiftAmount
                            // mem(wordAddr).data = (mem(wordAddr).data & ~wmask) | (wdataShift & wmask)
                            // println(wdataShift.toHexString)
                            mem(wordAddr).lastWrite(wordOffset) = cycle
                        }
                    }
                    
                    if(axi.wlast.litToBoolean) {
                        writeConfig.state = WriteState.B
                    }
                    writeConfig.awaddr = writeConfig.awaddr + writeConfig.awsize
                }
            }
            case WriteState.B => {
                // 使用共享的rand对象
                writeItem.bvalid = if(randDelay) (rand.nextInt(4) != 0) else true
                if(writeItem.bvalid && axi.bready.litToBoolean){
                    writeConfig.state = WriteState.IDLE
                }
            }
        }
        writeItem
    }

    def debugRead(addr: Int): (Int, Int) = {
        val wordAddr = UInt(addr) / UInt(4)
        val wordOffset = addr % 4
        assert(memRef.contains(wordAddr), f"Address ${addr}%x is not in the memory")
        ((memRef(wordAddr).data >> (wordOffset * 8)).toInt, memRef(wordAddr).lastWrite(wordOffset))
    }
    def debugWrite(addr: Int, wdata: Int, wstrb: Int, cycle: Int): Unit = {
        assert(addr % 4 == 0, "The address must be aligned to 4 bytes")
        val wordAddr = UInt(addr) / UInt(4)
        val wordOffset = addr % 4
        if(!mem.contains(wordAddr)){
            memRef(wordAddr) = new MemoryItem(UInt(0), Array.fill(4)(0))
        }
        // println(f"addr: ${addr}%x, data: ${wdata}%x, strb: ${wstrb}%x")
        (0 until 4).foreach{ i =>
            if((wstrb & (1 << i)) != 0){
                memRef(wordAddr).data = (memRef(wordAddr).data & UInt(~(0xff << (i * 8))) | UInt(wdata) & UInt(0xff << (i * 8)))
                memRef(wordAddr).lastWrite(i) = cycle
                // if(wordAddr == 0x58c >> 2) println(cycle)
            }
        }
    }
    def initialize(size: Int, load: Boolean): Unit = {
        // 使用Array.tabulate更高效地初始化
        for(i <- 0 until size) {
            mem(UInt(i)) = new MemoryItem(UInt(i << 2), Array.fill(4)(0))
            memRef(UInt(i)) = new MemoryItem(UInt(i << 2), Array.fill(4)(0))
        }
    }
    def loadFromFile(filename: String, baseAddr: UInt): Unit = {
        if(filename == "" || filename == null) {
            // println("没有提供镜像文件路径，使用默认镜像")
            mem(baseAddr) = new MemoryItem(UInt(0x80000000L), new Array[Int](4))
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
            mem(currentAddr) = new MemoryItem(uint, new Array[Int](4))
            memRef(currentAddr) = new MemoryItem(uint, new Array[Int](4))
            currentAddr = currentAddr + UInt(1)
        })
    }
}