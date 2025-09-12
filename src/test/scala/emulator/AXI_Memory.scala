// import chisel3._
import spire.math.UInt
import scala.collection.mutable
import scala.util.Random
import chiseltest._

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
    private var mem: mutable.Map[UInt, MemoryItem] = mutable.Map().withDefaultValue(new MemoryItem(UInt(0), Array.fill(4)(0)))
    private var memRef: mutable.Map[UInt, MemoryItem] = mutable.Map().withDefaultValue(new MemoryItem(UInt(0), Array.fill(4)(0)))
    private val device = new Device

    
    // 预计算常用的掩码
    private val BYTE_MASKS = Array(0xffL, 0xff00L, 0xff0000L, 0xff000000L)
    private val WORD_MASK = 0xffffffffL
    
    val readConfig:  AXIReadConfig  = new AXIReadConfig(0, 0, 0, 0, ReadState.IDLE)
    val writeConfig: AXIWriteConfig = new AXIWriteConfig(0, 0, 0, 0, 0, false, WriteState.IDLE)

    private val readItem = new AXIReadItem(false, UInt(0), false, false)
    private val writeItem = new AXIWriteItem(false, false, false)

    // 预生成随机数序列
    private val RANDOM_SEQ_SIZE = 1024
    private val randomSeq = Array.fill(RANDOM_SEQ_SIZE)(Random.nextInt(4))
    private var randomSeqIndex = 0
    
    // 获取下一个随机数
    private def nextRandom(): Int = {
        val result = randomSeq(randomSeqIndex)
        randomSeqIndex = (randomSeqIndex + 1) % RANDOM_SEQ_SIZE
        result
    }

    // read from the memory
    def read(
        cpu: CPU
    ): AXIReadItem = {
        readItem.arready = false
        readItem.rdata = UInt(0)
        readItem.rvalid = false
        readItem.rlast = false
        val axi = cpu.io.axi
        readConfig.state match {
            // record the read configuration
            case ReadState.IDLE => {
                if(axi.arvalid.peek().litToBoolean){
                    readConfig.araddr = axi.araddr.peek().litValue.toInt
                    readConfig.arlen = axi.arlen.peek().litValue.toInt
                    readConfig.arsize = 1 << axi.arsize.peek().litValue.toInt
                    readConfig.arburst = axi.arburst.peek().litValue.toInt
                    readConfig.state = ReadState.AR
                }
            }
            // address handshaking
            case ReadState.AR => {
                readItem.arready = if(randDelay) (nextRandom() != 0) else true
                if(readItem.arready && axi.arvalid.peek().litToBoolean){
                    readConfig.state = ReadState.R
                }
            }
            case ReadState.R => {
                readItem.rvalid = if(randDelay) (nextRandom() != 0) else true
                if(readItem.rvalid) {
                    val wordAddr = UInt(readConfig.araddr) >> 2  
                    val wordOffset = readConfig.araddr & 0x3
                    val shiftAmount = wordOffset << 3
                    readItem.rdata = ((mem(wordAddr).data >> shiftAmount))
                    // check if the last read
                    if(readConfig.arlen != 0){
                        if(axi.rready.peek().litToBoolean){
                            readConfig.arlen = readConfig.arlen - 1
                            readConfig.araddr = readConfig.araddr + readConfig.arsize
                        }
                    }else{
                        readItem.rlast = true
                        if(axi.rready.peek().litToBoolean){
                            readConfig.state = ReadState.IDLE
                        }
                    }
                }
            }
        }
        readItem
    }
    // write to the memory
    def write(
        cpu: CPU,   
        cycle: Int
    ): AXIWriteItem = {
        
        writeItem.awready = false
        writeItem.wready = false
        writeItem.bvalid = false
        val axi = cpu.io.axi

        writeConfig.state match {
            // record the write configuration
            case WriteState.IDLE => {
                if(axi.awvalid.peek().litToBoolean){

                    writeConfig.awaddr = axi.awaddr.peek().litValue.toInt
                    writeConfig.awlen = axi.awlen.peek().litValue.toInt
                    writeConfig.awsize = 1 << axi.awsize.peek().litValue.toInt
                    writeConfig.awburst = axi.awburst.peek().litValue.toInt
                    writeConfig.wstrb = axi.wstrb.peek().litValue.toInt
                    writeConfig.wlast = axi.wlast.peek().litToBoolean
                    writeConfig.state = WriteState.AW
                }
            }
            // address handshaking
            case WriteState.AW => {
                writeItem.awready = if(randDelay) (nextRandom() != 0) else true
                if(writeItem.awready && axi.awvalid.peek().litToBoolean){
                    writeConfig.state = WriteState.W
                }
            }
            case WriteState.W => {
                writeItem.wready = if(randDelay) (nextRandom() != 0) else true
                if(writeItem.wready) {
                    val wordAddr = UInt(writeConfig.awaddr) >> 2
                    val wordOffset = writeConfig.awaddr & 0x3
                    val wstrb = writeConfig.wstrb << wordOffset

                    if((wordAddr >> 26) == UInt(0xa)) {
                        device.write(UInt(writeConfig.awaddr), axi.wdata.peek().litValue.toByte)
                    } else {
                        val memItem = mem.getOrElseUpdate(wordAddr, new MemoryItem(UInt(0), Array.fill(4)(0)))
                        if(writeConfig.wstrb != 0) {
                            val wdataShift = axi.wdata.peek().litValue.toInt << (wordOffset << 3)
                            (0 until 4).foreach{ i =>
                                if((wstrb & (1 << i)) != 0){
                                    memItem.data = (memItem.data & UInt(~BYTE_MASKS(i)) | UInt(wdataShift) & UInt(BYTE_MASKS(i)))
                                    // mem(wordAddr).lastWrite(i) = cycle
                                }
                            }
                        }
                    }
                    
                    if(axi.wlast.peek().litToBoolean) {
                        writeConfig.state = WriteState.B
                    }
                    writeConfig.awaddr = writeConfig.awaddr + writeConfig.awsize
                }
            }
            case WriteState.B => {
                writeItem.bvalid = if(randDelay) (nextRandom() != 0) else true
                if(writeItem.bvalid && axi.bready.peek().litToBoolean){
                    writeConfig.state = WriteState.IDLE
                }
            }
        }
        writeItem
    }

    def debugRead(addr: Int): (Int, Int) = {
        val wordAddr = UInt(addr) >> 2
        val wordOffset = addr & 0x3
        ((memRef(wordAddr).data >> (wordOffset << 3)).toInt, memRef(wordAddr).lastWrite(wordOffset))
    }
    def debugWrite(addr: Int, wdata: Int, wstrb: Int, cycle: Int): Unit = {
        val wordAddr = UInt(addr) >> 2
        val wordOffset = addr & 0x3
        if(!mem.contains(wordAddr)){
            memRef(wordAddr) = new MemoryItem(UInt(0), Array.fill(4)(0))
        }
        // println(f"addr: ${addr}%x, data: ${wdata}%x, strb: ${wstrb}%x")
        (0 until 4).foreach{ i =>
            if((wstrb & (1 << i)) != 0){
                memRef(wordAddr).data = (memRef(wordAddr).data & UInt(~BYTE_MASKS(i)) | UInt(wdata) & UInt(BYTE_MASKS(i)))
                // memRef(wordAddr).lastWrite(i) = cycle
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
            println("没有提供镜像文件路径，使用默认镜像")
            mem(baseAddr >> 2) = new MemoryItem(UInt(0x80000000), new Array[Int](4))
            memRef(baseAddr >> 2) = new MemoryItem(UInt(0x80000000), new Array[Int](4))
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