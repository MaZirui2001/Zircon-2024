
import spire.math.UInt

class Fetch(mem: Memory, base_addr: UInt) {
    private var pc = base_addr

    def getPC(): UInt = {
        pc
    }

    def setPC(value: UInt): Unit = {
        pc = value
    }
    
    def reset(): Unit = {
        pc = base_addr
    }

    def fetch(): UInt = {
        mem.read(pc, 2)
    }
}