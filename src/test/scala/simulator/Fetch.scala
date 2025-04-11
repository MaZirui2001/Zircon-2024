
import spire.math.UInt

class Fetch(mem: Memory, baseAddr: UInt) {
    private var pc = baseAddr

    def getPC(): UInt = {
        pc
    }

    def setPC(value: UInt): Unit = {
        pc = value
    }
    
    def reset(): Unit = {
        pc = baseAddr
    }

    def fetch(): UInt = {
        mem.read(pc, 2)
    }
}