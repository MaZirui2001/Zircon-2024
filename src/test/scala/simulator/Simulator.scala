import spire.math.UInt

class Simulator {

    // config
    private val baseAddr = UInt(0x80000000)
    private val instRecorder = new InstRecorder()
    // soc
    private val mem     = new Memory()
    private val rf      = new LogicRegFile()
    private val fetch   = new Fetch(mem, baseAddr)
    private val decoder = new InstDecoder(rf, mem, fetch, instRecorder)

    // debug
    private val iring = new RingBuffer[(UInt, UInt)](8)

    // test if the program is end
    def simEnd(instruction: UInt): Boolean = {
        instruction == UInt(0x80000000)
    }

    // step the program
    def step(num: Int = 1): Int = {
        for (_ <- 0 until num) {
            val instruction = fetch.fetch()
            iring.push((fetch.getPC(), instruction))
            // 十六进制，打印
            if (simEnd(instruction)) {
                return (if(rf(10) == UInt(0)) 0 else -1)
            }
            decoder.decodeAndExecute(instruction)
            
        }
        return 1
    }

    // load the program from the file
    def memInit(filename: String): Unit = {
        mem.loadFromFile(filename, baseAddr)
    }

    // dump the register file
    def rfDump(): Array[Int] = {
        rf.dump().map(_.toInt)
    }
    // dump the pc
    def pcDump(): Int = {
        fetch.getPC().toInt
    }

    // dump the instruction ring buffer
    def iringDump(): Array[(UInt, UInt)] = {
        iring.toArray
    }
    def instRecorderDump(): InstNum = {
        instRecorder.getInstNum()
    }
    
}