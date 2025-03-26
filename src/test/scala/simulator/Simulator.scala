import spire.math.UInt

class Simulator {

    // config
    private val base_addr = UInt(0x80000000)
    // soc
    private val mem     = new Memory()
    private val rf      = new Regfile()
    private val fetch   = new Fetch(mem, base_addr)
    private val decoder = new Decoder(rf, mem, fetch)

    // debug
    private val iring = new RingBuffer[(UInt, UInt)](8)

    // test if the program is end
    def sim_end(instruction: UInt): Boolean = {
        instruction == UInt(0x80000000)
    }

    // step the program
    def step(num: Int = 1): Int = {
        for (_ <- 0 until num) {
            val instruction = fetch.fetch()
            iring.push((fetch.getPC(), instruction))
            // 十六进制，打印
            if (sim_end(instruction)) {
                return (if(rf(10) == UInt(0)) 0 else -1)
            }
            decoder.decodeAndExecute(instruction)
            
        }
        return 1
    }

    // load the program from the file
    def mem_init(filename: String): Unit = {
        mem.loadFromFile(filename, base_addr)
    }

    // dump the register file
    def rf_dump(): Array[Int] = {
        rf.dump()
    }

    // dump the instruction ring buffer
    def iring_dump(): Array[(UInt, UInt)] = {
        iring.toArray
    }
    
}