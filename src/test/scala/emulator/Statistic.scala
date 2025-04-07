import spire.math.UInt

class Statistic {
    private var total_cycles = 0
    private var total_insts  = 0

    def add_cycles(cycles: Int): Unit = {
        total_cycles += cycles
    }

    def add_insts(insts: Int): Unit = {
        total_insts += insts
    }

    def get_total_cycles(): Int = {
        total_cycles
    }

    def get_total_insts(): Int = {
        total_insts
    }

    def get_ipc(): Double = {
        total_insts.toDouble / total_cycles.toDouble
    }
}