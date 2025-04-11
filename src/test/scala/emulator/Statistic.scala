import spire.math.UInt

class Statistic {
    private var totalCycles = 0
    private var totalInsts  = 0

    def addCycles(cycles: Int): Unit = {
        totalCycles += cycles
    }

    def addInsts(insts: Int): Unit = {
        totalInsts += insts
    }

    def getTotalCycles(): Int = {
        totalCycles
    }

    def getTotalInsts(): Int = {
        totalInsts
    }

    def getIpc(): Double = {
        totalInsts.toDouble / totalCycles.toDouble
    }
}