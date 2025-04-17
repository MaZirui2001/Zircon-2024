
case class InstNum(alu: Int, branch: Int, load: Int, store: Int, mul: Int, div: Int)
class InstRecorder {
    private var instNum = InstNum(0, 0, 0, 0, 0, 0)

    def addALUInsts(insts: Int): Unit = {
        instNum = instNum.copy(alu = instNum.alu + insts)
    }
    def addBranchInsts(insts: Int): Unit = {
        instNum = instNum.copy(branch = instNum.branch + insts)
    }
    def addLoadInsts(insts: Int): Unit = {
        instNum = instNum.copy(load = instNum.load + insts)
    }
    def addStoreInsts(insts: Int): Unit = {
        instNum = instNum.copy(store = instNum.store + insts)
    }
    def addMulInsts(insts: Int): Unit = {
        instNum = instNum.copy(mul = instNum.mul + insts)
    }
    def addDivInsts(insts: Int): Unit = {
        instNum = instNum.copy(div = instNum.div + insts)
    }
    def getInstNum(): InstNum = {
        instNum
    }
}