import spire.math.UInt

class LogicRegFile {
    private val rf = Array.fill(32)(UInt(0))

    // 重定义()运算符，用于获取某个寄存器的值
    def apply(rd: Int): UInt = {
        read(UInt(rd))
    }

    def read(rd: UInt): UInt = {
        require(rd >= UInt(0) && rd < UInt(32), s"寄存器编号必须在0-31范围内，当前值: $rd")
        rf(rd.toInt)
    }
    
    def write(rd: UInt, value: UInt): Unit = {
        require(rd >= UInt(0) && rd < UInt(32), s"寄存器编号必须在0-31范围内，当前值: $rd")
        // x0寄存器始终为0
        if (rd != UInt(0)) {
            rf(rd.toInt) = value
        }
    }
    
    // // 重置寄存器堆，所有寄存器置为0
    // def reset(): Unit = {
    //     for (i <- 0 until 32) {
    //         rf(i) = UInt(0)
    //     }
    // }
    
    // // 打印所有寄存器的值，用于调试
    def dump(): Array[UInt] = {
        rf
    }
}