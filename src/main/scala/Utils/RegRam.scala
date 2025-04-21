import chisel3._
import chisel3.util._

class AsyncRegRam[T <: Data](gen: T, depth: Int, wport: Int, rport: Int, resetVal: Option[T] = None) extends Module {
    // 如果类型是Vec类型，那么wen需要能支持按照每个元素的wen
    val io = IO(new Bundle{
        // val wen = Input(Vec(wport, Bool()))
        val wen   = Input(Vec(wport, (if (gen.isInstanceOf[Vec[_]]) UInt(gen.asInstanceOf[Vec[_]].length.W) else Bool())))
        val waddr = Input(Vec(wport, UInt(log2Ceil(depth).W)))
        val wdata = Input(Vec(wport, gen))
        val raddr = Input(Vec(rport, UInt(log2Ceil(depth).W)))
        val rdata = Output(Vec(rport, gen))
    })
    val ram = RegInit(VecInit.fill(depth)(resetVal.getOrElse(0.U.asTypeOf(gen))))
    for (i <- 0 until wport) {
        if (gen.isInstanceOf[Vec[_]]) {
            for (j <- 0 until gen.asInstanceOf[Vec[_]].length) {
                when(io.wen.asInstanceOf[Vec[UInt]](i)(j)) {
                    ram(io.waddr.asInstanceOf[Vec[UInt]](i)).asInstanceOf[Vec[T]](j) := io.wdata.asInstanceOf[Vec[Vec[T]]](i)(j)
                }
            }
        } else {
            when(io.wen.asInstanceOf[Vec[Bool]](i)) {
                ram(io.waddr(i)) := io.wdata.asInstanceOf[Vec[T]](i)
            }
        }
    }
    for (i <- 0 until rport) {
        io.rdata(i) := ram(io.raddr(i))
    }
}

class SyncRegRam[T <: Data](gen: T, depth: Int, wport: Int, rport: Int) extends Module {
    val io = IO(new Bundle{
        val wen = Input(Vec(wport, Bool()))
        val waddr = Input(Vec(wport, UInt(log2Ceil(depth).W)))
        val wdata = Input(Vec(wport, gen))
        val raddr = Input(Vec(rport, UInt(log2Ceil(depth).W)))
        val rdata = Output(Vec(rport, gen))
        val flush = Input(Bool())
    })
    val ram = RegInit(VecInit.fill(depth)(0.U.asTypeOf(gen)))
    for (i <- 0 until wport) {
        when(io.wen(i)) {
            ram(io.waddr(i)) := io.wdata(i)
        }
    }
    for (i <- 0 until rport) {
        io.rdata(i) := ShiftRegister(ram(io.raddr(i)), 1, 0.U.asTypeOf(gen), true.B)
    }
    when(io.flush) {
        ram.foreach { _ := 0.U.asTypeOf(gen) }
    }
}