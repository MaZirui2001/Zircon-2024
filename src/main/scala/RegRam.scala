import chisel3._
import chisel3.util._

class AsyncRegRam[T <: Data](gen: T, depth: Int, wport: Int, rport: Int, resetVal: T) extends Module {
    val io = IO(new Bundle{
        val wen = Input(Vec(wport, Bool()))
        val waddr = Input(Vec(wport, UInt(log2Ceil(depth).W)))
        val wdata = Input(Vec(wport, gen))
        val raddr = Input(Vec(rport, UInt(log2Ceil(depth).W)))
        val rdata = Output(Vec(rport, gen))
    })
    val ram = RegInit(VecInit.fill(depth)(resetVal))
    for (i <- 0 until wport) {
        when(io.wen(i)) {
            ram(io.waddr(i)) := io.wdata(i)
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