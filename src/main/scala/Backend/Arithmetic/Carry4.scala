import chisel3._
import chisel3.util._

class Carry4 extends BlackBox with HasBlackBoxInline {
    val io = IO(new Bundle{
        val ci = Input(UInt(1.W))
        val s  = Input(UInt(4.W))
        val di = Input(UInt(4.W))
        val o  = Output(UInt(4.W))
        val co = Output(UInt(4.W))
    })
    setInline("Carry4.v",
        s"""
        |module Carry4(
        |    input ci,
        |    input [3:0] s,
        |    input [3:0] di,
        |    output [3:0] o,
        |    output [3:0] co
        |);
        |    CARRY4 carry4(
        |        .CI(ci),
        |        .CYINIT(0),
        |        .DI(di),
        |        .S(s),
        |        .O(o),
        |        .CO(co)
        |   );
        |endmodule
        """.stripMargin)
}
object Carry4 {
    def apply(ci: UInt, a: UInt, b: UInt): (UInt, UInt) = {
        val carry4 = Module(new Carry4)
        carry4.io.ci := ci
        carry4.io.s := a ^ b
        carry4.io.di := b
        (carry4.io.o, carry4.io.co)
    }
}
class Carry4_Sim extends Module {
    val io = IO(new Bundle{
        val ci = Input(UInt(1.W))
        val s  = Input(UInt(4.W))
        val di = Input(UInt(4.W))
        val o  = Output(UInt(4.W))
        val co = Output(UInt(4.W))
    })
    io.o := Cat(
        Mux(io.s(2), Mux(io.s(1), Mux(io.s(0), io.ci, io.di(0)), io.di(1)), io.di(2)) ^ io.s(3),
        Mux(io.s(1), Mux(io.s(0), io.ci, io.di(0)), io.di(1)) ^ io.s(2),
        Mux(io.s(0), io.ci, io.di(0)) ^ io.s(1),
        io.ci ^ io.s(0)
    )
    io.co := Cat(
        Mux(io.s(3), Mux(io.s(2), Mux(io.s(1), Mux(io.s(0), io.ci, io.di(0)), io.di(1)), io.di(2)), io.di(3)),
        Mux(io.s(2), Mux(io.s(1), Mux(io.s(0), io.ci, io.di(0)), io.di(1)), io.di(2)),
        Mux(io.s(1), Mux(io.s(0), io.ci, io.di(0)), io.di(1)),
        Mux(io.s(0), io.ci, io.di(0))
    )
}

object Carry4_Sim {
    def apply(ci: UInt, a: UInt, b: UInt): (UInt, UInt) = {
        val carry4 = Module(new Carry4_Sim)
        carry4.io.ci := ci
        carry4.io.s := a ^ b
        carry4.io.di := b
        (carry4.io.o, carry4.io.co)
    }
}

