import chisel3._
import chisel3.util._
import ZirconConfig.EXEOp._
import Shifter._


class ALUIO extends Bundle {
    val src1 = Input(UInt(32.W))
    val src2 = Input(UInt(32.W))
    val op   = Input(UInt(5.W))
    val res  = Output(UInt(32.W))
}

class ALU extends Module {
    val io = IO(new ALUIO)

    // adder
    val adderSrc1 = WireDefault(io.src1)
    val adderSrc2 = WireDefault(4.U(32.W))
    val adderCin  = WireDefault(0.U)

    val adder     = BLevelPAdder32(adderSrc1, adderSrc2, adderCin)

    val adderRes  = adder.io.res
    val adderCout = adder.io.cout

    // shifter
    val sfterSrc = Mux(io.op === SLL, Reverse(io.src1), io.src1)
    val sfterShf = io.src2(4, 0)
    val sfterSgn = io.op === SRA

    val shifter  = Shifter(sfterSrc, sfterShf, sfterSgn)

    val sfterRes = shifter.io.res

    io.res := adderRes
    // result select
    switch(io.op){
        is(ADD){
            adderSrc2 := io.src2
        }
        is(SUB){
            adderSrc2 := ~io.src2
            adderCin  := 1.U
        }
        is(SLTU){
            adderSrc2 := ~io.src2
            adderCin  := 1.U
            io.res    := !adderCout
        }
        is(SLT){
            adderSrc2 := ~io.src2
            adderCin  := 1.U
            io.res    := io.src1(31) && !io.src2(31) || !(io.src1(31) ^ io.src2(31)) && adderRes(31)
        }
        is(AND){
            io.res := io.src1 & io.src2
        }
        is(OR){
            io.res := io.src1 | io.src2
        }
        is(XOR){
            io.res := io.src1 ^ io.src2
        }
        is(SLL){
            io.res := Reverse(shifter.io.res)
        }
        is(SRL){
            io.res := sfterRes
        }
        is(SRA){
            io.res := sfterRes
        }
    }
}