import chisel3._
import chisel3.util._
import ALU_Op._
import Adder._
import Shifter._


class ALU_IO extends Bundle {
    val src1 = Input(UInt(32.W))
    val src2 = Input(UInt(32.W))
    val op = Input(ALU_Op())
    val res = Output(UInt(32.W))
}

class ALU extends Module {
    val io = IO(new ALU_IO)

    // adder
    val adder_src1 = io.src1
    val adder_src2 = Mux((io.op === SUB || io.op === SLT || io.op === SLTU), ~io.src2, io.src2)
    val adder_cin = Mux(io.op === SUB || io.op === SLT || io.op === SLTU, 1.U, 0.U)

    val adder = BLevel_PAdder32(adder_src1, adder_src2, adder_cin)

    val adder_res = adder.io.res
    val adder_cout = adder.io.cout

    // shifter
    val sfter_src = Mux(io.op === SLL, Reverse(io.src1), io.src1)
    val sfter_shf = io.src2(4, 0)
    val sfter_sgn = io.op === SRA

    val shifter = Shifter(sfter_src, sfter_shf, sfter_sgn)

    val sfter_res = Mux(io.op === SLL, Reverse(shifter.io.res), shifter.io.res)

    io.res := adder_res
    // result select
    switch(io.op){
        is(ADD){
            io.res := adder_res
        }
        is(SUB){
            io.res := adder_res
        }
        is(SLTU){
            io.res := !adder_cout
        }
        is(SLT){
            io.res := io.src1(31) && !io.src2(31) || !(io.src1(31) ^ io.src2(31)) && adder_res(31)
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
        is(NOR){
            io.res := ~(io.src1 | io.src2)
        }
        is(SLL){
            io.res := sfter_res
        }
        is(SRL){
            io.res := sfter_res
        }
        is(SRA){
            io.res := sfter_res
        }
        is(LUI){
            io.res := io.src2
        }

    }
    
}