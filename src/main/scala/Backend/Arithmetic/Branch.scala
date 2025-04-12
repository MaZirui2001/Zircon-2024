import chisel3._
import chisel3.util._
import ZirconConfig.EXEOp._

class BranchIO extends Bundle{
    val src1       = Input(UInt(32.W))
    val src2       = Input(UInt(32.W))
    val op         = Input(UInt(5.W))
    val pc         = Input(UInt(32.W))
    val imm        = Input(UInt(32.W))
    val predOffset = Input(UInt(32.W))
    val realJp     = Output(Bool())
    val predFail   = Output(Bool())
    val jumpTgt    = Output(UInt(32.W))
}

class Branch extends Module{
    val io = IO(new BranchIO)

    val realJp         = WireDefault(false.B)
    val fail           = WireDefault(Mux(io.op(4), io.predOffset =/= Mux(realJp, io.imm, 4.U), false.B))
    val tgtAdderSrc1   = WireDefault(io.pc)
    val tgtAdderSrc2   = WireDefault(io.imm)
    val jumpTgt        = BLevelPAdder32(tgtAdderSrc1, tgtAdderSrc2, 0.U).io.res
    val cmpSrc1        = WireDefault(io.src1)
    val cmpSrc2        = WireDefault(io.src2)  
    val cmpAdder       = BLevelPAdder32(cmpSrc1, ~cmpSrc2, 1.U)
    switch(io.op){
        is(BEQ) { realJp := io.src1 === io.src2 }
        is(BNE) { realJp := io.src1 =/= io.src2 }
        is(BLT) { realJp := cmpSrc1(31) && !cmpSrc2(31) || !(cmpSrc1(31) ^ cmpSrc2(31)) && cmpAdder.io.res(31) }
        is(BGE) { realJp := (!cmpSrc1(31) || cmpSrc2(31)) && ((cmpSrc1(31) ^ cmpSrc2(31)) || !cmpAdder.io.res(31)) }
        is(BLTU){ realJp := !cmpAdder.io.cout }
        is(BGEU){ realJp := cmpAdder.io.cout }
        is(JAL) { realJp := true.B }
        is(JALR){ realJp := true.B; fail := jumpTgt =/= io.predOffset; tgtAdderSrc1 := io.src1 }
    }
    io.realJp      := realJp
    io.predFail    := fail
    io.jumpTgt     := jumpTgt
}