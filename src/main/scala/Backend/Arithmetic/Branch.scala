import chisel3._
import chisel3.util._
import Zircon_Config.EXE_Op._

class Branch_IO extends Bundle{
    val src1        = Input(UInt(32.W))
    val src2        = Input(UInt(32.W))
    val op          = Input(UInt(5.W))
    val pc          = Input(UInt(32.W))
    val imm         = Input(UInt(32.W))
    val pred_offset = Input(UInt(32.W))
    val real_jp     = Output(Bool())
    val pred_fail   = Output(Bool())
    val jump_tgt    = Output(UInt(32.W))
}

class Branch extends Module{
    val io = IO(new Branch_IO)

    val real_jp         = WireDefault(false.B)
    val fail            = WireDefault(Mux(io.op(4), io.pred_offset =/= Mux(real_jp, io.imm, 4.U), false.B))
    val tgt_adder_src1  = WireDefault(io.pc)
    val tgt_adder_src2  = WireDefault(io.imm)
    val jump_tgt        = BLevel_PAdder32(tgt_adder_src1, tgt_adder_src2, 0.U).io.res
    val cmp_src1        = WireDefault(io.src1)
    val cmp_src2        = WireDefault(io.src2)  
    val cmp_adder       = BLevel_PAdder32(cmp_src1, ~cmp_src2, 1.U)
    switch(io.op){
        is(BEQ) { real_jp := io.src1 === io.src2 }
        is(BNE) { real_jp := io.src1 =/= io.src2 }
        is(BLT) { real_jp := cmp_src1(31) && !cmp_src2(31) || !(cmp_src1(31) ^ cmp_src2(31)) && cmp_adder.io.res(31) }
        is(BGE) { real_jp := (!cmp_src1(31) || cmp_src2(31)) && ((cmp_src1(31) ^ cmp_src2(31)) || !cmp_adder.io.res(31)) }
        is(BLTU){ real_jp := !cmp_adder.io.cout }
        is(BGEU){ real_jp := cmp_adder.io.cout }
        is(JAL) { real_jp := true.B }
        is(JALR){ real_jp := true.B; fail := jump_tgt =/= io.pred_offset; tgt_adder_src1 := io.src1 }
    }
    io.real_jp      := real_jp
    io.pred_fail    := fail
    io.jump_tgt     := jump_tgt
}