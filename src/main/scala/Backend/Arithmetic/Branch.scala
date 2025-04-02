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

    val real_jp = WireDefault(false.B)
    val fail = WireDefault(Mux(io.op(4), io.imm =/= io.pred_offset, false.B))
    val jump_tgt = WireDefault(io.pc + io.imm)
    switch(io.op){
        is(BEQ) { real_jp := io.src1 === io.src2 }
        is(BNE) { real_jp := io.src1 =/= io.src2 }
        is(BLT) { real_jp := io.src1.asSInt < io.src2.asSInt }
        is(BGE) { real_jp := io.src1.asSInt >= io.src2.asSInt }
        is(BLTU){ real_jp := io.src1 < io.src2 }
        is(BGEU){ real_jp := io.src1 >= io.src2 }
        is(JAL) { real_jp := true.B }
        is(JALR){ real_jp := true.B; fail := io.src1 + io.imm =/= io.pred_offset; jump_tgt := io.src1 + io.imm }
    }
    io.real_jp      := real_jp
    io.pred_fail    := fail
    io.jump_tgt     := jump_tgt
}