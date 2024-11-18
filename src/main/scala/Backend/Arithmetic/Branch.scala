import chisel3._
import chisel3.util._
import ALU_BR_Op._

class Branch_IO extends Bundle{
    val src1        = Input(UInt(32.W))
    val src2        = Input(UInt(32.W))
    val op          = Input(ALU_BR_Op())
    val alu_res     = Input(UInt(32.W))
    val prdc_tgt    = Input(UInt(32.W))
    val real_jp     = Output(Bool())
    val prdc_fail   = Output(Bool())
}

class Branch extends Module{
    val io = IO(new Branch_IO)

    val real_jp = WireDefault(false.B)
    
    switch(getVal(io.op)){
        is(getVal(BEQ)) { real_jp := io.src1 === io.src2 }
        is(getVal(BNE)) { real_jp := io.src1 =/= io.src2 }
        is(getVal(BLT)) { real_jp := io.src1.asSInt < io.src2.asSInt }
        is(getVal(BGE)) { real_jp := io.src1.asSInt >= io.src2.asSInt }
        is(getVal(BLTU)){ real_jp := io.src1 < io.src2 }
        is(getVal(BGEU)){ real_jp := io.src1 >= io.src2 }
        is(getVal(B))   { real_jp := true.B }
        is(getVal(BL))  { real_jp := true.B }
        is(getVal(JIRL)){ real_jp := true.B }
    }
    val fail = io.alu_res =/= io.prdc_tgt
    io.real_jp  := getType(io.op) && real_jp
    io.prdc_fail := getType(io.op) && fail

}