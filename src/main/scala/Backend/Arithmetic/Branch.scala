import chisel3._
import chisel3.util._
import EXE_Op._

class Branch_IO extends Bundle{
    val src1        = Input(UInt(32.W))
    val src2        = Input(UInt(32.W))
    val op          = Input(UInt(5.W))
    val branch_tgt  = Input(UInt(32.W))
    val prdc_tgt    = Input(UInt(32.W))
    val real_jp     = Output(Bool())
    val prdc_fail   = Output(Bool())
}

class Branch extends Module{
    val io = IO(new Branch_IO)

    val real_jp = WireDefault(false.B)
    
    switch(io.op){
        is(BEQ) { real_jp := io.src1 === io.src2 }
        is(BNE) { real_jp := io.src1 =/= io.src2 }
        is(BLT) { real_jp := io.src1.asSInt < io.src2.asSInt }
        is(BGE) { real_jp := io.src1.asSInt >= io.src2.asSInt }
        is(BLTU){ real_jp := io.src1 < io.src2 }
        is(BGEU){ real_jp := io.src1 >= io.src2 }
        is(JAL)  { real_jp := true.B }
        is(JALR){ real_jp := true.B }
    }
    val fail = io.branch_tgt =/= io.prdc_tgt
    io.real_jp  :=  real_jp
    io.prdc_fail := fail

}