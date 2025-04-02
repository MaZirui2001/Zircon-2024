import chisel3._
import chisel3.util._
import Zircon_Util._
import EXE_Op._
import Adder._

class SRT2_IO extends Bundle {
    val src1 = Input(UInt(32.W))
    val src2 = Input(UInt(32.W))
    val op   = Input(UInt(4.W))
    val res  = Output(UInt(32.W))
    val busy = Output(Bool())
    val ready = Output(Bool())
}

class SRT2 extends Module {
    val io = IO(new SRT2_IO)

    /* Stage 1: Initialization */
    // calculate the number of iterations
    val en = io.op(2)
    val iter = RegInit(63.U(6.W))
    
    io.busy := !iter(5)
    val sign_src1 = io.src1(31)
    val sign_src2 = io.src2(31)
    val res_sign  = Mux(io.op === DIV, sign_src1 ^ sign_src2, sign_src1)
    val rmd_reg   = RegInit(0.U(65.W))
    val rmd_m_reg = RegInit(0.U(33.W))
    val div_reg   = RegInit(0.U(33.W))
    val adder     = BLevel_PAdder33(rmd_reg(63, 31), div_reg, 0.U)
    val src1_neg  = BLevel_PAdder32(~io.src1, 0.U, 1.U).io.res
    val src2_neg  = BLevel_PAdder32(~io.src2, 0.U, 1.U).io.res 
    val src1_abs  = Mux((io.op === DIV || io.op === REM) && sign_src1, src1_neg, io.src1)
    val src2_abs  = Mux((io.op === DIV || io.op === REM) && sign_src2, src2_neg, io.src2)

    def count_leading_zeros(x: UInt): UInt = {
        Log2Rev(Reverse(x))(4, 0)
    }

    val src1_leading_zeros = count_leading_zeros(src1_abs)
    val src2_leading_zeros = count_leading_zeros(src2_abs)
    
    when(io.busy){
        iter := iter - 1.U
    }.elsewhen(en){
        when(src1_leading_zeros > src2_leading_zeros){
            iter := 63.U
        }.otherwise{
            iter := src2_leading_zeros - src1_leading_zeros
        }
    }
    /* stage 2: calculate the quotient */
    val op_s2                 = ShiftRegister(io.op, 1, 0.U, en && iter(5))
    val res_sign_s2           = ShiftRegister(res_sign, 1, false.B, true.B)
    val src1_leading_zeros_s2 = ShiftRegister(src1_leading_zeros, 1, 0.U, en && iter(5))
    val src2_leading_zeros_s2 = ShiftRegister(src2_leading_zeros, 1, 0.U, en && iter(5))

    when(en && iter(5)){
        div_reg     := src2_abs << src2_leading_zeros
    }
    when(io.busy){
        // 3 cases, scan the top 2 bits of the remainder
        val top2 = rmd_reg(63, 62)
        // if the top 2 bits are the same, q = 0, and shift the remainder left
        when(top2 === 0.U || top2 === 3.U){
            adder.io.src2 := 0.U
            rmd_reg := (adder.io.res ## rmd_reg(30, 0) ## 0.U)
            rmd_m_reg := rmd_m_reg << 1
        }.otherwise{
            when(top2(1) === 1.U){
                // if the top 2 bits are not the same and is negative, q = -1, shift and add the divisor
                adder.io.src2 := div_reg
                rmd_reg       := (adder.io.res ## rmd_reg(30, 0) ## 0.U)
                rmd_m_reg     := (rmd_m_reg << 1) | 1.U
            }.otherwise{
                // if the top 2 bits are not the same and is positive, q = 1, shift and sub the divisor
                adder.io.src2 := ~div_reg
                adder.io.cin  := 1.U
                rmd_reg       := (adder.io.res ## rmd_reg(30, 0) ## 1.U) 
                rmd_m_reg     := rmd_m_reg << 1
            }
        }

    }.elsewhen(en){
        adder.io.src1 := rmd_reg
        adder.io.src2 := ~rmd_m_reg
        adder.io.cin  := 1.U
        rmd_reg       := Mux(src1_leading_zeros > src2_leading_zeros, (src1_abs ## 0.U(32.W)) << src2_leading_zeros, (src1_abs ## 0.U(32.W)) << src1_leading_zeros >> 1)
        rmd_m_reg     := 0.U
    }.otherwise{
        adder.io.src1 := rmd_reg
        adder.io.src2 := ~rmd_m_reg
        adder.io.cin  := 1.U
    }

    val quotient_s2       = BLevel_PAdder32(adder.io.res, Mux(rmd_reg(64), 0xFFFFFFFFL.U(32.W), 0.U), 0.U).io.res
    val remainder_s2      = BLevel_PAdder32(rmd_reg(63, 32), Mux(rmd_reg(64), div_reg, 0.U), 0.U).io.res >> src2_leading_zeros_s2
    
    /* stage 3: calculate the result */
    val quotient_s3       = ShiftRegister(quotient_s2, 1, 0.U, true.B)
    val remainder_s3      = ShiftRegister(remainder_s2, 1, 0.U, true.B)
    val res_sign_s3       = ShiftRegister(res_sign_s2, 1, false.B, true.B)
    val op_s3             = ShiftRegister(op_s2, 1, 0.U, true.B)
    val ready_s3          = ShiftRegister(iter(5) && op_s2(2), 1, false.B, true.B)

    val result_adder      = Module(new BLevel_PAdder32)
    
    result_adder.io.src1  := Mux1H(Seq(
        (op_s3 === DIV, Mux(res_sign_s3, ~quotient_s3, quotient_s3)),
        (op_s3 === DIVU, quotient_s3),
        (op_s3 === REM, Mux(res_sign_s3, ~remainder_s3, remainder_s3)),
        (op_s3 === REMU, remainder_s3)
    ))
    result_adder.io.src2 := 0.U
    result_adder.io.cin  := (op_s3 === DIV || op_s3 === REM) && res_sign_s3
    io.res               := result_adder.io.res
    io.ready             := ready_s3
}