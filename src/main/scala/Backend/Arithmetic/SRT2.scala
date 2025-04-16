import chisel3._
import chisel3.util._
import ZirconUtil._
import ZirconConfig.EXEOp._

class SRT2IO extends Bundle {
    val src1  = Input(UInt(32.W))
    val src2  = Input(UInt(32.W))
    val op    = Input(UInt(4.W))
    val res   = Output(UInt(32.W))
    val busy  = Output(Bool())
    val ready = Output(Bool())
    val dbg   = Output(new SRT2DBG)
}

class SRT2DBG extends Bundle {
    val busyCycle = UInt(64.W)
}

class SRT2 extends Module {
    val io = IO(new SRT2IO)

    /* Stage 1: Initialization */
    // calculate the number of iterations
    val en = io.op(2)
    val iter = RegInit(63.U(6.W))
    
    io.busy      := !iter(5)
    val signSrc1 = io.src1(31)
    val signSrc2 = io.src2(31)
    val resSign  = Mux(io.op === DIV, signSrc1 ^ signSrc2, signSrc1)
    val rmdReg   = RegInit(0.U(65.W))
    val rmdMReg  = RegInit(0.U(33.W))
    val divReg   = RegInit(0.U(33.W))
    val adder    = BLevelPAdder33(rmdReg(63, 31), divReg, 0.U)
    val src1Neg  = BLevelPAdder32(~io.src1, 0.U, 1.U).io.res
    val src2Neg  = BLevelPAdder32(~io.src2, 0.U, 1.U).io.res 
    val src1Abs  = Mux((io.op === DIV || io.op === REM) && signSrc1, src1Neg, io.src1)
    val src2Abs  = Mux((io.op === DIV || io.op === REM) && signSrc2, src2Neg, io.src2)

    def countLeadingZeros(x: UInt): UInt = {
        Log2Rev(Reverse(x))(4, 0)
    }

    val src1LeadingZeros = countLeadingZeros(src1Abs)
    val src2LeadingZeros = countLeadingZeros(src2Abs)
    
    when(io.busy){
        iter := iter - 1.U
    }.elsewhen(en){
        when(src1LeadingZeros > src2LeadingZeros){
            iter := 63.U
        }.otherwise{
            iter := src2LeadingZeros - src1LeadingZeros
        }
    }
    /* stage 2: calculate the quotient */
    val opS2               = ShiftRegister(io.op, 1, 0.U, en && iter(5))
    val resSignS2          = ShiftRegister(resSign, 1, false.B, en && iter(5))
    val src1S2             = ShiftRegister(io.src1, 1, 0.U, en && iter(5))
    val src1LeadingZerosS2 = ShiftRegister(src1LeadingZeros, 1, 0.U, en && iter(5))
    val src2LeadingZerosS2 = ShiftRegister(src2LeadingZeros, 1, 0.U, en && iter(5))

    when(en && iter(5)){
        divReg     := src2Abs << src2LeadingZeros
    }
    when(io.busy){
        // 3 cases, scan the top 2 bits of the remainder
        val top2 = rmdReg(63, 62)
        // if the top 2 bits are the same, q = 0, and shift the remainder left
        when(top2 === 0.U || top2 === 3.U){
            adder.io.src2 := 0.U
            rmdReg  := (adder.io.res ## rmdReg(30, 0) ## 0.U)
            rmdMReg := rmdMReg << 1
        }.otherwise{
            when(top2(1) === 1.U){
                // if the top 2 bits are not the same and is negative, q = -1, shift and add the divisor
                adder.io.src2 := divReg
                rmdReg        := (adder.io.res ## rmdReg(30, 0) ## 0.U)
                rmdMReg       := (rmdMReg << 1) | 1.U
            }.otherwise{
                // if the top 2 bits are not the same and is positive, q = 1, shift and sub the divisor
                adder.io.src2 := ~divReg
                adder.io.cin  := 1.U
                rmdReg        := (adder.io.res ## rmdReg(30, 0) ## 1.U) 
                rmdMReg       := rmdMReg << 1
            }
        }

    }.elsewhen(en){
        adder.io.src1 := rmdReg
        adder.io.src2 := ~rmdMReg
        adder.io.cin  := 1.U
        rmdReg        := Mux(src1LeadingZeros > src2LeadingZeros, (src1Abs ## 0.U(32.W)) << src2LeadingZeros, (src1Abs ## 0.U(32.W)) << src1LeadingZeros >> 1)
        rmdMReg       := 0.U
    }.otherwise{
        adder.io.src1 := rmdReg
        adder.io.src2 := ~rmdMReg
        adder.io.cin  := 1.U
    }

    val quotientS2  = BLevelPAdder32(adder.io.res, Mux(rmdReg(64), 0xFFFFFFFFL.U(32.W), 0.U), 0.U).io.res
    val remainderS2 = BLevelPAdder32(rmdReg(63, 32), Mux(rmdReg(64), divReg, 0.U), 0.U).io.res >> src2LeadingZerosS2
    
    /* stage 3: calculate the result */
    val quotientS3  = ShiftRegister(quotientS2, 1, 0.U, iter(5))
    val remainderS3 = ShiftRegister(remainderS2, 1, 0.U, iter(5))
    val resSignS3   = ShiftRegister(resSignS2, 1, false.B, iter(5))
    val src1S3      = ShiftRegister(src1S2, 1, 0.U, iter(5))
    val divS3IsZero = ShiftRegister(divReg === 0.U, 1, false.B, iter(5))
    val opS3        = ShiftRegister(opS2, 1, 0.U, iter(5))
    val readyS3     = ShiftRegister(iter(5) && opS2(2), 1, false.B, true.B)

    val resultAdder = Module(new BLevelPAdder32)
    // for div, if the divisor is 0, the result is 0xffffffff according to the RISC-V spec
    // for rem, if the divisor is 0, the result is the divident
    resultAdder.io.src1  := Mux1H(Seq(
        (opS3 === DIV,  Mux(divS3IsZero, 0xffffffffL.U, Mux(resSignS3, ~quotientS3, quotientS3))),
        (opS3 === DIVU, Mux(divS3IsZero, 0xffffffffL.U, quotientS3)),
        (opS3 === REM,  Mux(divS3IsZero, src1S3, Mux(resSignS3, ~remainderS3, remainderS3))),
        (opS3 === REMU, Mux(divS3IsZero, src1S3, remainderS3))
    ))
    resultAdder.io.src2 := 0.U
    resultAdder.io.cin  := (opS3 === DIV || opS3 === REM) && resSignS3 && !divS3IsZero
    io.res              := resultAdder.io.res
    io.ready            := readyS3

    val busyCycleReg = RegInit(0.U(64.W))
    busyCycleReg     := busyCycleReg + io.busy
    io.dbg.busyCycle := busyCycleReg
}