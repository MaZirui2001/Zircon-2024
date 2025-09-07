import chisel3._
import chisel3.util._
import ZirconConfig.Fetch._
import ZirconConfig.Predict.RAS._
import ZirconConfig.JumpOp._
import ZirconUtil._

class RASGShareIO extends Bundle {
    val jumpEnPredict = Input(Vec(nfch, Bool()))
}
class RASBTBMiniIO extends Bundle {
    // val rData        = Input(Vec(nfch, new BTBMiniEntry))
    val predType     = Input(Vec(nfch, UInt(2.W)))
    val returnOffset = Output(UInt(32.W))
}
class RASFCIO extends Bundle {
    val pc       = Input(Vec(nfch, UInt(32.W)))
}
class RASPreDecodeIO extends Bundle {
    val predType = Input(UInt(2.W))
    val pc       = Input(UInt(32.W))
    val returnOffset = Output(UInt(32.W))
    val flush    = Input(Bool())
}
class RASCommitIO extends Bundle {
    val predType = Input(UInt(2.W))
    val pc       = Input(UInt(32.W))
    val flush    = Input(Bool())
}
class RASPPIO extends Bundle {
    val fcStall = Input(Bool())
    val pdStall = Input(Bool())
}

class RASIO extends Bundle {
    val gs      = new RASGShareIO
    val btbM    = new RASBTBMiniIO
    val fc      = new RASFCIO
    val pd      = new RASPreDecodeIO
    val cmt     = new RASCommitIO
    val fte     = new RASPPIO
}

class RAS extends Module {
    val io = IO(new RASIO)

    val ras = RegInit(VecInit.fill(size)(0.U(30.W)))
    val rasPD = RegInit(VecInit.fill(size)(0.U(30.W)))
    val rasCmt = RegInit(VecInit.fill(size)(0.U(30.W)))

    // fetch stage
    val ptr = RegInit(1.U(size.W))
    val predType = PriorityMux(
        io.gs.jumpEnPredict.zip(io.btbM.predType).map{ case (j, p) => (j, p) }
    )
    val pc = PriorityMux(
        io.gs.jumpEnPredict.zip(io.fc.pc).map{ case (j, p) => (j, p) }
    )
    when(!io.fte.fcStall){
        when(predType === CALL){
            ras.zipWithIndex.foreach{case (r, i) => 
                r := Mux(ptr(i), pc(31, 2), r)
            }
            ptr := ShiftAdd1(ptr)
        }.elsewhen(predType === RET){
            ptr := ShiftSub1(ptr)
        }
    }
    io.btbM.returnOffset := ZE(Mux1H(ShiftSub1(ptr), ras)) << 2

    // pre-decode stage
    val ptrPD = RegInit(1.U(size.W))
    val ptrPDNext = WireDefault(ptrPD)
    val rasPDNext = WireDefault(rasPD)
    when(!io.fte.pdStall){  
        when(io.pd.predType === CALL){
            ptrPDNext := ShiftAdd1(ptrPD)
            rasPDNext.zipWithIndex.foreach{case (r, i) => 
                r := Mux(ptrPD(i), io.pd.pc(31, 2), rasPD(i))
            }
        }.elsewhen(io.pd.predType === RET){
            ptrPDNext := ShiftSub1(ptrPD)
        }
    }
    ptrPD := ptrPDNext
    rasPD := rasPDNext
    io.pd.returnOffset := ShiftRegister(ZE(Mux1H(ShiftSub1(ptrPD), rasPD)) << 2, 1, 0.U, true.B)
    when(io.pd.flush){
        ptr := ptrPDNext
        ras := rasPDNext
    }

    // commit stage
    val ptrCmt = RegInit(1.U(size.W))
    val ptrCmtNext = WireDefault(ptrCmt)
    val rasCmtNext = WireDefault(rasCmt)
    when(io.cmt.predType === CALL){
        ptrCmtNext := ShiftAdd1(ptrCmt)
        rasCmtNext.zipWithIndex.foreach{case (r, i) => 
            r := Mux(ptrCmt(i), BLevelPAdder32(io.cmt.pc, 4.U, 0.U).io.res(31, 2), rasCmt(i))
        }
    }.elsewhen(io.cmt.predType === RET){
        ptrCmtNext := ShiftSub1(ptrCmt)
    }
    ptrCmt := ptrCmtNext
    rasCmt := rasCmtNext
    when(io.cmt.flush){
        ptr := ptrCmtNext
        ptrPD := ptrCmtNext
        ras := rasCmtNext
        rasPD := rasCmtNext
    }
}
