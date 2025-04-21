import chisel3._
import chisel3.util._
import ZirconConfig.Predict.GShare._
import ZirconConfig.Fetch._
import ZirconUtil._

class GShareFCIO extends Bundle {
    val pc            = Input(UInt(32.W))
    val jumpEnPredict = Output(Vec(nfch, Bool()))
}
class GShareBTBMiniIO extends Bundle {
    // isBr: the locations of the branch instructions
    val isBr          = Input(Vec(nfch, Bool()))
    // jumpCandidate: One-Hot code, the most probably branch that will jump
    val jumpCandidate = Input(Vec(nfch, Bool()))

    val jumpEnPredict = Output(Vec(nfch, Bool()))
}
class GSharePreDecodeIO extends Bundle {
    val isBr     = Input(Vec(nfch, Bool())) // insts not valid is also false
    val jumpEn   = Input(Vec(nfch, Bool()))
    val flush    = Input(Bool())
}
class GShareCommitIO extends Bundle {
    val pc       = Input(UInt(32.W))
    val jumpEn   = Input(Bool())
    val predType = Input(UInt(2.W))
    val flush    = Input(Bool())
}

class GShareIO extends Bundle {
    val fc   = new GShareFCIO
    val btbM = new GShareBTBMiniIO
    val pd   = new GSharePreDecodeIO
    val cmt  = new GShareCommitIO
}

class GShare extends Module {
    val io = IO(new GShareIO)

    val pcFC = io.fc.pc >> 2 // ignore the last 2 bits
    def hash(ghr: UInt, pc: UInt): UInt = {
        ghr ^ pc
    }
    val ghr = RegInit(0.U(ghrWidth.W))
    val pht = RegInit(VecInit.fill(phtSize)(2.U(2.W)))

    val phtRData = pht((hash(ghr, (pcFC >> log2Ceil(nfch)).take(ghrWidth))))

    io.fc.jumpEnPredict := io.btbM.jumpCandidate.zip(io.btbM.isBr).map{ case (j, b) =>
        j && b && phtRData(1)
    }
    io.btbM.jumpEnPredict := io.fc.jumpEnPredict
    val jumpMask      = Mux1H(io.btbM.jumpCandidate, (0 until nfch).map(i => ((2 << i) - 1).U)) & io.btbM.isBr.asUInt
    val shiftNum      = Mux(phtRData(1), PopCount(jumpMask), PopCount(io.btbM.isBr))
    val shiftFillBits = Mux(phtRData(1), 1.U << PopCount(jumpMask), 0.U(nfch.W))

    ghr := (shiftFillBits ## ghr) >> shiftNum

    // PreDecode
    val shiftNumPD      = PopCount(io.pd.isBr)
    val shiftFillBitsPD = io.pd.jumpEn.asUInt
    val ghrPD           = RegInit(0.U(ghrWidth.W))
    val ghrPDNext       = (shiftFillBitsPD ## ghrPD) >> shiftNumPD
    ghrPD               := ghrPDNext

    // Commit
    val ghrCmt      = RegInit(0.U(ghrWidth.W))
    val ghrCmtNext  = WireDefault(ghrCmt)
    when(io.cmt.predType =/= 0.U){
        ghrCmtNext := (io.cmt.jumpEn ## ghrCmt) >> 1
    }
    ghrCmt := ghrCmtNext

    // flush from preDecoder
    when(io.pd.flush){
        ghr := ghrPDNext
    }
    // flush from commit
    when(io.cmt.flush){
        ghr := ghrCmtNext
        ghrPD := ghrCmtNext
    }
    // update pht
    when(io.cmt.predType =/= 0.U){
        val pcCmt    = io.cmt.pc >> (2 + log2Ceil(nfch))
        val phtWIdx  = hash(ghrCmt, pcCmt).take(phtWidth)
        pht(phtWIdx) := Mux(io.cmt.jumpEn, 
            pht(phtWIdx) + (pht(phtWIdx) =/= 3.U),
            pht(phtWIdx) - (pht(phtWIdx) =/= 0.U)
        )
    }
    
}