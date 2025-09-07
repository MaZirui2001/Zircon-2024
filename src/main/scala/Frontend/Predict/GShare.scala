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
    // jumpCandidate: One-Hot code, the most probably branch that will jump
    val jumpCandidate = Input(Vec(nfch, Bool()))
    val predType      = Input(Vec(nfch, UInt(2.W)))
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
class GShareFrontendIO extends Bundle {
    val fcStall = Input(Bool())
    val pdStall = Input(Bool())
}

class GShareIO extends Bundle {
    val fc   = new GShareFCIO
    val btbM = new GShareBTBMiniIO
    val pd   = new GSharePreDecodeIO
    val cmt  = new GShareCommitIO
    val ras  = Flipped(new RASGShareIO)
    val fte  = new GShareFrontendIO
}

class GShare extends Module {
    val io = IO(new GShareIO)

    val pcFC = io.fc.pc >> 2 // ignore the last 2 bits
    def hash(ghr: UInt, pc: UInt): UInt = {
        ghr ^ pc.take(1)
    }
    val ghr = RegInit(0.U(ghrWidth.W))
    val pht = RegInit(VecInit.fill(phtSize)(2.U(2.W)))

    val phtRData = pht((hash(ghr, (pcFC >> log2Ceil(nfch)).take(ghrWidth))))

    val isBr = VecInit(io.btbM.predType.map{_ =/= 0.U})
    val isCallOrReturn = VecInit(io.btbM.predType.map{_(1)})

    io.fc.jumpEnPredict := io.btbM.jumpCandidate.zip(isCallOrReturn).map{ case (j, c) =>
        (j && phtRData(1)) || c
    }

    io.btbM.jumpEnPredict := io.fc.jumpEnPredict
    io.ras.jumpEnPredict := io.fc.jumpEnPredict
    val jumpMask = PriorityMux(
        io.fc.jumpEnPredict.zip((0 until nfch).map(i => ((2 << i) - 1).U & isBr(i)) ).map{ case (j, m) => (j, m) }
    ) & isBr.asUInt
    val shiftNum      = Mux(io.fc.jumpEnPredict.asUInt.orR, PopCount(jumpMask), PopCount(isBr))
    val shiftFillBits = Mux(io.fc.jumpEnPredict.asUInt.orR, 1.U << PopCount(jumpMask), 0.U(nfch.W))

    when(!io.fte.fcStall){
        ghr := (shiftFillBits ## ghr) >> shiftNum   
    }

    // PreDecode
    val shiftNumPD      = PopCount(io.pd.isBr)
    val shiftFillBitsPD = io.pd.jumpEn.asUInt
    val ghrPD           = RegInit(0.U(ghrWidth.W))
    val ghrPDNext       = (shiftFillBitsPD ## ghrPD) >> shiftNumPD

    when(!io.fte.pdStall){
        ghrPD               := ghrPDNext
    }

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