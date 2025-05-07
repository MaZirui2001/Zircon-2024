import chisel3._
import chisel3.util._
import ZirconConfig.Predict._
import ZirconUtil._
import ZirconConfig.Fetch._

class PredictFCIO extends Bundle {
    val gs        = new GShareFCIO
    val btbM      = new BTBMiniFCIO
    // val ras       = new RASFCIO
    val pc        = Input(Vec(nfch, UInt(32.W)))
    val validMask = Output(UInt(nfch.W))
}

class PredictPreDecodeIO extends Bundle {
    val gs  = new GSharePreDecodeIO
    val ras = new RASPreDecodeIO
}
class PredictFrontendIO extends Bundle {
    val fcStall = Input(Bool())
    val pdStall = Input(Bool())
}

class PredictCommitIO extends Bundle {
    val gs   = new GShareCommitIO
    val btbM = new BTBMiniCommitIO
    val ras  = new RASCommitIO
}

class PredictIO extends Bundle {
    val fc   = new PredictFCIO
    val npc  = Flipped(new NPCPredictIO)
    val pd   = new PredictPreDecodeIO
    val cmt  = new PredictCommitIO
    val fte  = new PredictFrontendIO
}

class Predict extends Module {
    val io   = IO(new PredictIO)
    
    val gs   = Module(new GShare)
    val btbM = Module(new BTBMini)
    val ras  = Module(new RAS)

    gs.io.btbM <> btbM.io.gs
    gs.io.fc   <> io.fc.gs
    gs.io.pd   <> io.pd.gs
    gs.io.cmt  <> io.cmt.gs
    gs.io.ras  <> ras.io.gs
    gs.io.fte.fcStall := io.fte.fcStall
    gs.io.fte.pdStall := io.fte.pdStall

    btbM.io.fc  <> io.fc.btbM
    btbM.io.cmt <> io.cmt.btbM
    btbM.io.ras <> ras.io.btbM

    ras.io.pd  <> io.pd.ras
    ras.io.cmt <> io.cmt.ras
    ras.io.fc.pc := io.fc.pc
    ras.io.fte.fcStall := io.fte.fcStall
    ras.io.fte.pdStall := io.fte.pdStall

    io.npc.flush      := gs.io.fc.jumpEnPredict.reduce(_ || _)
    io.npc.pc         := Mux1H(gs.io.fc.jumpEnPredict, io.fc.pc)
    io.npc.predType   := Mux1H(gs.io.fc.jumpEnPredict, io.fc.btbM.predType)
    io.fc.validMask   := Mux(gs.io.fc.jumpEnPredict.reduce(_ || _), 
        Mux1H(gs.io.fc.jumpEnPredict, (0 until nfch).map(i => ((2 << i) - 1).U)),
        Fill(nfch, true.B)
    )
    io.npc.jumpOffset := Mux1H(gs.io.fc.jumpEnPredict, btbM.io.fc.jumpTgt.map(_ << 2))
}