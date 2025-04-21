import chisel3._
import chisel3.util._
import ZirconConfig.Predict._
import ZirconUtil._
import ZirconConfig.Fetch._

class PredictFCIO extends Bundle {
    val gs        = new GShareFCIO
    val btbM      = new BTBMiniFCIO
    val pc        = Input(Vec(nfch, UInt(32.W)))
    val validMask = Output(UInt(nfch.W))
}

class PredictPreDecodeIO extends Bundle {
    val gs  = new GSharePreDecodeIO
}

class PredictCommitIO extends Bundle {
    val gs   = new GShareCommitIO
    val btbM = new BTBMiniCommitIO
}

class PredictIO extends Bundle {
    val fc   = new PredictFCIO
    val npc  = Flipped(new NPCPredictIO)
    val pd   = new PredictPreDecodeIO
    val cmt  = new PredictCommitIO
}

class Predict extends Module {
    val io   = IO(new PredictIO)
    
    val gs   = Module(new GShare)
    val btbM = Module(new BTBMini)

    gs.io.btbM <> btbM.io.gs
    gs.io.fc   <> io.fc.gs
    gs.io.pd   <> io.pd.gs
    gs.io.cmt  <> io.cmt.gs

    btbM.io.fc  <> io.fc.btbM
    btbM.io.cmt <> io.cmt.btbM

    io.npc.flush      := gs.io.fc.jumpEnPredict.reduce(_ || _)
    io.npc.pc         := Mux1H(gs.io.fc.jumpEnPredict, io.fc.pc)
    io.fc.validMask   := Mux(gs.io.fc.jumpEnPredict.reduce(_ || _), 
        Mux1H(gs.io.fc.jumpEnPredict, (0 until nfch).map(i => ((2 << i) - 1).U)),
        Fill(nfch, true.B)
    )
    io.npc.jumpOffset := SE(Mux1H(gs.io.fc.jumpEnPredict, btbM.io.fc.rData.map(_.imm << 2)))
}