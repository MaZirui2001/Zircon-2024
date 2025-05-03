import chisel3._
import chisel3.util._
import ZirconConfig.RegisterFile._
import ZirconConfig.Decode._
import ZirconConfig.Issue._
import ZirconConfig.Commit._
class ReadyBoardEntry extends Bundle {
    val ready = Bool()
    val lpv = UInt(3.W)
    def apply(ready: Bool, lpv: UInt): ReadyBoardEntry = {
        val entry = Wire(new ReadyBoardEntry)
        entry.ready := ready
        entry.lpv := lpv << 1
        entry
    }
}

class ReadyBoardIO extends Bundle {
    val pinfo   = Input(Vec(ndcd, new PRegisterInfo))
    val wakeBus = Input(Vec(nis, new WakeupBusPkg))
    val rplyBus = Input(new ReplayBusPkg)
    val prjInfo = Output(Vec(ndcd, new ReadyBoardEntry))
    val prkInfo = Output(Vec(ndcd, new ReadyBoardEntry))
    val flush   = Input(Bool())
}

class ReadyBoard extends Module {
    val io = IO(new ReadyBoardIO)

    val board = RegInit(VecInit.fill(npreg)((new ReadyBoardEntry)(true.B, 0.U)))

    for (i <- 0 until ndcd) {
        board(io.pinfo(i).prd).ready := false.B
    }
    for (i <- 0 until nis) {
        board(io.wakeBus(i).prd) := (new ReadyBoardEntry)(true.B, io.wakeBus(i).lpv)
    }
    board.zipWithIndex.foreach{case(e, i) => 
        when(e.lpv.orR){
            e.lpv   := e.lpv << !io.rplyBus.replay
            e.ready := Mux(io.rplyBus.replay, false.B, e.ready || io.rplyBus.prd === i.U)
        }
    }
    board(0) := (new ReadyBoardEntry)(true.B, 0.U)

    when(io.flush){
        board := VecInit.fill(npreg)((new ReadyBoardEntry)(true.B, 0.U))
    }

    io.prjInfo.zip(io.pinfo).foreach{case(e, p) => e := board(p.prj)}
    io.prkInfo.zip(io.pinfo).foreach{case(e, p) => e := board(p.prk)}

}
