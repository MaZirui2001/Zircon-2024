import chisel3._
import chisel3.util._
import Zircon_Config.RegisterFile._
import Zircon_Config.Decode._
import Zircon_Config.Issue._
import Zircon_Config.Commit._
class Ready_Board_Entry extends Bundle {
    val ready = Bool()
    val lpv = UInt(3.W)
    def apply(ready: Bool, lpv: UInt): Ready_Board_Entry = {
        val entry = Wire(new Ready_Board_Entry)
        entry.ready := ready
        entry.lpv := lpv
        entry
    }
}

class Ready_Board_IO extends Bundle {
    val pinfo       = Input(Vec(ndecode, new PRegister_Info))
    val wake_bus    = Input(Vec(nissue, new Wakeup_Bus_Pkg))
    val rply_bus    = Input(new Replay_Bus_Pkg)
    val prj_info    = Output(Vec(ndecode, new Ready_Board_Entry))
    val prk_info    = Output(Vec(ndecode, new Ready_Board_Entry))
    val flush       = Input(Bool())
}

class Ready_Board extends Module {
    val io = IO(new Ready_Board_IO)

    val board = RegInit(VecInit.fill(npreg)((new Ready_Board_Entry)(true.B, 0.U)))

    for (i <- 0 until ndecode) {
        board(io.pinfo(i).prd).ready := false.B
    }
    for (i <- 0 until nissue) {
        board(io.wake_bus(i).prd) := (new Ready_Board_Entry)(true.B, io.wake_bus(i).lpv)
    }
    board(io.rply_bus.prd).ready := true.B
    board.foreach{case(e) => 
        when(e.lpv.orR){
            e.lpv := e.lpv << 1
            e.ready := !io.rply_bus.replay
        }
    }
    board(0) := (new Ready_Board_Entry)(true.B, 0.U)

    when(io.flush){
        board := VecInit.fill(npreg)((new Ready_Board_Entry)(true.B, 0.U))
    }

    io.prj_info.zip(io.pinfo).foreach{case(e, p) => e := board(p.prj)}
    io.prk_info.zip(io.pinfo).foreach{case(e, p) => e := board(p.prk)}

    
}
