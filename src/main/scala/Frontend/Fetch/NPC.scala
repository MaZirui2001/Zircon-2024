import chisel3._
import chisel3.util._
import Zircon_Config.Fetch._

class NPC_Commit_IO extends Bundle {
    val flush       = Input(Bool())
    val jump_en     = Input(Bool())
    val jump_tgt    = Input(UInt(32.W))
}
class NPC_PreDecode_IO extends Bundle {
    val flush       = Input(Bool())
    val pc          = Input(UInt(32.W))
    val jump_offset = Input(UInt(32.W))
}
class NPC_Fetch_Queue_IO extends Bundle {
    val ready       = Input(Bool())
}
class NPC_Fetch_IO extends Bundle {
    val pc          = Input(UInt(32.W))
    val npc         = Output(UInt(32.W))
}
class NPC_ICache_IO extends Bundle {
    val miss        = Input(Bool())
}
class NPC_IO extends Bundle {
    val cmt = new NPC_Commit_IO
    val pd  = new NPC_PreDecode_IO
    val fq  = new NPC_Fetch_Queue_IO
    val pc  = new NPC_Fetch_IO
    val ic  = new NPC_ICache_IO
}
class NPC extends Module {
    val io = IO(new NPC_IO)
    val pc = WireDefault(io.pc.pc)
    val offset = WireDefault((nfch * 4).U)
    val npc = BLevel_PAdder32(pc, offset, 0.U).io.res
    io.pc.npc := npc
    when(io.cmt.flush){
        pc          := io.cmt.jump_tgt
        offset      := Mux(io.cmt.jump_en, 0.U, 4.U)
    }.elsewhen(io.fq.ready){
        when(io.ic.miss){
            offset := 0.U
        }.elsewhen(io.pd.flush){
            pc          := io.pd.pc
            offset      := io.pd.jump_offset
        }
    }.otherwise{
        offset := 0.U
    }
}