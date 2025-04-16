import chisel3._
import chisel3.util._
import ZirconConfig.Fetch._

class NPCCommitIO extends Bundle {
    val flush      = Input(Bool())
    val jumpEn     = Input(Bool())
    val jumpTgt    = Input(UInt(32.W))
}
class NPCPreDecodeIO extends Bundle {
    val flush      = Input(Bool())
    val pc         = Input(UInt(32.W))
    val jumpOffset = Input(UInt(32.W))
}
class NPCFetchQueueIO extends Bundle {
    val ready      = Input(Bool())
}
class NPCFetchIO extends Bundle {
    val pc         = Input(UInt(32.W))
    val npc        = Output(UInt(32.W))
}
class NPCICacheIO extends Bundle {
    val miss       = Input(Bool())
}
class NPCIO extends Bundle {
    val cmt = new NPCCommitIO
    val pd  = new NPCPreDecodeIO
    val fq  = new NPCFetchQueueIO
    val pc  = new NPCFetchIO
    val ic  = new NPCICacheIO
}
class NPC extends Module {
    val io     = IO(new NPCIO)
    val pc     = WireDefault(io.pc.pc)
    val offset = WireDefault((nfch * 4).U)
    val npc    = BLevelPAdder32(pc, offset, 0.U).io.res
    io.pc.npc  := npc
    when(io.cmt.flush){
        pc          := io.cmt.jumpTgt
        offset      := Mux(io.cmt.jumpEn, 0.U, 4.U)
    }.elsewhen(io.fq.ready){
        when(io.ic.miss){
            offset  := 0.U
        }.elsewhen(io.pd.flush){
            pc      := io.pd.pc
            offset  := io.pd.jumpOffset
        }
    }.otherwise{
        offset := 0.U
    }
}