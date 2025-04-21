import chisel3._
import chisel3.util._
import ZirconConfig.Fetch._
import ZirconConfig.Predict._

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
    val btbRIdx    = Output(Vec(nfch, UInt(BTB.totalWidth.W)))
}
class NPCICacheIO extends Bundle {
    val miss       = Input(Bool())
}
class NPCPredictIO extends Bundle {
    val flush      = Input(Bool())
    val pc         = Input(UInt(32.W))
    val jumpOffset = Input(UInt(32.W))
}

class NPCIO extends Bundle {
    val cmt = new NPCCommitIO
    val pd  = new NPCPreDecodeIO
    val fq  = new NPCFetchQueueIO
    val pr  = new NPCPredictIO
    val pc  = new NPCFetchIO
    val ic  = new NPCICacheIO
}
class NPC extends Module {
    val io             = IO(new NPCIO)
    val pc             = WireDefault(io.pc.pc)
    val offset         = WireDefault((nfch * 4).U)
    val adderResult    = BLevelPAdder32(pc, offset, 0.U).io.res
    io.pc.npc  := adderResult
    when(io.cmt.flush){
        pc          := io.cmt.jumpTgt
        offset      := Mux(io.cmt.jumpEn, 0.U, 4.U)
    }.elsewhen(io.fq.ready){
        when(io.ic.miss){
            offset  := 0.U
        }.elsewhen(io.pd.flush){
            pc      := io.pd.pc
            offset  := io.pd.jumpOffset
        }.elsewhen(io.pr.flush){
            pc      := io.pr.pc
            offset  := io.pr.jumpOffset
        }.otherwise {
            io.pc.npc := adderResult(31, 2+log2Ceil(nfch)) ## 0.U((2+log2Ceil(nfch)).W)
        }
    }.otherwise{
        offset := 0.U
    }
    io.pc.btbRIdx := VecInit.tabulate(nfch){i =>
        (io.pc.npc >> 2).take(BTB.totalWidth) + i.U
    }
}