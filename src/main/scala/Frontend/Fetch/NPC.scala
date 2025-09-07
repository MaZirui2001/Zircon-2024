import chisel3._
import chisel3.util._
import ZirconConfig.Fetch._
import ZirconConfig.Predict._
import ZirconConfig.JumpOp._

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
    val validMask  = Output(Vec(nfch, Bool()))
}
class NPCICacheIO extends Bundle {
    val miss       = Input(Bool())
}
class NPCPredictIO extends Bundle {
    val flush      = Input(Bool())
    val pc         = Input(UInt(32.W))
    val jumpOffset = Input(UInt(32.W))
    val predType   = Input(UInt(2.W))
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
    when(io.cmt.flush){
        io.pc.npc := BLevelPAdder32(io.cmt.jumpTgt, Mux(io.cmt.jumpEn, 0.U, 4.U), 0.U).io.res
    }.elsewhen(io.fq.ready){
        when(io.ic.miss){
            io.pc.npc := pc
        }.elsewhen(io.pd.flush){
            io.pc.npc := BLevelPAdder32(io.pd.pc, io.pd.jumpOffset, 0.U).io.res
        }
        .elsewhen(io.pr.flush){
            io.pc.npc := io.pr.jumpOffset
        }
        .otherwise{
            io.pc.npc := BLevelPAdder32(pc, offset, 0.U).io.res(31, 2+log2Ceil(nfch)) ## 0.U((2+log2Ceil(nfch)).W)
        }
    }.otherwise{
        io.pc.npc := pc
    }
    val validMask = MuxLookup((io.pc.npc >> 2).take(log2Ceil(nfch)), 0.U(4.W))(
        (0 until nfch).map(i => ((nfch-1-i).U, ((2<<i)-1).U))
    ).asBools
    io.pc.validMask := validMask
}