import chisel3._
import chisel3.util._
import ZirconConfig.RegisterFile._
import ZirconConfig.Decode._
import ZirconConfig.Commit._
import ZirconConfig.Issue._
import ZirconUtil._
import ZirconConfig.JumpOp._
import ZirconConfig.EXEOp._

class ROBFrontendEntry extends Bundle{
    val rdVld      = Bool()
    val inst       = UInt(32.W)
    val rd         = UInt(wlreg.W)
    val prd        = UInt(wpreg.W)
    val pprd       = UInt(wpreg.W)
    val pc         = UInt(32.W)
    val predType   = UInt(2.W)
    val isStore    = Bool()

    def apply(pkg: FrontendPackage): ROBFrontendEntry = {
        val entry = Wire(new ROBFrontendEntry)
        entry.rdVld      := pkg.rinfo.rdVld
        entry.inst       := pkg.inst
        entry.rd         := pkg.rinfo.rd
        entry.prd        := pkg.pinfo.prd
        entry.pprd       := pkg.pinfo.pprd
        entry.pc         := pkg.pc
        entry.isStore    := pkg.op(6) && pkg.func(2)
        entry.predType   := Mux1H(Seq(
            (pkg.op(4) && (pkg.op(2) || !pkg.op(1) || pkg.rinfo.rd === 0.U)) -> BR,
            (pkg.op(4) && !pkg.op(2) && pkg.op(1) && pkg.rinfo.rd =/= 0.U) -> CALL,
            (pkg.op(4, 0) === JALR && pkg.rinfo.rj === 1.U) -> RET,
        ))
        entry
    }
}

class ROBBackendEntry extends Bundle{
    val complete   = Bool()
    val jumpEn     = Bool()
    val predFail   = Bool()
    val exception  = UInt(8.W)
    val result     = UInt(32.W)
    val nxtCmtEn   = Bool()

    def apply(pkg: BackendPackage): ROBBackendEntry = {
        val entry = Wire(new ROBBackendEntry)
        entry.complete   := pkg.valid
        entry.jumpEn     := pkg.jumpEn
        entry.predFail   := pkg.predFail
        entry.exception  := pkg.exception
        entry.result     := pkg.result
        entry.nxtCmtEn   := pkg.nxtCmtEn
        entry
    }
}

class ROBEntry extends Bundle{
    val fte = new ROBFrontendEntry
    val bke = new ROBBackendEntry
    
    def apply(fte: ROBFrontendEntry, bke: ROBBackendEntry): ROBEntry = {
        val entry = Wire(new ROBEntry)
        entry.fte := fte
        entry.bke := bke
        entry
    }
    def flushGen(): Bool = {
        this.bke.predFail || this.bke.exception(7)
    }
}

class ROBDebugIO extends Bundle {
    val fullCycle = Output(UInt(64.W))
}

class ROBDispatchIO extends Bundle{
    val flush  = Output(Bool())
    val enq    = Vec(ndcd, Flipped(Decoupled(new ROBFrontendEntry)))
    val enqIdx = Output(Vec(ndcd, new ClusterEntry(wrobQ, wdecode)))
    
}

class ROBCommitIO extends Bundle{
    val deq = Vec(ncommit, Decoupled(new ROBEntry))
}
class ReorderBufferIO extends Bundle{
    val fte = Flipped(new FrontendCommitIO)
    val bke = Flipped(new BackendCommitIO)
    val cmt = new ROBCommitIO
    val dsp = new ROBDispatchIO
    val dbg = new ROBDebugIO
}


class ReorderBuffer extends Module{
    val io = IO(new ReorderBufferIO)

    val q = Module(new ClusterIndexFIFO(new ROBEntry, nrob, ndcd, ncommit, 0, nis))

    // 1. frontend: in dispatch stage, each instruction will enqueue into the ROB
    q.io.enq.zip(io.dsp.enq).foreach{case (enq, fte) =>
        enq.bits.fte := fte.bits
        enq.bits.bke := DontCare
        enq.valid := fte.valid
        fte.ready := enq.ready
    }
    io.dsp.enqIdx.zip(q.io.enqIdx).foreach{ case(idx, enq) =>
        idx.qidx    := OHToUInt(enq.qidx)
        idx.offset  := OHToUInt(enq.offset)
        idx.high    := enq.high
    }
    // 2. backend: in writeback stage, some instruction will write some data into the ROB
    q.io.wdata.zip(io.bke.wdata).foreach{case (wdata, bke) =>
        wdata.fte := DontCare
        wdata.bke := bke
    }
    q.io.widx := io.bke.widx
    q.io.wen := io.bke.wen
    // 3. commit: in commit stage, some instruction will be committed
    for(i <- 0 until ncommit){
        io.cmt.deq(i).bits := {
            val entry = Wire(new ROBEntry)
            entry.fte := q.io.deq(i).bits.fte
            entry.bke := q.io.deq(i).bits.bke
            entry
        }
        io.cmt.deq(i).valid := (
            if(i == 0) q.io.deq(i).bits.bke.complete && q.io.deq(i).valid
            else q.io.deq.take(i+1).map(_.bits.bke.complete).reduce(_ && _) 
              && q.io.deq(i).valid
              && q.io.deq(i-1).bits.bke.nxtCmtEn
        )
        q.io.deq(i).ready := io.cmt.deq(i).valid
    }

    // output
    val lastCmtIndex1H = Log2OH(io.cmt.deq.map(_.valid))
    val lastCmtItem    = Mux1H(lastCmtIndex1H, q.io.deq.map(_.bits))
    val flush          = lastCmtItem.flushGen()
    // self flush
    q.io.flush       := flush || ShiftRegister(flush, 1, false.B, true.B)
    // store buffer
    io.bke.sb.stCmt  := ShiftRegister(lastCmtItem.fte.isStore, 1, false.B, true.B)
    io.bke.sb.flush  := ShiftRegister(flush, 1, false.B, true.B)
    // rename
    io.fte.rnm.fList.enq.zipWithIndex.foreach{ case (enq, i) =>
        enq.valid   := ShiftRegister(io.cmt.deq(i).valid && q.io.deq(i).bits.fte.rdVld, 1, false.B, true.B)
        enq.bits    := ShiftRegister(q.io.deq(i).bits.fte.pprd, 1, 0.U(wpreg.W), true.B)
    }
    io.fte.rnm.fList.flush   := ShiftRegister(flush, 1, false.B, true.B)
    io.fte.rnm.srat.rdVld.zipWithIndex.foreach{ case (rdVld, i) =>
        rdVld      := ShiftRegister(io.cmt.deq(i).valid && q.io.deq(i).bits.fte.rdVld, 1, false.B, true.B)
    }
    io.fte.rnm.srat.rd      := ShiftRegister(VecInit(io.cmt.deq.map(_.bits.fte.rd)), 1, VecInit.fill(ncommit)(0.U(wlreg.W)), true.B)
    io.fte.rnm.srat.prd     := ShiftRegister(VecInit(io.cmt.deq.map(_.bits.fte.prd)), 1, VecInit.fill(ncommit)(0.U(wpreg.W)), true.B)
    io.fte.rnm.srat.flush   := ShiftRegister(flush, 1, false.B, true.B)

    // fetch queue flush
    io.fte.fq.flush         := ShiftRegister(flush, 1, false.B, true.B) 

    // npc
    io.fte.npc.flush       := ShiftRegister(flush, 1, false.B, true.B)
    io.fte.npc.jumpEn      := ShiftRegister(lastCmtItem.bke.jumpEn, 1, false.B, true.B)
    io.fte.npc.jumpTgt     := ShiftRegister(Mux(lastCmtItem.bke.jumpEn, lastCmtItem.bke.result, lastCmtItem.fte.pc), 1, 0.U(32.W), true.B)

    // dispatch
    io.dsp.flush := ShiftRegister(flush, 1, false.B, true.B)

    // backend
    io.bke.flush := ShiftRegister(VecInit.fill(nis)(flush), 1, VecInit.fill(nis)(false.B), true.B)

    // debug
    val fullCycleReg    = RegInit(0.U(64.W))
    fullCycleReg        := fullCycleReg + !io.dsp.enq.map(_.ready).reduce(_ && _)
    io.dbg.fullCycle    := fullCycleReg

}