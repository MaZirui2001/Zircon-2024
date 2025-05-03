import chisel3._
import chisel3.util._
import ZirconConfig.Commit._
import ZirconConfig.Decode._
import ZirconConfig.Issue._
import ZirconConfig.JumpOp._
import ZirconConfig.EXEOp._

class BDBEntry extends Bundle {
    val predType = UInt(2.W)
    val offset   = UInt(32.W)
    val jumpEn   = Bool()
    val predFail = Bool()

    def apply(pkg: FrontendPackage): BDBEntry = {
        val entry = Wire(new BDBEntry)
        entry.predType := Mux1H(Seq(
            (pkg.op(4) && (pkg.op(2) || !pkg.op(1) || pkg.rinfo.rd === 0.U)) -> BR,
            (pkg.op(4) && !pkg.op(2) && pkg.op(1) && pkg.rinfo.rd =/= 0.U) -> CALL,
            (pkg.op(4, 0) === JALR && pkg.rinfo.rj === 1.U) -> RET,
        ))
        entry.offset   := pkg.predInfo.offset
        entry.jumpEn   := DontCare
        entry.predFail := DontCare
        entry
    }
    def apply(pkg: BackendPackage): BDBEntry = {
        val entry = Wire(new BDBEntry)
        entry.predType := DontCare
        entry.offset   := pkg.result
        entry.jumpEn   := pkg.jumpEn
        entry.predFail := pkg.predFail
        entry
    }
    def enqueue(data: Data): Unit = {
        val bits = data.asInstanceOf[BDBEntry]
        this.predType := bits.predType
        this.offset   := bits.offset
    }
    def write(data: Data): Unit = {
        val bits = data.asInstanceOf[BDBEntry]
        this.offset   := bits.offset
        this.jumpEn   := bits.jumpEn
        this.predFail := bits.predFail
    }
}

class BDBDispatchIO extends Bundle {
    val enq = Vec(ndcd, Flipped(Decoupled(new BDBEntry)))
    val enqIdx = Output(Vec(ndcd, new ClusterEntry(wbdbQ, wdecode)))
}

class BDBCommitIO extends Bundle {
    val deq = Decoupled(new BDBEntry)
    val flush = Input(Bool())
}

class BDBDebugIO extends Bundle {
    val fullCycle = Output(UInt(64.W))
    val branch    = Output(UInt(64.W))
    val branchFail = Output(UInt(64.W))
    val call      = Output(UInt(64.W))
    val callFail  = Output(UInt(64.W))
    val ret       = Output(UInt(64.W))
    val retFail   = Output(UInt(64.W))
}

class BranchDataBufferIO extends Bundle {
    val bke = Flipped(new BackendBDBIO)
    val cmt = new BDBCommitIO
    val dsp = new BDBDispatchIO
    val dbg = new BDBDebugIO
}

class BranchDataBuffer extends Module {
    val io = IO(new BranchDataBufferIO)

    val q = Module(new ClusterIndexFIFO(new BDBEntry, nbdb, ndcd, 1, arithNissue, arithNissue))

    // 1. dispatch stage: dispatch instructions to the BDB
    q.io.enq.zip(io.dsp.enq).foreach{ case (enq, dsp) => enq <> dsp }
    io.dsp.enqIdx.zip(q.io.enqIdx).foreach{ case (idx, enq) =>
        idx.qidx    := OHToUInt(enq.qidx)
        idx.offset  := OHToUInt(enq.offset)
        idx.high    := DontCare
    }

    // 2. backend: write data into the BDB
    q.io.wdata := io.bke.wdata
    q.io.wen   := io.bke.wen
    q.io.widx  := io.bke.widx

    q.io.ridx := io.bke.ridx
    io.bke.rdata := q.io.rdata

    // 3. commit stage: commit instructions from the BDB
    io.cmt.deq <> q.io.deq(0)

    q.io.flush := io.cmt.flush

    // debug
    val fullCycleReg = RegInit(0.U(64.W))
    fullCycleReg := fullCycleReg + !io.dsp.enq(0).ready
    io.dbg.fullCycle := fullCycleReg
    val branchReg = RegInit(0.U(64.W))
    val branchFailReg = RegInit(0.U(64.W))
    val callReg = RegInit(0.U(64.W))
    val callFailReg = RegInit(0.U(64.W))
    val retReg = RegInit(0.U(64.W))
    val retFailReg = RegInit(0.U(64.W))
    when(io.cmt.deq.valid && io.cmt.deq.ready && io.cmt.deq.bits.predType === BR){
        branchReg := branchReg + 1.U
        when(io.cmt.deq.bits.predFail){
            branchFailReg := branchFailReg + 1.U
        }
    }
    when(io.cmt.deq.valid && io.cmt.deq.ready && io.cmt.deq.bits.predType === CALL){
        callReg := callReg + 1.U
        when(io.cmt.deq.bits.predFail){
            callFailReg := callFailReg + 1.U
        }
    }
    when(io.cmt.deq.valid && io.cmt.deq.ready && io.cmt.deq.bits.predType === RET){ 
        retReg := retReg + 1.U
        when(io.cmt.deq.bits.predFail){
            retFailReg := retFailReg + 1.U
        }
    }
    io.dbg.branch := branchReg
    io.dbg.branchFail := branchFailReg
    io.dbg.call := callReg
    io.dbg.callFail := callFailReg
    io.dbg.ret := retReg
    io.dbg.retFail := retFailReg

}