import chisel3._
import chisel3.util._
import ZirconConfig.RegisterFile._
import ZirconConfig.Decode._
import ZirconConfig.Commit._
import ZirconConfig.Issue._
import ZirconUtil._
import ZirconConfig.JumpOp._
import ZirconConfig.EXEOp._

class ROBEntry extends Bundle{
    val rdVld    = Bool()
    val rd       = UInt(wlreg.W)
    val prd      = UInt(wpreg.W)
    val pprd     = UInt(wpreg.W)
    val pc       = UInt(32.W)
    val isBranch = Bool()
    val isStore  = Bool()

    val complete = Bool()
    val nxtCmtEn = Bool()
    
    def apply(pkg: FrontendPackage): ROBEntry = {
        val entry = WireDefault(0.U.asTypeOf(new ROBEntry))
        entry.rdVld    := pkg.rinfo.rdVld
        entry.rd       := pkg.rinfo.rd
        entry.prd      := pkg.pinfo.prd
        entry.pprd     := pkg.pinfo.pprd
        entry.pc       := pkg.pc
        entry.isBranch := pkg.op(4)
        entry.isStore  := pkg.op(6) && pkg.func(2)
        entry
    }
    def apply(pkg: BackendPackage): ROBEntry = {
        val entry = WireDefault(0.U.asTypeOf(new ROBEntry))
        entry.complete := pkg.valid
        entry.nxtCmtEn := pkg.nxtCmtEn
        entry
    }
    def enqueue(data: Data): Unit = {
        val bits = data.asInstanceOf[ROBEntry]
        this.rdVld    := bits.rdVld
        this.rd       := bits.rd
        this.prd      := bits.prd
        this.pprd     := bits.pprd
        this.pc       := bits.pc
        this.isBranch := bits.isBranch
        this.isStore  := bits.isStore
        this.complete := false.B
    }
    def write(data: Data): Unit = {
        val bits = data.asInstanceOf[ROBEntry]
        this.complete := bits.complete
        this.nxtCmtEn := bits.nxtCmtEn
    }
}

class ROBDebugIO extends Bundle {
    val fullCycle = Output(UInt(64.W))
}

class ROBDispatchIO extends Bundle{
    val enq    = Vec(ndcd, Flipped(Decoupled(new ROBEntry)))
    val enqIdx = Output(Vec(ndcd, new ClusterEntry(wrobQ, wdecode)))
}

class ROBCommitIO extends Bundle{
    val deq = Vec(ncommit, Decoupled(new ROBEntry))
    val flush = Input(Bool())
}
class ReorderBufferIO extends Bundle{
    val bke = Flipped(new BackendROBIO)
    val cmt = new ROBCommitIO
    val dsp = new ROBDispatchIO
    val dbg = new ROBDebugIO
}


class ReorderBuffer extends Module{
    val io = IO(new ReorderBufferIO)

    val q = Module(new ClusterIndexFIFO(new ROBEntry, nrob, ndcd, ncommit, arithNissue, nis))
    // 1. frontend: in dispatch stage, each instruction will enqueue into the ROB
    q.io.enq.zip(io.dsp.enq).foreach{case (enq, dsp) => enq <> dsp }
    io.dsp.enqIdx.zip(q.io.enqIdx).foreach{ case(idx, enq) =>
        idx.qidx    := OHToUInt(enq.qidx)
        idx.offset  := OHToUInt(enq.offset)
        idx.high    := enq.high
    }
    // 2. backend: in writeback stage, some instruction will write some data into the ROB
    q.io.wdata   := io.bke.wdata
    q.io.ridx    := io.bke.ridx
    io.bke.rdata := q.io.rdata
    q.io.widx    := io.bke.widx
    q.io.wen     := io.bke.wen
    // 3. commit: in commit stage, some instruction will be committed
    for(i <- 0 until ncommit){
        io.cmt.deq(i).bits := q.io.deq(i).bits
        io.cmt.deq(i).valid := (
            if(i == 0) q.io.deq(i).bits.complete && q.io.deq(i).valid
            else q.io.deq.take(i+1).map(_.bits.complete).reduce(_ && _) 
              && q.io.deq(i).valid
              && q.io.deq(i-1).bits.nxtCmtEn
        )
        q.io.deq(i).ready := io.cmt.deq(i).valid
    }
    q.io.flush := io.cmt.flush
    // debug
    val fullCycleReg = RegInit(0.U(64.W))
    fullCycleReg     := fullCycleReg + !io.dsp.enq(0).ready
    io.dbg.fullCycle := fullCycleReg

}