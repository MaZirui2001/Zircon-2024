import chisel3._
import chisel3.util._
import ZirconConfig.Issue._
import ZirconConfig.Decode._
import ZirconConfig.Commit._

class BackendDispatchIO extends Bundle {
    val instPkg = Vec(niq, Vec(ndcd, Flipped(Decoupled(new BackendPackage))))
    val wakeBus = Output(Vec(nis, new WakeupBusPkg))
    val rplyBus = Output(new ReplayBusPkg)
}

class BackendCommitIO extends Bundle {
    val widx    = Output(Vec(nis, new ClusterEntry(nrobQ, ndcd)))
    val wen     = Output(Vec(nis, Bool()))
    val wdata   = Output(Vec(nis, new ROBBackendEntry))
    val sb      = new DCommitIO
    val flush   = Input(Vec(nis, Bool()))
}

class BackendMemoryIO extends Bundle {
    val l2 = Flipped(new L2DCacheIO)
}
class BackendDebugIO extends Bundle {
    val rf = new RegfileDBGIO
}

class BackendIO extends Bundle {
    val dsp = new BackendDispatchIO
    val cmt = new BackendCommitIO
    val mem = new BackendMemoryIO
    val dbg = new BackendDebugIO
}

class Backend extends Module {
    val io = IO(new BackendIO)
    
    val arIq = Module(new IssueQueue(ndcd, arithNissue, arithNiq, false))
    val mdIq = Module(new IssueQueue(ndcd, muldivNissue, muldivNiq, false))
    val lsIq = Module(new IssueQueue(ndcd, lsuNissue, lsuNiq, true))

    val rf    = Module(new Regfile)

    val fwd   = Module(new Forward)

    val arPp = VecInit.fill(3)(Module(new ArithPipeline).io)
    val mdPp = Module(new MulDivPipeline)
    val lsPp = Module(new LSPipeline)

    val wakeBus = Wire(Vec(niq, Vec(nis, new WakeupBusPkg)))
    val rplyBus = Wire(new ReplayBusPkg)

    /* pipeline 0-2: arith and branch */
    // issue queue
    arIq.io.enq.zip(io.dsp.instPkg(0)).foreach{case (enq, inst) => enq <> inst}
    arIq.io.deq.zip(arPp).foreach{case (deq, pp) => deq <> pp.iq.instPkg}
    arIq.io.wakeBus := wakeBus(0)
    arIq.io.rplyBus := rplyBus
    arIq.io.flush    := io.cmt.flush(0)

    // pipeline
    arPp.zipWithIndex.foreach{case(a, i) => 
        a.rf            <> rf.io(i)
        a.wk.rplyIn    := rplyBus
        fwd.io.instPkgWb(i) := a.fwd.instPkgWb
        fwd.io.instPkgEx(i) := a.fwd.instPkgEx
        a.fwd.src1Fwd  <> fwd.io.src1Fwd(i)
        a.fwd.src2Fwd  <> fwd.io.src2Fwd(i)
        a.cmt.flush     := io.cmt.flush(i)
        io.cmt.widx(i)  := a.cmt.widx
        io.cmt.wen(i)   := a.cmt.wen
        io.cmt.wdata(i) := a.cmt.wdata

    }
    wakeBus(0) := VecInit(
        arPp(0).wk.wakeIssue,
        arPp(1).wk.wakeIssue,
        arPp(2).wk.wakeIssue,
        mdPp.io.wk.wakeEx2,
        lsPp.io.wk.wakeRf
    )
    /* pipeline 3: muldiv */
    // issue queue
    mdIq.io.enq.zip(io.dsp.instPkg(1)).foreach{case (enq, inst) => enq <> inst}
    mdIq.io.deq.foreach{case deq => deq <> mdPp.io.iq.instPkg}
    mdIq.io.wakeBus := wakeBus(1)
    mdIq.io.rplyBus := rplyBus
    mdIq.io.flush    := io.cmt.flush(3)

    // pipeline
    mdPp.io.rf             <> rf.io(3)
    mdPp.io.wk.rplyIn     := rplyBus
    fwd.io.instPkgWb(3)   := mdPp.io.fwd.instPkgWb
    fwd.io.instPkgEx(3)   := mdPp.io.fwd.instPkgEx
    fwd.io.src1Fwd(3)      <> mdPp.io.fwd.src1Fwd
    fwd.io.src2Fwd(3)      <> mdPp.io.fwd.src2Fwd
    mdPp.io.cmt.flush      := io.cmt.flush(3)
    io.cmt.widx(3)          := mdPp.io.cmt.widx
    io.cmt.wen(3)           := mdPp.io.cmt.wen
    io.cmt.wdata(3)         := mdPp.io.cmt.wdata

    wakeBus(1) := VecInit(
        arPp(0).wk.wakeRf,
        arPp(1).wk.wakeRf,
        arPp(2).wk.wakeRf,
        mdPp.io.wk.wakeEx2,
        lsPp.io.wk.wakeRf
    )

    /* pipeline 4: lsu */
    lsIq.io.enq.zip(io.dsp.instPkg(2)).foreach{case (enq, inst) => enq <> inst}
    lsIq.io.deq.foreach{case deq => deq <> lsPp.io.iq.instPkg}
    lsIq.io.wakeBus := wakeBus(2)
    lsIq.io.rplyBus := rplyBus
    lsIq.io.flush    := io.cmt.flush(4)

    // pipeline
    lsPp.io.rf             <> rf.io(4)
    lsPp.io.wk.rplyIn     := rplyBus
    fwd.io.instPkgWb(4)   := lsPp.io.fwd.instPkgWb
    lsPp.io.cmt.flush      := io.cmt.flush(4)
    lsPp.io.cmt.dc         := io.cmt.sb
    io.cmt.widx(4)          := lsPp.io.cmt.widx
    io.cmt.wen(4)           := lsPp.io.cmt.wen
    io.cmt.wdata(4)         := lsPp.io.cmt.wdata

    wakeBus(2) := VecInit(
        arPp(0).wk.wakeRf,
        arPp(1).wk.wakeRf,
        arPp(2).wk.wakeRf,
        mdPp.io.wk.wakeEx2,
        lsPp.io.wk.wakeD1
    )
    rplyBus := lsPp.io.wk.rplyOut
    io.mem.l2 <> lsPp.io.mem.l2

    io.dsp.wakeBus := wakeBus(0)
    io.dsp.rplyBus := rplyBus

    io.dbg.rf.rf := rf.dbg.rf
}   