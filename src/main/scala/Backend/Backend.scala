import chisel3._
import chisel3.util._
import ZirconConfig.Issue._
import ZirconConfig.Decode._
import ZirconConfig.Commit._

// class BackendFrontendIO extends Bundle {
//     val rf = new RegfilePredictIO
// }

class BackendDispatchIO extends Bundle {
    val instPkg = Vec(niq, Vec(ndcd, Flipped(Decoupled(new BackendPackage))))
    val wakeBus = Output(Vec(nis, new WakeupBusPkg))
    val rplyBus = Output(new ReplayBusPkg)
}

class BackendROBIO extends Bundle {
    val ridx  = Output(Vec(arithNissue, new ClusterEntry(nrobQ, ndcd)))
    val rdata = Input(Vec(arithNissue, new ROBEntry))
    val widx  = Output(Vec(nis, new ClusterEntry(nrobQ, ndcd)))
    val wen   = Output(Vec(nis, Bool()))
    val wdata = Output(Vec(nis, new ROBEntry))
}
class BackendBDBIO extends Bundle {
    val ridx  = Output(Vec(arithNissue, new ClusterEntry(nbdbQ, ndcd)))
    val rdata = Input(Vec(arithNissue, new BDBEntry))
    val widx  = Output(Vec(arithNissue, new ClusterEntry(nbdbQ, ndcd)))
    val wen   = Output(Vec(arithNissue, Bool()))
    val wdata = Output(Vec(arithNissue, new BDBEntry))
}
class BackendCommitIO extends Bundle {
    val rob = new BackendROBIO
    val bdb = new BackendBDBIO
    val sb    = new DCommitIO
    val flush = Input(Vec(nis, Bool()))
}

class BackendMemoryIO extends Bundle {
    val l2 = Flipped(new L2DCacheIO)
}
class BackendDBGIO extends Bundle {
    val rf   = new RegfileDBGIO
    val arIQ = new IssueQueueDBGIO
    val mdIQ = new IssueQueueDBGIO
    val lsIQ = new IssueQueueDBGIO
    val lsPP = new LSDBGIO
    val mdPP = new MulDivDBGIO
}

class BackendIO extends Bundle {
    // val fte = new BackendFrontendIO
    val dsp = new BackendDispatchIO
    val cmt = new BackendCommitIO
    val mem = new BackendMemoryIO
    val dbg = new BackendDBGIO
}

class Backend extends Module {
    val io   = IO(new BackendIO)
    
    val arIQ = Module(new IssueQueue(ndcd, arithNissue, arithNiq, false))
    val mdIQ = Module(new IssueQueue(ndcd, muldivNissue, muldivNiq, false))
    val lsIQ = Module(new IssueQueue(ndcd, lsuNissue, lsuNiq, true))

    val rf   = Module(new Regfile)

    val fwd  = Module(new Forward)

    val arPP = VecInit.fill(3)(Module(new ArithPipeline).io)
    val mdPP = Module(new MulDivPipeline)
    val lsPP = Module(new LSPipeline)

    val wakeBus = Wire(Vec(niq, Vec(nis, new WakeupBusPkg)))
    val rplyBus = Wire(new ReplayBusPkg)

    /* pipeline 0-2: arith and branch */
    // issue queue
    arIQ.io.enq.zip(io.dsp.instPkg(0)).foreach{case (enq, inst) => enq <> inst}
    arIQ.io.deq.zip(arPP).foreach{case (deq, pp) => deq <> pp.iq.instPkg}
    arIQ.io.wakeBus := wakeBus(0)
    arIQ.io.rplyBus := rplyBus
    arIQ.io.flush   := io.cmt.flush(0)

    // pipeline
    arPP.zipWithIndex.foreach{case(a, i) => 
        a.rf                <> rf.io(i)
        a.wk.rplyIn         := rplyBus
        fwd.io.instPkgWB(i) := a.fwd.instPkgWB
        fwd.io.instPkgEX(i) := a.fwd.instPkgEX
        a.fwd.src1Fwd       <> fwd.io.src1Fwd(i)
        a.fwd.src2Fwd       <> fwd.io.src2Fwd(i)
        a.cmt.flush         := io.cmt.flush(i)
        io.cmt.rob.ridx(i)  := a.cmt.rob.ridx
        a.cmt.rob.rdata     := io.cmt.rob.rdata(i)
        io.cmt.bdb.ridx(i)  := a.cmt.bdb.ridx
        a.cmt.bdb.rdata     := io.cmt.bdb.rdata(i)
        io.cmt.rob.widx(i)  := a.cmt.rob.widx
        io.cmt.rob.wen(i)   := a.cmt.rob.wen
        io.cmt.rob.wdata(i) := a.cmt.rob.wdata
        io.cmt.bdb.widx(i)  := a.cmt.bdb.widx
        io.cmt.bdb.wen(i)   := a.cmt.bdb.wen
        io.cmt.bdb.wdata(i) := a.cmt.bdb.wdata
    }
    wakeBus(0) := VecInit(
        arPP(0).wk.wakeIssue,
        arPP(1).wk.wakeIssue,
        arPP(2).wk.wakeIssue,
        mdPP.io.wk.wakeEX2,
        lsPP.io.wk.wakeRF
    )
    /* pipeline 3: muldiv */
    // issue queue
    mdIQ.io.enq.zip(io.dsp.instPkg(1)).foreach{case (enq, inst) => enq <> inst}
    mdIQ.io.deq.foreach{case deq => deq <> mdPP.io.iq.instPkg}
    mdIQ.io.wakeBus := wakeBus(1)
    mdIQ.io.rplyBus := rplyBus
    mdIQ.io.flush   := io.cmt.flush(3)

    // pipeline
    mdPP.io.rf          <> rf.io(3)
    mdPP.io.wk.rplyIn   := rplyBus
    fwd.io.instPkgWB(3) := mdPP.io.fwd.instPkgWB
    fwd.io.instPkgEX(3) := mdPP.io.fwd.instPkgEX
    fwd.io.src1Fwd(3)   <> mdPP.io.fwd.src1Fwd
    fwd.io.src2Fwd(3)   <> mdPP.io.fwd.src2Fwd
    mdPP.io.cmt.flush   := io.cmt.flush(3)
    io.cmt.rob.widx(3)  := mdPP.io.cmt.rob.widx
    io.cmt.rob.wen(3)   := mdPP.io.cmt.rob.wen
    io.cmt.rob.wdata(3) := mdPP.io.cmt.rob.wdata

    wakeBus(1) := VecInit(
        arPP(0).wk.wakeRF,
        arPP(1).wk.wakeRF,
        arPP(2).wk.wakeRF,
        mdPP.io.wk.wakeEX2,
        lsPP.io.wk.wakeRF
    )

    /* pipeline 4: lsu */
    lsIQ.io.enq.zip(io.dsp.instPkg(2)).foreach{case (enq, inst) => enq <> inst}
    lsIQ.io.deq.foreach{case deq => deq <> lsPP.io.iq.instPkg}
    lsIQ.io.wakeBus := wakeBus(2)
    lsIQ.io.rplyBus := rplyBus
    lsIQ.io.flush   := io.cmt.flush(4)

    // pipeline
    lsPP.io.rf          <> rf.io(4)
    lsPP.io.wk.rplyIn   := rplyBus
    fwd.io.instPkgWB(4) := lsPP.io.fwd.instPkgWB
    lsPP.io.cmt.flush   := io.cmt.flush(4)
    lsPP.io.cmt.dc      := io.cmt.sb
    io.cmt.rob.widx(4)  := lsPP.io.cmt.rob.widx
    io.cmt.rob.wen(4)   := lsPP.io.cmt.rob.wen
    io.cmt.rob.wdata(4) := lsPP.io.cmt.rob.wdata

    wakeBus(2) := VecInit(
        arPP(0).wk.wakeRF,
        arPP(1).wk.wakeRF,
        arPP(2).wk.wakeRF,
        mdPP.io.wk.wakeEX2,
        lsPP.io.wk.wakeD1
    )
    rplyBus := lsPP.io.wk.rplyOut
    io.mem.l2 <> lsPP.io.mem.l2

    io.dsp.wakeBus := wakeBus(0)
    io.dsp.rplyBus := rplyBus

    // io.fte.rf   <> rf.predictIO

    io.dbg.rf   := rf.dbg
    io.dbg.arIQ := arIQ.io.dbg
    io.dbg.mdIQ := mdIQ.io.dbg
    io.dbg.lsIQ := lsIQ.io.dbg
    io.dbg.mdPP := mdPP.io.dbg
    io.dbg.lsPP := lsPP.io.dbg
}   