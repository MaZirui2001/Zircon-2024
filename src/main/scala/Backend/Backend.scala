import chisel3._
import chisel3.util._
import Zircon_Config.Issue._
import Zircon_Config.Decode._
import Zircon_Config.Commit._

class Backend_Dispatch_IO extends Bundle {
    val inst_pkg = Vec(niq, Vec(ndecode, Flipped(Decoupled(new Backend_Package))))
    val wake_bus = Output(Vec(nissue, new Wakeup_Bus_Pkg))
    val rply_bus = Output(new Replay_Bus_Pkg)
}

class Backend_Commit_IO extends Bundle {
    val widx    = Output(Vec(nissue, new Cluster_Entry(nrob_q, ndecode)))
    val wen     = Output(Vec(nissue, Bool()))
    val wdata   = Output(Vec(nissue, new ROB_Backend_Entry))
    val sb      = new D_Commit_IO
    val flush   = Input(Vec(nissue, Bool()))
}

class Backend_Memory_IO extends Bundle {
    val l2 = Flipped(new L2_DCache_IO)
}
class Backend_Debug_IO extends Bundle {
    val rf = new Regfile_DBG_IO
}

class Backend_IO extends Bundle {
    val dsp = new Backend_Dispatch_IO
    val cmt = new Backend_Commit_IO
    val mem = new Backend_Memory_IO
    val dbg = new Backend_Debug_IO
}

class Backend extends Module {
    val io = IO(new Backend_IO)
    
    val ar_iq = Module(new Issue_Queue(ndecode, arith_nissue, arith_niq, false))
    val md_iq = Module(new Issue_Queue(ndecode, muldiv_nissue, muldiv_niq, false))
    val ls_iq = Module(new Issue_Queue(ndecode, lsu_nissue, lsu_niq, true))

    val rf    = Module(new Regfile)

    val fwd   = Module(new Forward)

    val ar_pp = VecInit.fill(3)(Module(new Arith_Pipeline).io)
    val md_pp = Module(new MulDiv_Pipeline)
    val ls_pp = Module(new LS_Pipeline)

    val wake_bus = Wire(Vec(niq, Vec(nissue, new Wakeup_Bus_Pkg)))
    val rply_bus = Wire(new Replay_Bus_Pkg)

    /* pipeline 0-2: arith and branch */
    // issue queue
    ar_iq.io.enq.zip(io.dsp.inst_pkg(0)).foreach{case (enq, inst) => enq <> inst}
    ar_iq.io.deq.zip(ar_pp).foreach{case (deq, pp) => deq <> pp.iq.inst_pkg}
    ar_iq.io.wake_bus := wake_bus(0)
    ar_iq.io.rply_bus := rply_bus
    ar_iq.io.flush    := io.cmt.flush(0)

    // pipeline
    ar_pp.zipWithIndex.foreach{case(a, i) => 
        a.rf            <> rf.io(i)
        a.wk.rply_in    := rply_bus
        fwd.io.inst_pkg_wb(i) := a.fwd.inst_pkg_wb
        fwd.io.inst_pkg_ex(i) := a.fwd.inst_pkg_ex
        a.fwd.src1_fwd  <> fwd.io.src1_fwd(i)
        a.fwd.src2_fwd  <> fwd.io.src2_fwd(i)
        a.cmt.flush     := io.cmt.flush(i)
        io.cmt.widx(i)  := a.cmt.widx
        io.cmt.wen(i)   := a.cmt.wen
        io.cmt.wdata(i) := a.cmt.wdata

    }
    wake_bus(0) := VecInit(
        ar_pp(0).wk.wake_issue,
        ar_pp(1).wk.wake_issue,
        ar_pp(2).wk.wake_issue,
        md_pp.io.wk.wake_ex3,
        ls_pp.io.wk.wake_rf
    )
    /* pipeline 3: muldiv */
    // issue queue
    md_iq.io.enq.zip(io.dsp.inst_pkg(1)).foreach{case (enq, inst) => enq <> inst}
    md_iq.io.deq.foreach{case deq => deq <> md_pp.io.iq.inst_pkg}
    md_iq.io.wake_bus := wake_bus(1)
    md_iq.io.rply_bus := rply_bus
    md_iq.io.flush    := io.cmt.flush(3)

    // pipeline
    md_pp.io.rf             <> rf.io(3)
    md_pp.io.wk.rply_in     := rply_bus
    fwd.io.inst_pkg_wb(3)   := md_pp.io.fwd.inst_pkg_wb
    md_pp.io.cmt.flush      := io.cmt.flush(3)
    io.cmt.widx(3)          := md_pp.io.cmt.widx
    io.cmt.wen(3)           := md_pp.io.cmt.wen
    io.cmt.wdata(3)         := md_pp.io.cmt.wdata

    wake_bus(1) := VecInit(
        ar_pp(0).wk.wake_rf,
        ar_pp(1).wk.wake_rf,
        ar_pp(2).wk.wake_rf,
        md_pp.io.wk.wake_ex3,
        ls_pp.io.wk.wake_rf
    )

    /* pipeline 4: lsu */
    ls_iq.io.enq.zip(io.dsp.inst_pkg(2)).foreach{case (enq, inst) => enq <> inst}
    ls_iq.io.deq.foreach{case deq => deq <> ls_pp.io.iq.inst_pkg}
    ls_iq.io.wake_bus := wake_bus(2)
    ls_iq.io.rply_bus := rply_bus
    ls_iq.io.flush    := io.cmt.flush(4)

    // pipeline
    ls_pp.io.rf             <> rf.io(4)
    ls_pp.io.wk.rply_in     := rply_bus
    fwd.io.inst_pkg_wb(4)   := ls_pp.io.fwd.inst_pkg_wb
    ls_pp.io.cmt.flush      := io.cmt.flush(4)
    ls_pp.io.cmt.dc         := io.cmt.sb
    io.cmt.widx(4)          := ls_pp.io.cmt.widx
    io.cmt.wen(4)           := ls_pp.io.cmt.wen
    io.cmt.wdata(4)         := ls_pp.io.cmt.wdata

    wake_bus(2) := VecInit(
        ar_pp(0).wk.wake_rf,
        ar_pp(1).wk.wake_rf,
        ar_pp(2).wk.wake_rf,
        md_pp.io.wk.wake_ex3,
        ls_pp.io.wk.wake_rf
    )
    rply_bus := ls_pp.io.wk.rply_out
    io.mem.l2 <> ls_pp.io.mem.l2

    io.dsp.wake_bus := wake_bus(0)
    io.dsp.rply_bus := rply_bus

    io.dbg.rf.rf := rf.dbg.rf
}   