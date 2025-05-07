import chisel3._
import chisel3.util._
import ZirconConfig.Commit._
import ZirconConfig.Decode._
import ZirconConfig.RegisterFile._
import ZirconConfig.Issue._
import ZirconUtil._

class CommitDBGIO extends Bundle {
    val rob = new ROBDebugIO
    val bdb = new BDBDebugIO
    val robDeq = new ROBCommitIO
    val bdbDeq = new BDBCommitIO
}

class CommitDispatchIO extends Bundle {
    val rob   = new ROBDispatchIO
    val bdb   = new BDBDispatchIO
    val flush = Output(Bool())
}

class CommitIO extends Bundle {
    val fte = Flipped(new FrontendCommitIO)
    val bke = Flipped(new BackendCommitIO)
    val dsp = new CommitDispatchIO
    val dbg = new CommitDBGIO
}

class Commit extends Module {
    val io = IO(new CommitIO)

    val rob = Module(new ReorderBuffer)
    val bdb = Module(new BranchDataBuffer)

    /* bdb enqueue port map */
    val portMapIn = VecInit.fill(ndcd)(0.U(ndcd.W))
    val portMapInTrans = Transpose(portMapIn)
    var enqInPtr = 1.U(ndcd.W)
    for(i <- 0 until ndcd){
        portMapIn(i) := Mux(io.dsp.bdb.enq(i).valid, enqInPtr, 0.U)
        enqInPtr = Mux(io.dsp.bdb.enq(i).valid, ShiftAdd1(enqInPtr), enqInPtr)
    }
    bdb.io.dsp.enq.zipWithIndex.foreach{ case (enq, i) =>
        enq.valid := portMapInTrans(i).orR && io.dsp.rob.enq(0).ready
        enq.bits  := Mux1H(portMapInTrans(i), io.dsp.bdb.enq.map(_.bits))
        io.dsp.bdb.enq(i).ready := bdb.io.dsp.enq(0).ready
    }
    io.dsp.bdb.enqIdx.zipWithIndex.foreach{ case (enqIdx, i) =>
        enqIdx := Mux1H(portMapIn(i), bdb.io.dsp.enqIdx)
    }
    /* rob enqueue */
    rob.io.dsp.enq.zipWithIndex.foreach{ case (enq, i) => 
        enq.valid := io.dsp.rob.enq(i).valid && bdb.io.dsp.enq(0).ready
        enq.bits  := io.dsp.rob.enq(i).bits
        io.dsp.rob.enq(i).ready := rob.io.dsp.enq(0).ready
    }
    io.dsp.rob.enqIdx := rob.io.dsp.enqIdx

    // backend
    bdb.io.bke <> io.bke.bdb
    rob.io.bke <> io.bke.rob

    // output
    val cmtEn = rob.io.cmt.deq(0).valid
    val lastROBIdx1H = Log2OH(rob.io.cmt.deq.map(_.valid))
    val lastROBItem  = Mux1H(lastROBIdx1H, rob.io.cmt.deq.map(_.bits))
    val lastBDBItem  = Mux(cmtEn && lastROBItem.isBranch, bdb.io.cmt.deq.bits, 0.U.asTypeOf(new BDBEntry))
    bdb.io.cmt.deq.ready := cmtEn && lastROBItem.isBranch
    rob.io.cmt.deq.foreach{ case(deq) => deq.ready := DontCare }
    val flush        = lastBDBItem.predFail
    rob.io.cmt.flush := flush || ShiftRegister(flush, 1, false.B, true.B)
    bdb.io.cmt.flush := flush || ShiftRegister(flush, 1, false.B, true.B)

    // store buffer
    io.bke.sb.stCmt := ShiftRegister(lastROBItem.isStore, 1, false.B, true.B)
    io.bke.sb.flush := ShiftRegister(flush, 1, false.B, true.B)

    // rename
    io.fte.rnm.fList.enq.zipWithIndex.foreach{ case (enq, i) =>
        enq.valid := ShiftRegister(rob.io.cmt.deq(i).valid && rob.io.cmt.deq(i).bits.rdVld, 1, false.B, true.B)
        enq.bits  := ShiftRegister(rob.io.cmt.deq(i).bits.pprd, 1, 0.U(wpreg.W), true.B)
    }
    io.fte.rnm.fList.flush := ShiftRegister(flush, 1, false.B, true.B)
    io.fte.rnm.srat.rdVld.zipWithIndex.foreach{ case (rdVld, i) =>
        rdVld := ShiftRegister(rob.io.cmt.deq(i).valid && rob.io.cmt.deq(i).bits.rdVld, 1, false.B, true.B)
    }
    io.fte.rnm.srat.rd    := ShiftRegister(VecInit(rob.io.cmt.deq.map(_.bits.rd)), 1, VecInit.fill(ncommit)(0.U(wlreg.W)), true.B)
    io.fte.rnm.srat.prd   := ShiftRegister(VecInit(rob.io.cmt.deq.map(_.bits.prd)), 1, VecInit.fill(ncommit)(0.U(wpreg.W)), true.B)
    io.fte.rnm.srat.flush := ShiftRegister(flush, 1, false.B, true.B)

    // fetch queue flush
    io.fte.fq.flush := ShiftRegister(flush, 1, false.B, true.B)

    // npc
    io.fte.npc.flush    := ShiftRegister(flush, 1, false.B, true.B)
    io.fte.npc.jumpEn   := ShiftRegister(lastBDBItem.jumpEn, 1, false.B, true.B)
    io.fte.npc.jumpTgt  := ShiftRegister(Mux(lastBDBItem.jumpEn, lastBDBItem.offset, lastROBItem.pc), 1, 0.U(32.W), true.B)

    // predict
    io.fte.pr.gs.pc       := ShiftRegister(lastROBItem.pc, 1, 0.U(32.W), true.B)
    io.fte.pr.gs.jumpEn   := ShiftRegister(lastBDBItem.jumpEn, 1, false.B, true.B)
    io.fte.pr.gs.predType := ShiftRegister(lastBDBItem.predType, 1, 0.U(2.W), true.B)
    io.fte.pr.gs.flush    := ShiftRegister(flush, 1, false.B, true.B)

    io.fte.pr.btbM.pc       := ShiftRegister(lastROBItem.pc, 1, 0.U(32.W), true.B)
    io.fte.pr.btbM.jumpTgt  := ShiftRegister(lastBDBItem.offset, 1, 0.U(32.W), true.B)
    io.fte.pr.btbM.predType := ShiftRegister(lastBDBItem.predType, 1, 0.U(2.W), true.B)
    io.fte.pr.btbM.jumpEn   := ShiftRegister(lastBDBItem.jumpEn, 1, false.B, true.B)

    io.fte.pr.ras.flush     := ShiftRegister(flush, 1, false.B, true.B)
    io.fte.pr.ras.pc        := ShiftRegister(lastROBItem.pc, 1, 0.U(32.W), true.B)
    io.fte.pr.ras.predType  := ShiftRegister(lastBDBItem.predType, 1, 0.U(2.W), true.B)
    
    // dispatch
    io.dsp.flush := ShiftRegister(flush, 1, false.B, true.B)

    // backend
    io.bke.flush := ShiftRegister(VecInit.fill(nis)(flush), 1, VecInit.fill(nis)(false.B), true.B)

    // debug
    io.dbg.rob := rob.io.dbg
    io.dbg.bdb := bdb.io.dbg
    io.dbg.robDeq.deq.zipWithIndex.foreach{ case (deq, i) =>
        deq.valid := rob.io.cmt.deq(i).valid
        deq.bits  := rob.io.cmt.deq(i).bits
    }
    io.dbg.bdbDeq.deq.valid := bdb.io.cmt.deq.valid
    io.dbg.bdbDeq.deq.bits  := bdb.io.cmt.deq.bits
}
