import chisel3._
import chisel3.util._
import ZirconConfig.Decode._
import ZirconConfig.Issue._
import ZirconUtil._
import ZirconConfig.Commit._

class LSCommitIO extends PipelineCommitIO {
    val dc      = new DCommitIO
}
class LSIQIO extends PipelineIQIO 

// class LSRegfileIO extends PipelineRegfileIO

class LSWakeupIO extends Bundle {
    val wakeRf    = Output(new WakeupBusPkg)
    val wakeD1    = Output(new WakeupBusPkg)
    val rplyIn    = Input(new ReplayBusPkg)
    val rplyOut   = Output(new ReplayBusPkg)
}
class LSForwardIO extends Bundle {
    val instPkgWb  = Output(new BackendPackage)
}

class LSMemoryIO extends Bundle {
    val l2         = Flipped(new L2DCacheIO)
}

class LSPipelineIO extends Bundle {
    val iq         = new LSIQIO
    val rf         = Flipped(new RegfileSingleIO)
    val cmt        = new LSCommitIO
    val fwd        = new LSForwardIO
    val wk         = new LSWakeupIO
    val mem        = new LSMemoryIO
}

class LSPipeline extends Module {
    val io = IO(new LSPipelineIO)
    
    val agu = Module(new BLevelPAdder32)
    val dc  = Module(new DCache)

    /* Issue Stage */
    val instPkgIs = WireDefault(io.iq.instPkg.bits)
    io.iq.instPkg.ready := !(dc.io.pp.miss || dc.io.pp.sbFull)
    
    def segFlush(instPkg: BackendPackage): Bool = {
        io.cmt.flush || io.wk.rplyIn.replay && (instPkg.prjLpv | instPkg.prkLpv).orR
    }
    instPkgIs.prjLpv := io.iq.instPkg.bits.prjLpv << 1
    instPkgIs.prkLpv := io.iq.instPkg.bits.prkLpv << 1
    
    /* Regfile Stage */
    val instPkgRf = WireDefault(ShiftRegister(
        Mux(segFlush(instPkgIs), 0.U.asTypeOf(new BackendPackage), instPkgIs), 
        1, 
        0.U.asTypeOf(new BackendPackage), 
        !(dc.io.pp.miss || dc.io.pp.sbFull) || io.cmt.flush
    ))
    // regfile read
    io.rf.rd.prj      := instPkgRf.prj
    io.rf.rd.prk      := instPkgRf.prk
    // agu
    agu.io.src1       := io.rf.rd.prjData
    agu.io.src2       := instPkgRf.imm
    agu.io.cin        := 0.U
    instPkgRf.src1    := agu.io.res
    instPkgRf.src2    := io.rf.rd.prkData
    // wakeup
    io.wk.wakeRf := (new WakeupBusPkg)(instPkgRf, io.wk.rplyIn, 1)

    dc.io.pp.rreq       := instPkgRf.op(5)
    dc.io.pp.mtype      := instPkgRf.op(2, 0)
    dc.io.pp.isLatest   := instPkgRf.isLatest
    dc.io.pp.wreq       := instPkgRf.op(6)
    dc.io.pp.wdata      := instPkgRf.src2
    dc.io.pp.vaddr      := instPkgRf.src1

    /* DCache Stage 1 */
    val instPkgD1 = WireDefault(ShiftRegister(
        Mux(io.cmt.flush, 0.U.asTypeOf(new BackendPackage), instPkgRf), 
        1, 
        0.U.asTypeOf(new BackendPackage), 
        !(dc.io.pp.miss || dc.io.pp.sbFull) || io.cmt.flush
    ))
    io.wk.wakeD1 := (new WakeupBusPkg)(instPkgD1, io.wk.rplyIn, 2)
    // dcache
    dc.io.cmt           := io.cmt.dc
    dc.io.mmu.paddr     := instPkgD1.src1
    // TODO: add mmu
    dc.io.mmu.uncache   := false.B
    dc.io.mmu.exception := 0.U(8.W)
    dc.io.l2            <> io.mem.l2
    /* DCache Stage 2 */
    val instPkgD2 = WireDefault(ShiftRegister(
        Mux(segFlush(instPkgD1), 0.U.asTypeOf(new BackendPackage), instPkgD1), 
        1, 
        0.U.asTypeOf(new BackendPackage), 
        !(dc.io.pp.miss || dc.io.pp.sbFull) || io.cmt.flush
    ))

    instPkgD2.nxtCmtEn := !instPkgD2.op(6)

    // replay
    io.wk.rplyOut.prd      := instPkgD2.prd
    io.wk.rplyOut.replay   := (dc.io.pp.miss || dc.io.pp.sbFull) && instPkgD2.valid

    /* Write Back Stage */
    val instPkgWb = WireDefault(ShiftRegister(
        Mux(io.cmt.flush || dc.io.pp.miss || dc.io.pp.sbFull, 0.U.asTypeOf(new BackendPackage), instPkgD2), 
        1, 
        0.U.asTypeOf(new BackendPackage), 
        true.B
    ))
    instPkgWb.rfWdata := dc.io.pp.rdata
    // rob
    io.cmt.widx.offset  := UIntToOH(instPkgWb.robIdx.offset)
    io.cmt.widx.qidx    := UIntToOH(instPkgWb.robIdx.qidx)
    io.cmt.widx.high    := DontCare
    io.cmt.wen          := instPkgWb.valid
    io.cmt.wdata        := (new ROBBackendEntry)(instPkgWb) 
    // regfile
    io.rf.wr.prd        := instPkgWb.prd
    io.rf.wr.prdVld    := instPkgWb.rdVld
    io.rf.wr.prdData   := instPkgWb.rfWdata
    // forward
    io.fwd.instPkgWb  := instPkgWb
}
