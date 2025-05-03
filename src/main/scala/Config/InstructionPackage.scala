import chisel3._
import chisel3.util._
import ZirconConfig.Issue._
import ZirconConfig.Decode._
import ZirconConfig.Commit._
import ZirconConfig.RegisterFile._

class PredictInfo extends Bundle {
    val offset     = UInt(32.W)
    val jumpEn     = Bool()
    val vld        = Bool()
}

class FrontendPackage extends Bundle {
    val valid      = Bool()
    val pc         = UInt(32.W)
    val predInfo   = new PredictInfo()
    val inst       = UInt(32.W)
    val op         = UInt(7.W)
    val imm        = UInt(32.W)
    val func       = UInt(niq.W)
    val rinfo      = new RegisterInfo()
    val pinfo      = new PRegisterInfo()
}

class BackendPackage extends Bundle {
    val valid      = Bool()
    val pc         = UInt(32.W)
    val predOffset = UInt(32.W)
    val prj        = UInt(wpreg.W)
    val prk        = UInt(wpreg.W)
    val prd        = UInt(wpreg.W)
    val rdVld      = Bool()
    val op         = UInt(7.W)
    val imm        = UInt(32.W)
    val robIdx     = new ClusterEntry(wrobQ, wdecode)
    val bdbIdx     = new ClusterEntry(wbdbQ, wdecode)
    val prjWk      = Bool()
    val prkWk      = Bool()

    // for inferred wakeup
    val prjLpv     = UInt(3.W)
    val prkLpv     = UInt(3.W)

    val isLatest   = Bool()
    val src1       = UInt(32.W)
    val src2       = UInt(32.W)
    val rfWdata    = UInt(32.W)
    val jumpEn     = Bool()
    val predFail   = Bool()
    val exception  = UInt(8.W)
    val result     = UInt(32.W)
    val nxtCmtEn   = Bool()
    
    def apply(fte: FrontendPackage, robIdx: ClusterEntry, bdbIdx: ClusterEntry, prjInfo: ReadyBoardEntry, prkInfo: ReadyBoardEntry): BackendPackage = {
        val bke = WireDefault(0.U.asTypeOf(new BackendPackage))
        bke.valid      := fte.valid
        bke.pc         := fte.pc
        bke.prj        := fte.pinfo.prj
        bke.prk        := fte.pinfo.prk
        bke.prd        := fte.pinfo.prd
        bke.rdVld      := fte.rinfo.rdVld
        bke.op         := fte.op(6, 0)
        bke.imm        := fte.imm
        bke.robIdx     := robIdx
        bke.bdbIdx     := bdbIdx
        bke.prjWk      := prjInfo.ready && fte.pinfo.prjWk
        bke.prkWk      := prkInfo.ready && fte.pinfo.prkWk
        bke.prjLpv     := prjInfo.lpv
        bke.prkLpv     := prkInfo.lpv
        bke
    }
}
