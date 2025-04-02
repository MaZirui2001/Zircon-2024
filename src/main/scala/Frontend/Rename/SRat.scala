import chisel3._
import chisel3.util._
import Zircon_Config.RegisterFile._
import Zircon_Config.Decode._
import Zircon_Config.Commit._
import Zircon_Util._

class SRat_Rename_IO extends Bundle{
    val rj          = Input(Vec(ndecode, UInt(wlreg.W)))
    val rk          = Input(Vec(ndecode, UInt(wlreg.W)))
    val rd          = Input(Vec(ndecode, UInt(wlreg.W)))
    val rd_vld      = Input(Vec(ndecode, Bool()))
    val prd         = Input(Vec(ndecode, UInt(wpreg.W)))
    val prj         = Output(Vec(ndecode, UInt(wpreg.W)))
    val prk         = Output(Vec(ndecode, UInt(wpreg.W)))
    val pprd        = Output(Vec(ndecode, UInt(wpreg.W)))
}

class SRat_Commit_IO extends Bundle{
    val rd_vld      = Input(Vec(ncommit, Bool()))
    val rd          = Input(Vec(ncommit, UInt(wlreg.W)))
    val prd         = Input(Vec(ncommit, UInt(wpreg.W)))
    val flush       = Input(Bool())
}

class SRat_Diff_IO extends Bundle{
    val rename_table = Output(Vec(nlreg, UInt(wpreg.W)))
}

class SRat_IO extends Bundle{
    val rnm = new SRat_Rename_IO
    val cmt = new SRat_Commit_IO
    val dif = new SRat_Diff_IO
}

class SRat extends Module {
    val io  = IO(new SRat_IO)

    val rat_rnm = RegInit(VecInit.tabulate(nlreg)(i => i.U(wpreg.W)))
    val rat_cmt = RegInit(VecInit.tabulate(nlreg)(i => i.U(wpreg.W)))

    // reanme stage
    io.rnm.prj.zipWithIndex.foreach{ case (prj, i) =>
        prj := rat_rnm(io.rnm.rj(i))
    }
    io.rnm.prk.zipWithIndex.foreach{ case (prk, i) =>
        prk := rat_rnm(io.rnm.rk(i))
    }
    io.rnm.pprd.zipWithIndex.foreach{ case (pprd, i) =>
        pprd := rat_rnm(io.rnm.rd(i))
    }
    io.rnm.rd_vld.zipWithIndex.foreach{ case (rd_vld, i) =>
        when(rd_vld){
            rat_rnm(io.rnm.rd(i)) := io.rnm.prd(i)
        }
    }

    // commit stage
    io.cmt.rd_vld.zipWithIndex.foreach{ case (rd_vld, i) =>
        when(rd_vld){
            rat_cmt(io.cmt.rd(i)) := io.cmt.prd(i)
        }
    }

    when(ShiftRegister(io.cmt.flush, 1, false.B, true.B)){
        rat_rnm := rat_cmt
    }

    io.dif.rename_table := rat_rnm 
    
}