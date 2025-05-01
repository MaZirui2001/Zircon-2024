import chisel3._
import chisel3.util._
import ZirconConfig.RegisterFile._
import ZirconConfig.Decode._
import ZirconConfig.Commit._
import ZirconUtil._

class SRatRenameIO extends Bundle{
    val rj    = Input(Vec(ndcd, UInt(wlreg.W)))
    val rk    = Input(Vec(ndcd, UInt(wlreg.W)))
    val rd    = Input(Vec(ndcd, UInt(wlreg.W)))
    val rdVld = Input(Vec(ndcd, Bool()))
    val prd   = Input(Vec(ndcd, UInt(wpreg.W)))
    val prj   = Output(Vec(ndcd, UInt(wpreg.W)))
    val prk   = Output(Vec(ndcd, UInt(wpreg.W)))
    val pprd  = Output(Vec(ndcd, UInt(wpreg.W)))
    val pra   = Output(UInt(wpreg.W))
}

class SRatCommitIO extends Bundle{
    val rdVld = Input(Vec(ncommit, Bool()))
    val rd    = Input(Vec(ncommit, UInt(wlreg.W)))
    val prd   = Input(Vec(ncommit, UInt(wpreg.W)))
    val flush = Input(Bool())
}

class SRatDiffIO extends Bundle{
    val renameTable = Output(Vec(nlreg, UInt(wpreg.W)))
}

class SRatIO extends Bundle{
    val rnm = new SRatRenameIO
    val cmt = new SRatCommitIO
    // val dif = new SRatDiffIO
}

class SRat extends Module {
    val io  = IO(new SRatIO)

    val ratRnm = RegInit(VecInit.tabulate(nlreg)(i => i.U(wpreg.W)))
    val ratCmt = RegInit(VecInit.tabulate(nlreg)(i => i.U(wpreg.W)))

    // reanme stage
    io.rnm.prj.zipWithIndex.foreach{ case (prj, i) =>
        prj := ratRnm(io.rnm.rj(i))
    }
    io.rnm.prk.zipWithIndex.foreach{ case (prk, i) =>
        prk := ratRnm(io.rnm.rk(i))
    }
    io.rnm.pprd.zipWithIndex.foreach{ case (pprd, i) =>
        pprd := ratRnm(io.rnm.rd(i))
    }
    io.rnm.rdVld.zipWithIndex.foreach{ case (rdVld, i) =>
        when(rdVld){
            ratRnm(io.rnm.rd(i)) := io.rnm.prd(i)
        }
    }

    // commit stage
    io.cmt.rdVld.zipWithIndex.foreach{ case (rdVld, i) =>
        when(rdVld){
            ratCmt(io.cmt.rd(i)) := io.cmt.prd(i)
        }
    }

    when(ShiftRegister(io.cmt.flush, 1, false.B, true.B)){
        ratRnm := ratCmt
    }

    io.rnm.pra := ShiftRegister(ratCmt(1), 1, 0.U, true.B)

    // io.dif.renameTable := ratRnm
    
}