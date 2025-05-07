import chisel3._
import chisel3.util._
import ZirconConfig.RegisterFile._
import ZirconConfig.Issue._
import ZirconUtil._

class RegfileRDIO extends Bundle{
    val prj     = Input(UInt(wpreg.W))
    val prk     = Input(UInt(wpreg.W))
    val prjData = Output(UInt(32.W))
    val prkData = Output(UInt(32.W))
}
class RegfileWRIO extends Bundle{
    val prd     = Input(UInt(wpreg.W))
    val prdVld  = Input(Bool())
    val prdData = Input(UInt(32.W))
}

// class RegfilePredictIO extends Bundle {
//     val pra        = Input(UInt(wpreg.W))
//     val praData    = Output(UInt(32.W))
// }

class RegfileDBGIO extends Bundle{
    val rf = Output(Vec(npreg, UInt(32.W)))
}

class RegfileSingleIO extends Bundle{
    val rd = new RegfileRDIO
    val wr = new RegfileWRIO
}

class Regfile extends Module {
    val io  = IO(Vec(nis, new RegfileSingleIO))
    // val predictIO = IO(new RegfilePredictIO)
    val dbg = IO(new RegfileDBGIO)

    val regfile = RegInit(VecInit.tabulate(npreg)(i => 0.U(32.W)))

    for(i <- 0 until nis){
        io(i).rd.prjData := WFirstRead(regfile(io(i).rd.prj), io(i).rd.prj, io.map(_.wr.prd), io.map(_.wr.prdData), io.map(_.wr.prdVld))
        io(i).rd.prkData := WFirstRead(regfile(io(i).rd.prk), io(i).rd.prk, io.map(_.wr.prd), io.map(_.wr.prdData), io.map(_.wr.prdVld))
        when(io(i).wr.prdVld){
            regfile(io(i).wr.prd) := io(i).wr.prdData
        }
    }
    // predictIO.praData := ShiftRegister(regfile(predictIO.pra), 1, 0.U, true.B)
    dbg.rf := regfile
}