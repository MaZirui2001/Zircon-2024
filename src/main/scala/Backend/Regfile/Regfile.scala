import chisel3._
import chisel3.util._
import Zircon_Config.RegisterFile._
import Zircon_Config.Issue._
import Zircon_Util._

class Regfile_RD_IO extends Bundle{
    val prj         = Input(UInt(wpreg.W))
    val prk         = Input(UInt(wpreg.W))
    val prj_data    = Output(UInt(32.W))
    val prk_data    = Output(UInt(32.W))
}
class Regfile_WR_IO extends Bundle{
    val prd         = Input(UInt(wpreg.W))
    val prd_vld     = Input(Bool())
    val prd_data    = Input(UInt(32.W))
}

class Regfile_DBG_IO extends Bundle{
    val rf = Output(Vec(npreg, UInt(32.W)))
}

class Regfile_Single_IO extends Bundle{
    val rd = new Regfile_RD_IO
    val wr = new Regfile_WR_IO
}

class Regfile extends Module {
    val io  = IO(Vec(nissue, new Regfile_Single_IO))
    val dbg = IO(new Regfile_DBG_IO)

    val regfile = RegInit(VecInit.tabulate(npreg)(i => 0.U(32.W)))

    for(i <- 0 until nissue){
        io(i).rd.prj_data := WFirstRead(regfile(io(i).rd.prj), io(i).rd.prj, io.map(_.wr.prd), io.map(_.wr.prd_data), io.map(_.wr.prd_vld))
        io(i).rd.prk_data := WFirstRead(regfile(io(i).rd.prk), io(i).rd.prk, io.map(_.wr.prd), io.map(_.wr.prd_data), io.map(_.wr.prd_vld))
        when(io(i).wr.prd_vld){
            regfile(io(i).wr.prd) := io(i).wr.prd_data
        }
    }
    dbg.rf := regfile
}