import chisel3._
import chisel3.util._
import CPU_Config.RegisterFile._
import CPU_Config.Issue._
import Zircon_Util._

class Regfile_IO extends Bundle{
    val prj         = Input(Vec(wissue, UInt(wpreg.W)))
    val prk         = Input(Vec(wissue, UInt(wpreg.W)))
    val prj_data    = Output(Vec(wissue, UInt(32.W)))
    val prk_data    = Output(Vec(wissue, UInt(32.W)))
    val prd         = Input(Vec(wissue, UInt(wpreg.W)))
    val prd_vld     = Input(Vec(wissue, Bool()))
    val prd_data    = Input(Vec(wissue, UInt(32.W)))
}

class Regfile extends Module {
    val io = IO(new Regfile_IO)

    val regfile = RegInit(VecInit.tabulate(npreg)(i => 0.U(32.W)))

    for(i <- 0 until wissue){
        io.prj_data(i) := wfirst_read(regfile(io.prj(i)), io.prj(i), io.prd, io.prd_data, io.prd_vld)
        io.prk_data(i) := wfirst_read(regfile(io.prk(i)), io.prk(i), io.prd, io.prd_data, io.prd_vld)
        when(io.prd_vld(i)){
            regfile(io.prd(i)) := io.prd_data(i)
        }
    }
}