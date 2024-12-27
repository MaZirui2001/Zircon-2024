import chisel3._
import chisel3.util._
import CPU_Config.RegisterFile._
import Zircon_Util._

class SRat_RAM(rn_w: Int) extends Module {
    val io = IO(new Bundle {
        val addr  = Input(Vec(3 * rn_w, UInt(nlreg.W)))
        val rdata = Output(Vec(3 * rn_w, UInt(wpreg.W)))
        val wdata = Input(Vec(3 * rn_w, UInt(wpreg.W)))
        val wen   = Input(Vec(3 * rn_w, Bool()))
        val dbg   = Output(Vec(nlreg, UInt(wpreg.W)))
    })
    val ram = RegInit(VecInit.tabulate(nlreg)(i => 0.U(wpreg.W)))
    for(i <- 0 until 3 * rn_w){
        when(io.wen(i)){
            for(j <- 0 until nlreg){
                when(io.addr(i)(j)){
                    ram(j) := io.wdata(i)
                }
            }
        }
        // io.rdata(i) := VecInit(ram.zip(io.addr(i).asBools).zipWithIndex.map({case ((r, a), j) => 
        //     r & Fill(npreg, a)
        // })).reduceTree(_ | _)
        io.rdata(i) := MuxOH(io.addr(i), ram)
    }
    io.dbg := ram
}

class SRat_IO(rn_w: Int) extends Bundle{
    val rj          = Input(Vec(rn_w, UInt(wlreg.W)))
    val rk          = Input(Vec(rn_w, UInt(wlreg.W)))
    val rd          = Input(Vec(rn_w, UInt(wlreg.W)))
    val rd_vld      = Input(Vec(rn_w, Bool()))
    val prd         = Input(Vec(rn_w, UInt(wpreg.W)))
    val prj         = Output(Vec(rn_w, UInt(wpreg.W)))
    val prk         = Output(Vec(rn_w, UInt(wpreg.W)))
    val pprd        = Output(Vec(rn_w, UInt(wpreg.W)))

    val rdrct_en    = Input(Bool())
    val arat        = Input(Vec(nlreg, UInt(wpreg.W)))
    val srat_rdy    = Output(Bool())

    val dbg_srat    = Output(Vec(nlreg, UInt(wpreg.W)))
}


class SRat(rn_w: Int) extends Module {
    val io  = IO(new SRat_IO(rn_w))
    val rat = Module(new SRat_RAM(rn_w))

    // rw_w: Read/Write Width
    val rw_w                = 3 * rn_w
    val rdrct_cnt_max       = ((31 + rw_w) / rw_w)
    val rdrct_cnt           = RegInit(Fill(log2Ceil(rdrct_cnt_max)+1, true.B).asUInt)
    val free                = rdrct_cnt(log2Ceil(rdrct_cnt_max))
    val rdrct_wr_idx_1h     = RegInit(VecInit.tabulate(rw_w)(i => (1 << i).U(32.W)))
    val rdrct_wdata_init    = MixedVecInit(Seq.tabulate(rw_w)(i => 
                              VecInit.tabulate((31+rw_w-i)/rw_w)(j => io.arat(j * rw_w + i))))
    val rdrct_wdata         = RegInit(rdrct_wdata_init)

    when(!rdrct_cnt(log2Ceil(rdrct_cnt_max))){
        rdrct_cnt := rdrct_cnt - 1.U
        rdrct_wr_idx_1h.foreach{ r => r := r.take(r.getWidth-rw_w) ## 0.U(rw_w.W)}
        rdrct_wdata.foreach{ r => r := (r(r.length-1).asUInt ## VecInit(r.drop(1)).asUInt).asTypeOf(r)}
    }.elsewhen(io.rdrct_en){
        rdrct_cnt := (rdrct_cnt_max - 1).U
        rdrct_wr_idx_1h := VecInit.tabulate(rw_w)(i => (1 << i).U(32.W))
        rdrct_wdata := rdrct_wdata_init
    }

    for(i <- 0 until 3 * rn_w){
        // rj
        if (i < rn_w){
            rat.io.addr(i) := Mux(free, UIntToOH(io.rj(i)), rdrct_wr_idx_1h(i))
            rat.io.wen(i) := !free && rdrct_wr_idx_1h(i).orR
            rat.io.wdata(i) := rdrct_wdata(i)(0)
            io.prj(i) := rat.io.rdata(i)
        }
        // rk
        else if (i < 2 * rn_w){
            rat.io.addr(i) := Mux(free, UIntToOH(io.rk(i % rn_w)), rdrct_wr_idx_1h(i))
            rat.io.wen(i) := !free && rdrct_wr_idx_1h(i).orR
            rat.io.wdata(i) := rdrct_wdata(i)(0)
            io.prk(i % rn_w) := rat.io.rdata(i)
        }
        // rd
        else{
            rat.io.addr(i) := Mux(free, UIntToOH(io.rd(i % rn_w)), rdrct_wr_idx_1h(i))
            rat.io.wen(i) := Mux(free, io.rd_vld(i % rn_w), rdrct_wr_idx_1h(i).orR)
            rat.io.wdata(i) := Mux(free, io.prd(i % rn_w), rdrct_wdata(i)(0))
            io.pprd(i % rn_w) := rat.io.rdata(i)
        }
    }
    io.srat_rdy := free
    io.dbg_srat := rat.io.dbg
}