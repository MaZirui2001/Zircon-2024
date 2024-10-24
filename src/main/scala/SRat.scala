import chisel3._
import chisel3.util._
import CPU_Config.RegisterFile._

class SRat_RAM(rw: Int) extends Module {
    val io = IO(new Bundle {
        val addr  = Input(Vec(3 * rw, UInt(nlreg.W)))
        val rdata = Output(Vec(3 * rw, UInt(log2Ceil(npreg).W)))
        val wdata = Input(Vec(3 * rw, UInt(log2Ceil(npreg).W)))
        val wen   = Input(Vec(3 * rw, Bool()))
    })
    val ram = RegInit(VecInit.tabulate(nlreg)(i => 0.U(log2Ceil(npreg).W)))
    for(i <- 0 until 3 * rw){
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
        io.rdata(i) := Mux1H(io.addr(i), ram)
    }
}

class SRat_IO(rw: Int) extends Bundle{
    val rj          = Input(Vec(rw, UInt(log2Ceil(nlreg).W)))
    val rk          = Input(Vec(rw, UInt(log2Ceil(nlreg).W)))
    val rd          = Input(Vec(rw, UInt(log2Ceil(nlreg).W)))
    val rd_vld      = Input(Vec(rw, Bool()))
    val prd         = Input(Vec(rw, UInt(log2Ceil(npreg).W)))
    val prj         = Output(Vec(rw, UInt(log2Ceil(npreg).W)))
    val prk         = Output(Vec(rw, UInt(log2Ceil(npreg).W)))
    val pprd        = Output(Vec(rw, UInt(log2Ceil(npreg).W)))

    val rdrct_en    = Input(Bool())
    val arat        = Input(Vec(nlreg, UInt(log2Ceil(npreg).W)))
    val srat_rdy    = Output(Bool())
}


class SRat(rw: Int) extends Module {
    val io  = IO(new SRat_IO(rw))
    val rat = Module(new SRat_RAM(rw))

    // rw_w: Read/Write Width
    val rw_w                = 3 * rw
    val rdrct_cnt_max       = ((31 + rw_w) / rw_w)
    val rdrct_cnt           = RegInit((rdrct_cnt_max - 2).U((log2Ceil(rdrct_cnt_max)+1).W))
    val free                = rdrct_cnt(log2Ceil(rdrct_cnt_max))
    val rdrct_wr_idx_1h     = RegInit(VecInit.tabulate(rw_w)(i => (1 << i).U(32.W)))
    val rdrct_wdata         = RegInit(MixedVecInit(Seq.tabulate(rw_w)(i => VecInit.tabulate((31+rw_w-i)/rw_w)(j => io.arat(j * rw_w + i)))))

    when(io.rdrct_en){
        rdrct_cnt := (rdrct_cnt_max - 2).U
        rdrct_wr_idx_1h := VecInit.tabulate(rw_w)(i => (1 << i).U(32.W))
        rdrct_wdata := MixedVecInit(Seq.tabulate(rw_w)(i => VecInit.tabulate((31+rw_w-i)/rw_w)(j => io.arat(j * rw_w + i))))
    }.elsewhen(!rdrct_cnt(log2Ceil(rdrct_cnt_max))){
        rdrct_cnt := rdrct_cnt - 1.U
        rdrct_wr_idx_1h.foreach{ r => r := r.take(r.getWidth-rw_w) ## 0.U(rw_w.W)}
        rdrct_wdata.foreach{ r => r := (r.asUInt(r.asUInt.getWidth-1, r.asUInt.getWidth-log2Ceil(npreg)) ## r.asUInt(r.asUInt.getWidth-1, log2Ceil(npreg))).asTypeOf(r)}
    }

    for(i <- 0 until 3 * rw){
        // rj
        if (i < rw){
            rat.io.addr(i) := Mux(free, UIntToOH(io.rj(i)), rdrct_wr_idx_1h(i))
            rat.io.wen(i) := !free && rdrct_wr_idx_1h(i).orR
            rat.io.wdata(i) := rdrct_wdata(i)(0)
            io.prj(i) := rat.io.rdata(i)
        }
        // rk
        else if (i < 2 * rw){
            rat.io.addr(i) := Mux(free, UIntToOH(io.rk(i % rw)), rdrct_wr_idx_1h(i))
            rat.io.wen(i) := !free && rdrct_wr_idx_1h(i).orR
            rat.io.wdata(i) := rdrct_wdata(i)(0)
            io.prk(i % rw) := rat.io.rdata(i)
        }
        // rd
        else{
            rat.io.addr(i) := Mux(free, UIntToOH(io.rd(i % rw)), rdrct_wr_idx_1h(i))
            rat.io.wen(i) := !free && rdrct_wr_idx_1h(i).orR || io.rd_vld(i % rw)
            rat.io.wdata(i) := Mux(free, io.prd(i % rw), rdrct_wdata(i)(0))
            io.pprd(i % rw) := rat.io.rdata(i)
        }
    }
    io.srat_rdy := free
}