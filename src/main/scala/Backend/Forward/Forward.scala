import chisel3._
import chisel3.util._
import Zircon_Config.Issue._

class Forward_IO extends Bundle {
    val inst_pkg_wb  = Input(Vec(nis, new Backend_Package))
    // only forward arith pipeline
    val inst_pkg_ex  = Input(Vec(4, new Backend_Package))
    val src1_fwd     = Vec(4, Decoupled(UInt(32.W)))
    val src2_fwd     = Vec(4, Decoupled(UInt(32.W)))
}

class Forward extends Module {
    val io = IO(new Forward_IO)

    io.src1_fwd.zipWithIndex.foreach{ case (fwd, i) =>
        val fwd_en = VecInit.tabulate(nis){ j =>
            io.inst_pkg_ex(i).prj === io.inst_pkg_wb(j).prd && io.inst_pkg_wb(j).rd_vld
        }
        fwd.valid := fwd_en.reduce(_ || _)
        fwd.bits  := Mux1H(fwd_en, io.inst_pkg_wb.map(_.rf_wdata))
    }

    io.src2_fwd.zipWithIndex.foreach{ case (fwd, i) =>
        val fwd_en = VecInit.tabulate(nis){ j =>
            io.inst_pkg_ex(i).prk === io.inst_pkg_wb(j).prd && io.inst_pkg_wb(j).rd_vld
        }
        fwd.valid := fwd_en.reduce(_ || _)
        fwd.bits  := Mux1H(fwd_en, io.inst_pkg_wb.map(_.rf_wdata))
    }
}
