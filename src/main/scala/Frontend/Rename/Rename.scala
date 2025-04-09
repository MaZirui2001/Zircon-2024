import chisel3._
import chisel3.util._
import Zircon_Config.RegisterFile._
import Zircon_Config.Decode._
import Zircon_Util._


class PRegister_Info extends Bundle {
    val prj     = UInt(wpreg.W)
    val prk     = UInt(wpreg.W)
    val prd     = UInt(wpreg.W)
    val pprd    = UInt(wpreg.W)
    val prj_wk  = Bool()
    val prk_wk  = Bool()
}

class Rename_Frontend_IO extends Bundle {
    val rinfo   = Vec(ndcd, Flipped(Decoupled(new Register_Info)))
    val pinfo   = Output(Vec(ndcd, new PRegister_Info))
}

class Rename_Commit_IO extends Bundle {
    val flst = new Free_List_Commit_IO
    val srat = new SRat_Commit_IO
}
class Rename_Diff_IO extends Bundle {
    val flst = new Free_List_Diff_IO
    val srat = new SRat_Diff_IO
}

class Rename_IO extends Bundle {    
    val fte = new Rename_Frontend_IO
    val cmt = new Rename_Commit_IO
    val dif = new Rename_Diff_IO
}

class Rename extends Module {
    val io = IO(new Rename_IO)
    val flst = Module(new PReg_Free_List)
    val srat = Module(new SRat)
    // free list: 
    flst.io.cmt <> io.cmt.flst
    flst.io.fte.deq.zip(io.fte.rinfo).foreach{ case (d, rinfo) =>
        d.ready := rinfo.bits.rd_vld && rinfo.valid
    }
    io.fte.rinfo.foreach(_.ready := flst.io.fte.deq.map(_.valid).reduce(_ && _))
    io.fte.pinfo.zip(flst.io.fte.deq.map(_.bits)).foreach{ case (pinfo, prd) => pinfo.prd := prd }
    // srat
    srat.io.cmt         <> io.cmt.srat
    srat.io.rnm.rj      := io.fte.rinfo.map(_.bits.rj)
    srat.io.rnm.rk      := io.fte.rinfo.map(_.bits.rk)
    srat.io.rnm.rd      := io.fte.rinfo.map(_.bits.rd)
    srat.io.rnm.rd_vld  := io.fte.rinfo.map{ case(r) => r.bits.rd_vld && r.valid && r.ready } // no matter whether the rd is the same, becuase srat-write is bigger index first
    srat.io.rnm.prd     := io.fte.pinfo.map(_.prd)

    // RAW: 
    def raw(rs: UInt, rds: Seq[UInt]): Bool = {
        val n = rds.length
        if(n == 0) return false.B
        val idx1H = VecInit.tabulate(n){i => (rs === rds(i))}
        idx1H.asUInt.orR
    }
    def raw_idx1H(rs: UInt, rds: Seq[UInt]): UInt = {
        val n = rds.length
        if(n == 0) return 0.U
        val idx1H = VecInit.tabulate(n){i => (rs === rds(i))}
        Log2OH(idx1H)
    }
    def raw_read(rs: UInt, rds: Seq[UInt], prs: UInt, prds: Seq[UInt]): UInt = {
        val n = rds.length
        if(n == 0) return prs
        val idx1H = raw_idx1H(rs, rds)
        Mux(idx1H.orR, Mux1H(idx1H, prds), prs)
    }
    io.fte.pinfo.zipWithIndex.foreach{ case (pinfo, i) =>
        pinfo.prj   := raw_read(io.fte.rinfo(i).bits.rj, io.fte.rinfo.map(_.bits.rd).take(i), srat.io.rnm.prj(i), flst.io.fte.deq.map(_.bits).take(i))
        pinfo.prk   := raw_read(io.fte.rinfo(i).bits.rk, io.fte.rinfo.map(_.bits.rd).take(i), srat.io.rnm.prk(i), flst.io.fte.deq.map(_.bits).take(i))
        pinfo.pprd  := raw_read(io.fte.rinfo(i).bits.rd, io.fte.rinfo.map(_.bits.rd).take(i), srat.io.rnm.pprd(i), flst.io.fte.deq.map(_.bits).take(i))
        // for prj amd prk, this stage will judge whether raw in the group, in order to initially set the prj_wk and prk_wk
        pinfo.prj_wk := !raw(io.fte.rinfo(i).bits.rj, io.fte.rinfo.map(_.bits.rd).take(i))
        pinfo.prk_wk := !raw(io.fte.rinfo(i).bits.rk, io.fte.rinfo.map(_.bits.rd).take(i))
    }
    
    io.dif.srat.rename_table := srat.io.dif.rename_table
    io.dif.flst.free_list   := flst.io.dif.free_list
}