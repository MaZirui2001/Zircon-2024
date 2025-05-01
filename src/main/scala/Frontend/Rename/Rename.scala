import chisel3._
import chisel3.util._
import ZirconConfig.RegisterFile._
import ZirconConfig.Decode._
import ZirconUtil._


class PRegisterInfo extends Bundle {
    val prj   = UInt(wpreg.W)
    val prk   = UInt(wpreg.W)
    val prd   = UInt(wpreg.W)
    val pprd  = UInt(wpreg.W)
    val prjWk = Bool()
    val prkWk = Bool()
}

class RenameFrontendIO extends Bundle {
    val rinfo  = Vec(ndcd, Flipped(Decoupled(new RegisterInfo)))
    val pinfo  = Output(Vec(ndcd, new PRegisterInfo))
    val pra    = Output(UInt(wpreg.W))
}

class RenameCommitIO extends Bundle {
    val fList = new FreeListCommitIO
    val srat  = new SRatCommitIO
}
class RenameDiffIO extends Bundle {
    val fList = new FreeListDiffIO
    val srat  = new SRatDiffIO
}
class RenameDBGIO extends Bundle {
    val fList = new FreeListDBGIO
}

class RenameIO extends Bundle {    
    val fte = new RenameFrontendIO
    val cmt = new RenameCommitIO
    // val dif = new RenameDiffIO
    val dbg = new RenameDBGIO
}

class Rename extends Module {
    val io    = IO(new RenameIO)
    val fList = Module(new PRegFreeList)
    val srat  = Module(new SRat)
    // free list: 
    fList.io.cmt <> io.cmt.fList
    fList.io.fte.deq.zip(io.fte.rinfo).foreach{ case (d, rinfo) =>
        d.ready := rinfo.bits.rdVld && rinfo.valid
    }
    io.fte.rinfo.foreach(_.ready := fList.io.fte.deq.map(_.valid).reduce(_ && _))
    io.fte.pinfo.zip(fList.io.fte.deq.map(_.bits)).foreach{ case (pinfo, prd) => pinfo.prd := prd }
    // srat
    srat.io.cmt       <> io.cmt.srat
    srat.io.rnm.rj    := io.fte.rinfo.map(_.bits.rj)
    srat.io.rnm.rk    := io.fte.rinfo.map(_.bits.rk)
    srat.io.rnm.rd    := io.fte.rinfo.map(_.bits.rd)
    srat.io.rnm.rdVld := io.fte.rinfo.map{ case(r) => r.bits.rdVld && r.valid && r.ready } // no matter whether the rd is the same, becuase srat-write is bigger index first
    srat.io.rnm.prd   := io.fte.pinfo.map(_.prd)

    // RAW: 
    def raw(rs: UInt, rds: Seq[UInt]): Bool = {
        val n = rds.length
        if(n == 0) return false.B
        val idx1H = VecInit.tabulate(n){i => (rs === rds(i))}
        idx1H.asUInt.orR
    }
    def rawIdx1H(rs: UInt, rds: Seq[UInt]): UInt = {
        val n = rds.length
        if(n == 0) return 0.U
        val idx1H = VecInit.tabulate(n){i => (rs === rds(i))}
        Log2OH(idx1H)
    }
    def rawRead(rs: UInt, rds: Seq[UInt], prs: UInt, prds: Seq[UInt]): UInt = {
        val n = rds.length
        if(n == 0) return prs
        val idx1H = rawIdx1H(rs, rds)
        Mux(idx1H.orR, Mux1H(idx1H, prds), prs)
    }
    io.fte.pinfo.zipWithIndex.foreach{ case (pinfo, i) =>
        pinfo.prj   := rawRead(io.fte.rinfo(i).bits.rj, io.fte.rinfo.map(_.bits.rd).take(i), srat.io.rnm.prj(i), fList.io.fte.deq.map(_.bits).take(i))
        pinfo.prk   := rawRead(io.fte.rinfo(i).bits.rk, io.fte.rinfo.map(_.bits.rd).take(i), srat.io.rnm.prk(i), fList.io.fte.deq.map(_.bits).take(i))
        pinfo.pprd  := rawRead(io.fte.rinfo(i).bits.rd, io.fte.rinfo.map(_.bits.rd).take(i), srat.io.rnm.pprd(i), fList.io.fte.deq.map(_.bits).take(i))
        // for prj amd prk, this stage will judge whether raw in the group, in order to initially set the prjWk and prkWk
        pinfo.prjWk := !raw(io.fte.rinfo(i).bits.rj, io.fte.rinfo.map(_.bits.rd).take(i))
        pinfo.prkWk := !raw(io.fte.rinfo(i).bits.rk, io.fte.rinfo.map(_.bits.rd).take(i))
    }

    io.fte.pra    := srat.io.rnm.pra
    
    // io.dif.srat.renameTable := srat.io.dif.renameTable
    // io.dif.fList.fList      := fList.io.dif.fList

    io.dbg.fList := fList.io.dbg
}