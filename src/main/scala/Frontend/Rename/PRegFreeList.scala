import chisel3._
import chisel3.util._
import ZirconConfig.RegisterFile._
import ZirconConfig.Commit._
import ZirconConfig.Decode._
import ZirconUtil._

class FreeListFrontendIO extends Bundle{
    val deq   = Vec(ndcd, Decoupled(UInt(wpreg.W)))
}

class FreeListCommitIO extends Bundle{
    val enq   = Vec(ncommit, Flipped(Decoupled(UInt(wpreg.W))))
    val flush = Input(Bool())
}

class FreeListDiffIO extends Bundle{
    val fList = Output(Vec(npreg-32, UInt(wpreg.W)))
}
class FreeListDBGIO extends Bundle{
    val fListEmptyCycle = Output(UInt(64.W))
}

class FreeListIO extends Bundle{
    val fte = new FreeListFrontendIO
    val cmt = new FreeListCommitIO
    // val dif = new FreeListDiffIO
    val dbg = new FreeListDBGIO
}

class PRegFreeList extends Module{
    val io = IO(new FreeListIO)

    val fList = Module(new ClusterIndexFIFO(UInt(wpreg.W), npreg-32, ncommit, ndcd, 0, 0, true, Some(Seq.tabulate(npreg-32)(i => (i+32).U(wpreg.W)))))
    /* calculate the port map
        portMap(i) means io port i is connected to the portMap(i)th enq port
        it traverse is the enq port mapped to the io port
    */
    val portMapFte      = VecInit.fill(ndcd)(0.U(ndcd.W))
    val portMapTransFte = Transpose(portMapFte)
    var validPtrFte     = 1.U(ndcd.W)
    for(i <- 0 until ndcd) {
        portMapFte(i) := Mux(io.fte.deq(i).ready, validPtrFte, 0.U)
        validPtrFte   = Mux(io.fte.deq(i).ready, ShiftAdd1(validPtrFte), validPtrFte)
    }
    // rename stage
    fList.io.deq.zipWithIndex.foreach{ case (d, i) =>
        d.ready := portMapTransFte(i).orR
    }
    io.fte.deq.zipWithIndex.foreach{ case (d, i) =>
        d.valid := fList.io.deq(i).valid
        d.bits  := Mux1H(portMapFte(i), fList.io.deq.map(_.bits))
    }
    // commit stage
    val portMapEnq      = VecInit.fill(ncommit)(0.U(ncommit.W))
    val portMapTransEnq = Transpose(portMapEnq)
    var validPtrEnq     = 1.U(ncommit.W)
    
    for(i <- 0 until ncommit) {
        portMapEnq(i) := Mux(io.cmt.enq(i).valid, validPtrEnq, 0.U)
        validPtrEnq   = Mux(io.cmt.enq(i).valid, ShiftAdd1(validPtrEnq), validPtrEnq)
    }
    fList.io.enq.zipWithIndex.foreach{ case (e, i) =>
        e.valid := portMapTransEnq(i).orR
        e.bits  := Mux1H(portMapTransEnq(i), io.cmt.enq.map(_.bits))
    }
    io.cmt.enq.foreach(_.ready := DontCare)
    fList.io.flush := ShiftRegister(io.cmt.flush, 1, false.B, true.B)
    // io.dif.fList   := fList.io.dbgFIFO

    val fListEmptyCycleReg = RegInit(0.U(64.W))
    fListEmptyCycleReg     := fListEmptyCycleReg + !io.fte.deq.map(_.valid).reduce(_ && _)
    io.dbg.fListEmptyCycle := fListEmptyCycleReg
}
    