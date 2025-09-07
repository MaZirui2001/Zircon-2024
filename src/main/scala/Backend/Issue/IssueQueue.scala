import chisel3._
import chisel3.util._
import ZirconConfig.RegisterFile._
import ZirconConfig.Issue._   
import ZirconConfig.Commit._
import ZirconConfig.Decode._
import ZirconUtil._
import Log2OH._

class ReplayBusPkg extends Bundle {
    val prd     = UInt(wpreg.W)
    val replay  = Bool()
}
class WakeupBusPkg extends Bundle {
    val prd = UInt(wpreg.W)
    val lpv = UInt(3.W)
    def apply(pkg: BackendPackage, rplyBus: ReplayBusPkg, MemStage: Int = 0): WakeupBusPkg = {
        assert(PopCount(MemStage.U) <= 1.U, "Correct MemStage must be 0 or one-hot.")
        val wk = Wire(new WakeupBusPkg)
        wk.prd := Mux(rplyBus.replay && (if(MemStage != 0) true.B else (pkg.prjLpv | pkg.prkLpv).orR), 0.U, pkg.prd)
        wk.lpv := pkg.prjLpv | pkg.prkLpv | MemStage.U
        wk
    }
}



class IQEntry(num: Int) extends Bundle {
    val instExi    = Bool()
    val item       = new BackendPackage()
    // for memory access partial unordered execution
    // for load, stBefore is the number of load instructions in the queue
    // for store, stBefore is the number of instructions in the queue
    val stBefore   = UInt((log2Ceil(num)+1).W)

    def apply(item: BackendPackage, stBefore: UInt): IQEntry = {
        val e = Wire(new IQEntry(num))
        e.instExi  := true.B
        e.item     := item
        e.stBefore := stBefore
        e
    }

    def stateUpdate(wakeBus: Vec[WakeupBusPkg], rplyBus: ReplayBusPkg, deqItem: Seq[DecoupledIO[BackendPackage]], isMem: Boolean): IQEntry = {
        if(isMem){ val e = this.wakeup(wakeBus, rplyBus, deqItem, isMem).lpvUpdate(wakeBus, rplyBus); e}
        else{ val e = this.wakeup(wakeBus, rplyBus, deqItem, isMem).lpvUpdate(wakeBus, rplyBus); e}
    }
    
    def wakeup(wakeBus: Vec[WakeupBusPkg], rplyBus: ReplayBusPkg, deqItem: Seq[DecoupledIO[BackendPackage]], isMem: Boolean): IQEntry = {
        val e = WireDefault(this)
        val prjWkNxt = Mux(this.item.prjLpv.orR && rplyBus.replay, false.B, this.item.prjWk || wakeBus.map(_.prd === this.item.prj).reduce(_ || _) || rplyBus.prd === this.item.prj)
        val prkWkNxt = Mux(this.item.prkLpv.orR && rplyBus.replay, false.B, this.item.prkWk || wakeBus.map(_.prd === this.item.prk).reduce(_ || _) || rplyBus.prd === this.item.prk)
        val stBeforeNxt = if(isMem) this.stBefore - PopCount(deqItem.map{case s => s.valid && s.ready && Mux(this.item.op(6), true.B, s.bits.op(6))}) else this.stBefore
        e.stBefore := stBeforeNxt
        e.item.prjWk := prjWkNxt
        e.item.prkWk := prkWkNxt
        e.item.prAllWk := prjWkNxt && prkWkNxt && (if(isMem) stBeforeNxt(log2Ceil(num)) else true.B) 
        e
    }

    def lpvUpdate(wakeBus: Vec[WakeupBusPkg], rplyBus: ReplayBusPkg): IQEntry = {
        val e = WireDefault(this)
        e.item.prjLpv := Mux(this.item.prjLpv.orR || this.item.prj === 0.U, this.item.prjLpv << !rplyBus.replay, MuxOH(wakeBus.map(_.prd === this.item.prj), wakeBus.map(_.lpv << 1)))
        e.item.prkLpv := Mux(this.item.prkLpv.orR || this.item.prk === 0.U, this.item.prkLpv << !rplyBus.replay, MuxOH(wakeBus.map(_.prd === this.item.prk), wakeBus.map(_.lpv << 1)))
        e
    }
    def stBeforeUpdate(stItem: Seq[DecoupledIO[BackendPackage]]): IQEntry = {
        val e = WireDefault(this)
        e
    }
}


class SelectItem(len: Int, ageLen: Int) extends Bundle {
    val idxOH = UInt(len.W)
    val vld = Bool()
    val age = UInt(ageLen.W)
    val e = new BackendPackage
}

object SelectItem{
    def apply(idxOH: UInt, vld: Bool, age: UInt, e: BackendPackage): SelectItem = {
        val si = Wire(new SelectItem(idxOH.getWidth, age.getWidth))
        si.idxOH := idxOH & Fill(idxOH.getWidth, vld)
        si.vld := vld
        si.age := age
        si.e := e
        si
    }
}

class IssueQueueDBGIO extends Bundle {
    val fullCycle = UInt(64.W)
}

class IssueQueueIO(ew: Int, dw: Int, num: Int) extends Bundle {
    val enq     = Vec(ew, Flipped(DecoupledIO(new BackendPackage)))
    val deq     = Vec(dw, DecoupledIO(new BackendPackage))
    val wakeBus = Input(Vec(nis, new WakeupBusPkg))
    val rplyBus = Input(new ReplayBusPkg)
    val stLeft  = Output(UInt(log2Ceil(num).W))
    val flush   = Input(Bool())
    val dbg     = Output(new IssueQueueDBGIO)
}

class IssueQueue(ew: Int, dw: Int, num: Int, isMem: Boolean = false) extends Module {
    val io = IO(new IssueQueueIO(ew, dw, num))
    // assert(ew >= dw, "enq width must be greater than or equal to deq width")
    assert(num % dw == 0, "Issue Queue length must be a multiple of deq width")
    assert(num % ew == 0, "Issue Queue length must be a multiple of enq width")

    val len = num / dw
    val n = dw

    val iq = RegInit(
        VecInit.fill(n)(VecInit.fill(len)(0.U.asTypeOf(new IQEntry(num))))
    )
    
    val fList = Module(new ClusterIndexFIFO(
        UInt((log2Ceil(n)+log2Ceil(len)).W), num, dw, ew, 0, 0, true, 
        Some(Seq.tabulate(num)(i => ((i / len) << log2Ceil(len) | (i % len)).U((log2Ceil(n) + log2Ceil(len)).W)))
    ))
    
    fList.io.enq.foreach(_.valid := false.B) 
    fList.io.enq.foreach(_.bits := DontCare)
    fList.io.deq.foreach(_.ready := false.B)
    
    // val stLeft      = RegInit((-1).U((log2Ceil(num)+1).W))
    val stLeft      = RegInit(Fill(log2Ceil(num)+1, 1.U(1.W)))
    val stLeftNext  = WireDefault(VecInit.fill(ew)(0.U((log2Ceil(num)+1).W)))
    val memLeft     = RegInit(Fill(log2Ceil(num)+1, 1.U(1.W)))
    val memLeftNext = WireDefault(VecInit.fill(ew)(0.U((log2Ceil(num)+1).W)))
    if(isMem){
        stLeftNext.zipWithIndex.foreach{ case (e, i) =>
            e := stLeft + PopCount(io.enq.take(i).map{case (e) => e.bits.op(6) && e.valid && e.ready}) 
        }
        stLeft := Mux(io.flush, Fill(log2Ceil(num)+1, 1.U(1.W)), stLeft + PopCount(io.enq.take(ew).map{case (e) => e.bits.op(6) && e.valid && e.ready}) - PopCount(io.deq.map{case (e) => e.bits.op(6) && e.valid && e.ready}))
        memLeftNext.zipWithIndex.foreach{ case (e, i) =>
            e := memLeft + PopCount(io.enq.take(i).map{case (e) => e.valid && e.ready}) 
        }
        memLeft := Mux(io.flush, Fill(log2Ceil(num)+1, 1.U(1.W)), memLeft + PopCount(io.enq.take(ew).map{case (e) => e.valid && e.ready}) - PopCount(io.deq.map{case (e) => e.valid && e.ready}))
    }
    io.stLeft := stLeft

    /* insert into iq */
    // allocate free item in iq
    fList.io.deq.zipWithIndex.foreach{ case (deq, i) => 
        deq.ready := io.enq(i).valid
    }
    val freeIQ     = fList.io.deq.map((_.bits >> log2Ceil(len)))
    val freeItem   = fList.io.deq.map(_.bits(log2Ceil(len)-1, 0))
    val enqEntries = WireDefault(VecInit(io.enq.map(_.bits)))
    fList.io.flush := false.B

    /* wake up */
    iq.foreach{case (qq) =>
        qq.foreach{case (e) =>
            e := e.stateUpdate(io.wakeBus, io.rplyBus, io.deq, isMem)
        }
    }
    /* write to iq */
    for (i <- 0 until ew){
        when(io.enq(i).valid && fList.io.deq(0).valid){
            iq(freeIQ(i))(freeItem(i)) := (new IQEntry(len))(
                enqEntries(i), 
                if(isMem) Mux(enqEntries(i).op(6), memLeftNext(i), stLeftNext(i)) 
                else 0.U
            ).stateUpdate(io.wakeBus, io.rplyBus, io.deq, isMem)
        }
    }

    /* select dw items from iq */
    fList.io.enq.foreach(_.valid := false.B)
    fList.io.enq.foreach(_.bits := DontCare)
    for(i <- 0 until dw){
        // get the oldest valid existing instruction
        val issueValid = iq(i).map{ case (e) => e.item.prAllWk && e.instExi }
        val issueAge   = iq(i).map{ case (e) => e.item.robIdx.getAge }
        val selectItem = VecInit.tabulate(len)(j => SelectItem(
            idxOH = (1 << j).U(len.W),
            vld = issueValid(j),
            age = issueAge(j),
            e = iq(i)(j).item
        )).reduceTree((a, b) => {
            val selectA = a.vld && (!b.vld || ESltu(a.age, b.age))
            Mux(selectA, a, b)
        })
        
        io.deq(i).valid := selectItem.vld
        io.deq(i).bits := Mux(selectItem.vld, selectItem.e, 0.U.asTypeOf(new BackendPackage))
        // caculate if the selected instruction is the latest, just for load instructions
        if(isMem){
            val loadInQueue = iq(i).map{ case (e) => e.instExi }
            val selectLatest = VecInit.tabulate(len)(j => SelectItem(
                idxOH = (1 << j).U(len.W),
                vld = loadInQueue(j),
                age = issueAge(j),
                e = iq(i)(j).item
            )).reduceTree((a, b) => {
                val selectA = a.vld && (!b.vld || ESltu(a.age, b.age))
                Mux(selectA, a, b)
            })
            io.deq(i).bits.isLatest := selectItem.idxOH === selectLatest.idxOH
        }

        // make the selected instruction not exist
        iq(i).zipWithIndex.foreach{ case (e, j) =>
            when(selectItem.idxOH(j) && io.deq(i).ready){ 
                e.instExi := false.B 
                // e.item.prjWk := false.B
                // e.item.prkWk := false.B
            }
        }
    }
    /* replay */
    iq.foreach{case (qq) =>
        qq.foreach{case (e) =>
            when(e.item.prjLpv.orR && io.rplyBus.replay && e.item.valid){
                e.instExi := true.B
            }
            when(e.item.prkLpv.orR && io.rplyBus.replay && e.item.valid){
                e.instExi := true.B
            }
        }
    }

    var fListInsertPtr   = 1.U(n.W)
    val portMapFlst      = VecInit.fill(dw)(0.U(dw.W))
    val portMapTransFlst = Transpose(portMapFlst)
    val readyToRecycle   = iq.map{ case (qq) => qq.map{ case (e) => e.item.valid && !(e.instExi || e.item.prjLpv.orR || e.item.prkLpv.orR) } }
    val selectRecycleIdx = readyToRecycle.map{ case (qq) => VecInit(Log2OH(qq)).asUInt}
    for(i <- 0 until dw) {
        portMapFlst(i) := Mux(selectRecycleIdx(i).orR, fListInsertPtr, 0.U)
        fListInsertPtr = Mux(selectRecycleIdx(i).orR, ShiftAdd1(fListInsertPtr), fListInsertPtr)
    }
    fList.io.enq.zipWithIndex.foreach{ case (e, i) =>
        e.valid := portMapTransFlst(i).orR
        e.bits  := Mux1H(portMapTransFlst(i), VecInit.tabulate(dw)(j => j.U(log2Ceil(dw).W))) ## OHToUInt(Mux1H(portMapTransFlst(i), selectRecycleIdx)).take(log2Ceil(len))
    }
    iq.zipWithIndex.foreach{ case (qq, i) =>
        qq.zipWithIndex.foreach{ case (e, j) =>
            when(selectRecycleIdx(i)(j)){
                e.item.valid := false.B
            }
        }
    }
    io.enq.foreach(_.ready := fList.io.deq.map(_.valid).reduce(_ && _))
    /* flush */
    when(io.flush){
        iq.foreach{case (qq) =>
            qq.foreach{case (e) =>
                e.instExi := false.B
            }
        }
    }
    val fullCycleReg = RegInit(0.U(64.W))
    fullCycleReg     := fullCycleReg + !io.enq.map(_.ready).reduce(_ && _)
    io.dbg.fullCycle := fullCycleReg
}