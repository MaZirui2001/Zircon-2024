import chisel3._
import chisel3.util._
import Zircon_Config.RegisterFile._
import Zircon_Config.ReserveQueue._
import Zircon_Config.Issue._   
import Zircon_Config.Commit._
import Zircon_Config.Decode._
import Zircon_Util._
import Log2OH._

class Replay_Bus_Pkg extends Bundle {
    val prd     = UInt(wpreg.W)
    val replay  = Bool()
}
class Wakeup_Bus_Pkg extends Bundle {
    val prd = UInt(wpreg.W)
    val lpv = UInt(3.W)
    def apply(pkg: Backend_Package, is_mem: Boolean = false): Wakeup_Bus_Pkg = {
        val wk = Wire(new Wakeup_Bus_Pkg)
        wk.prd := pkg.prd
        wk.lpv := (if(is_mem) pkg.prj_lpv | pkg.prk_lpv | 0x1.U else pkg.prj_lpv | pkg.prk_lpv)
        wk
    }
}

class Select_Item(len: Int, age_len: Int) extends Bundle {
    val idx_1h = UInt(len.W)
    val vld = Bool()
    val age = UInt(age_len.W)
}

object Select_Item{
    def apply(idx_1h: UInt, vld: Bool, age: UInt): Select_Item = {
        val si = Wire(new Select_Item(idx_1h.getWidth, age.getWidth))
        si.idx_1h := idx_1h & Fill(idx_1h.getWidth, vld)
        si.vld := vld
        si.age := age
        si
    }
}


class IQ_Entry(num: Int) extends Bundle {
    val inst_exi    = Bool()
    val item        = new Backend_Package()
    // for memory access partial unordered execution
    val st_before   = UInt(log2Ceil(num).W)

    def apply(item: Backend_Package, st_before: UInt): IQ_Entry = {
        val e = Wire(new IQ_Entry(num))
        e.inst_exi := true.B
        e.item := item
        e.st_before := st_before
        e
    }

    def state_update(wake_bus: Vec[Wakeup_Bus_Pkg], rply_bus: Replay_Bus_Pkg, deq_item: Seq[DecoupledIO[Backend_Package]], is_mem: Boolean): IQ_Entry = {
        if(is_mem){ val e = this.wakeup(wake_bus, rply_bus).lpv_update(wake_bus).st_before_update(deq_item); e}
        else{ val e = this.wakeup(wake_bus, rply_bus).lpv_update(wake_bus); e}
    }
    
    def wakeup(wake_bus: Vec[Wakeup_Bus_Pkg], rply_bus: Replay_Bus_Pkg): IQ_Entry = {
        val e = WireDefault(this)
        e.item.prj_wk := Mux(this.item.prj_lpv.orR && rply_bus.replay, this.item.prj === 0.U, this.item.prj_wk || wake_bus.map(_.prd === this.item.prj).reduce(_ || _) || rply_bus.prd === this.item.prj)
        e.item.prk_wk := Mux(this.item.prk_lpv.orR && rply_bus.replay, this.item.prk === 0.U, this.item.prk_wk || wake_bus.map(_.prd === this.item.prk).reduce(_ || _) || rply_bus.prd === this.item.prk)
        e
    }

    def lpv_update(wake_bus: Vec[Wakeup_Bus_Pkg]): IQ_Entry = {
        val e = WireDefault(this)
        e.item.prj_lpv := Mux(this.item.prj_lpv.orR || this.item.prj === 0.U, this.item.prj_lpv << 1, Mux1H(wake_bus.map(_.prd === this.item.prj), wake_bus.map(_.lpv << 1)))
        e.item.prk_lpv := Mux(this.item.prk_lpv.orR || this.item.prk === 0.U, this.item.prk_lpv << 1, Mux1H(wake_bus.map(_.prd === this.item.prk), wake_bus.map(_.lpv << 1)))
        e
    }
    def st_before_update(st_item: Seq[DecoupledIO[Backend_Package]]): IQ_Entry = {
        val e = WireDefault(this)
        e.st_before := this.st_before - PopCount(st_item.map{case s => s.valid && s.bits.op(5)})
        e
    }
}


class Issue_Queue_IO(ew: Int, dw: Int, num: Int) extends Bundle {
    val enq         = Vec(ew, Flipped(DecoupledIO(new Backend_Package)))
    val deq         = Vec(dw, DecoupledIO(new Backend_Package))
    val wake_bus    = Input(Vec(nissue, new Wakeup_Bus_Pkg))
    val rply_bus    = Input(new Replay_Bus_Pkg)
    val st_left     = Output(UInt(log2Ceil(num).W))
    val flush       = Input(Bool())
}

class Issue_Queue(ew: Int, dw: Int, num: Int, is_mem: Boolean = false) extends Module {
    val io = IO(new Issue_Queue_IO(ew, dw, num))
    assert(ew >= dw, "enq width must be greater than or equal to deq width")
    assert(num % dw == 0, "Issue Queue length must be a multiple of deq width")
    assert(num % ew == 0, "Issue Queue length must be a multiple of enq width")

    val len = num / dw
    val n = dw

    val iq = RegInit(
        VecInit.fill(n)(VecInit.fill(len)(0.U.asTypeOf(new IQ_Entry(num))))
    )
    
    val flst = Module(new Cluster_Index_FIFO(UInt((log2Ceil(n)+log2Ceil(len)).W), num, dw, ew, 0, 0, true, Some(Seq.tabulate(num)(i => ((i / len) << log2Ceil(len) | (i % len)).U((log2Ceil(n) + log2Ceil(len)).W)))))
    
    // 在flst进行特定设置
    flst.io.enq.foreach(_.valid := false.B) 
    flst.io.enq.foreach(_.bits := DontCare)
    flst.io.deq.foreach(_.ready := false.B)
    
    val st_left = RegInit(0.U(log2Ceil(num).W))
    if(is_mem){
        st_left := Mux(io.flush, 0.U, (st_left + PopCount(io.enq.map{case (e) => e.bits.op(5) && e.valid && e.ready})
                                               - PopCount(io.deq.map{case (e) => e.bits.op(5) && e.valid && e.ready})))
    }
    io.st_left := st_left
    /* insert into iq */
    // allocate free item in iq
    flst.io.deq.zipWithIndex.foreach{ case (deq, i) => 
        deq.ready := io.enq(i).valid
    }
    val free_iq = flst.io.deq.map((_.bits >> log2Ceil(len)))
    val free_item = flst.io.deq.map(_.bits(log2Ceil(len)-1, 0))
    val enq_entries = WireDefault(VecInit(io.enq.map(_.bits)))
    flst.io.flush := false.B

    /* wake up */
    iq.foreach{case (qq) =>
        qq.foreach{case (e) =>
            e := e.state_update(io.wake_bus, io.rply_bus, io.deq, is_mem)
        }
    }
    // write to iq
    for (i <- 0 until ew){
        when(io.enq(i).valid && flst.io.deq(0).valid){
            iq(free_iq(i))(free_item(i)) := (new IQ_Entry(len))(enq_entries(i), if(is_mem) st_left else 0.U).state_update(io.wake_bus, io.rply_bus, io.deq, is_mem)
        }
    }

    /* select dw items from iq */
    flst.io.enq.foreach(_.valid := false.B)
    flst.io.enq.foreach(_.bits := DontCare)
    for(i <- 0 until dw){
        // get the oldest valid existing instruction
        val issue_valid = iq(i).map{ case (e) => e.item.prj_wk && e.item.prk_wk && e.inst_exi && (if(is_mem) e.st_before === 0.U else true.B)}
        val issue_age = iq(i).map{ case (e) => e.item.rob_idx.get_age }
        val select_item = VecInit.tabulate(len)(j => Select_Item(
            idx_1h = (1 << j).U(len.W),
            vld = issue_valid(j),
            age = issue_age(j)
        )).reduceTree((a, b) => Mux(a.vld, Mux(b.vld, Mux(ESltu(a.age, b.age), a, b), a), b))
        
        io.deq(i).valid := select_item.vld
        io.deq(i).bits := Mux1H(select_item.idx_1h, iq(i).map(_.item))
        // caculate if the selected instruction is the latest, just for load instructions
        if(is_mem){
            val load_in_queue = iq(i).map{ case (e) => e.inst_exi }
            val select_latest = VecInit.tabulate(len)(j => Select_Item(
                idx_1h = (1 << j).U(len.W),
                vld = load_in_queue(j),
                age = issue_age(j)
            )).reduceTree((a, b) => Mux(a.vld, Mux(b.vld, Mux(ESltu(a.age, b.age), a, b), a), b))
            io.deq(i).bits.is_latest := select_item.idx_1h === select_latest.idx_1h
        }

        // make the selected instruction not exist
        iq(i).zipWithIndex.foreach{ case (e, j) =>
            when(select_item.idx_1h(j)){ 
                e.inst_exi := false.B 
                // e.item.prj_wk := false.B
                // e.item.prk_wk := false.B
            }
        }
    }
    /* replay */
    iq.foreach{case (qq) =>
        qq.foreach{case (e) =>
            when(e.item.prj_lpv.orR && io.rply_bus.replay){
                e.inst_exi := true.B
            }
            when(e.item.prk_lpv.orR && io.rply_bus.replay){
                e.inst_exi := true.B
            }
        }
    }

    var flst_insert_ptr     = 1.U(n.W)
    val port_map_flst       = VecInit.fill(dw)(0.U(dw.W))
    val port_map_trans_flst = Transpose(port_map_flst)
    val ready_to_recycle    = iq.map{ case (qq) => qq.map{ case (e) => e.item.valid && !(e.inst_exi || e.item.prj_lpv.orR || e.item.prk_lpv.orR) } }
    // val select_recycle_idx  = ready_to_recycle.map{ case (qq) => VecInit(PriorityEncoderOH(qq)).asUInt }
    val select_recycle_idx  = ready_to_recycle.map{ case (qq) => VecInit(Log2OH(qq)).asUInt}
    for(i <- 0 until dw) {
        port_map_flst(i) := Mux(select_recycle_idx(i).orR, flst_insert_ptr, 0.U)
        flst_insert_ptr = Mux(select_recycle_idx(i).orR, ShiftAdd1(flst_insert_ptr), flst_insert_ptr)
    }
    flst.io.enq.zipWithIndex.foreach{ case (e, i) =>
        e.valid := port_map_trans_flst(i).orR
        e.bits  := Mux1H(port_map_trans_flst(i), VecInit.tabulate(dw)(j => j.U(log2Ceil(dw).W))) ## OHToUInt(Mux1H(port_map_trans_flst(i), select_recycle_idx)).take(log2Ceil(len))
    }
    iq.zipWithIndex.foreach{ case (qq, i) =>
        qq.zipWithIndex.foreach{ case (e, j) =>
            when(select_recycle_idx(i)(j)){
                e.item.valid := false.B
            }
        }
    }
    io.enq.foreach(_.ready := flst.io.deq.map(_.valid).reduce(_ && _))
    /* flush */
    when(io.flush){
        iq.foreach{case (qq) =>
            qq.foreach{case (e) =>
                e.inst_exi := false.B
            }
        }
    }
}