import chisel3._
import chisel3.util._
import CPU_Config.RegisterFile._
import CPU_Config.ReserveQueue._
import CPU_Config.Issue._
import Zircon_Util._

class Replay_Bus_Pkg extends Bundle {
    // val prd = UInt(wpreg.W)
    // val confirm = Bool()
    val replay = Bool()
}
class Wakeup_Bus_Pkg extends Bundle {
    val prd = UInt(wpreg.W)
    val lpv = UInt(3.W)
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

class IQ_Entry extends Bundle {
    val inst_vld    = Bool()
    val inst_exi    = Bool()
    val prj         = UInt(wpreg.W)
    val prj_wk      = Bool()
    val prk         = UInt(wpreg.W)
    val prk_wk      = Bool()
    val prd         = UInt(wpreg.W)
    val rd_vld      = Bool()
    val op          = UInt(5.W)

    // val pcq_idx     = UInt(wpcq.W)
    // val imq_idx     = UInt(wimq.W)
    val rob_idx     = UInt(wrob.W)
    // for oldest-first select
    val age         = UInt((wrob+1).W)
    // for inferred wakeup
    val prj_lpv     = UInt(2.W)
    val prk_lpv     = UInt(2.W)
    

    def lpv_update(dcache_miss: Bool) = {
        this.prj_lpv := this.prj_lpv << 1.U
        this.prk_lpv := this.prk_lpv << 1.U
    }
}

object IQ_Entry{
    def apply(inst_vld: Bool, inst_exi: Bool, prj: UInt, prj_wk: Bool, prk: UInt, prk_wk: Bool, prd: UInt, rd_vld: Bool, 
              op: UInt, rob_idx: UInt, age: UInt, prj_lpv: UInt, prk_lpv: UInt): IQ_Entry = {
        val ie = Wire(new IQ_Entry)
        ie.inst_vld := inst_vld; ie.inst_exi := inst_exi; ie.prj := prj; ie.prj_wk := prj_wk
        ie.prk := prk; ie.prk_wk := prk_wk; ie.prd := prd; ie.rd_vld := rd_vld; ie.op := op
        ie.rob_idx := rob_idx; ie.age := age; ie.prj_lpv := prj_lpv; ie.prk_lpv := prk_lpv
        ie
    }
    def apply(): IQ_Entry = {
        IQ_Entry(false.B, false.B, 0.U, false.B, 0.U, false.B, 0.U, false.B, EXE_Op.ADD, 0.U, 0.U, 0.U, 0.U)
    }
}


class Issue_Queue_IO(ew: Int, dw: Int) extends Bundle {
    val enq         = Vec(ew, Flipped(DecoupledIO(new IQ_Entry)))
    val deq         = Vec(dw, DecoupledIO(new IQ_Entry))
    // val wake_bus    = Input(Vec(wissue, UInt(wpreg.W)))
    val wake_bus    = Input(Vec(nissue, new Wakeup_Bus_Pkg))
    val rply_bus    = Input(new Replay_Bus_Pkg)
    val dcache_miss = Input(Bool())
    val flush       = Input(Bool())
}

class Issue_Queue(ew: Int, dw: Int, num: Int) extends Module {
    val io = IO(new Issue_Queue_IO(ew, dw))
    assert(ew >= dw, "enq width must be greater than or equal to deq width")
    assert(num % dw == 0, "Issue Queue length must be a multiple of deq width")
    assert(num % ew == 0, "Issue Queue length must be a multiple of enq width")

    val len = num / dw
    val n = dw

    val iq = RegInit(
        VecInit.fill(n)(VecInit.fill(len)(0.U.asTypeOf(new IQ_Entry)))
    )
    val flst = Module(new Cluster_Index_FIFO(new Cluster_Entry(log2Ceil(len), log2Ceil(n)), num, dw, ew, 0, 0))
    /* insert into iq */
    // allocate free item in iq
    flst.io.deq.zipWithIndex.foreach{ case (deq, i) => 
        deq.ready := io.enq(i).valid
    }
    val free_iq = flst.io.deq.map(_.bits.qidx)
    val free_item = flst.io.deq.map(_.bits.offset)
    // write to iq
    for (i <- 0 until ew){
        when(io.enq(i).valid && flst.io.deq(0).valid){
            iq(free_iq(i))(free_item(i)) := io.enq(i).bits
        }
    }
    flst.io.flush := io.flush

    /* wake up */
    iq.foreach{case (qq) =>
        qq.foreach{case (e) =>
            e.prj_wk := io.wake_bus.map(_.prd === e.prj).reduce(_ || _)
            e.prk_wk := io.wake_bus.map(_.prd === e.prk).reduce(_ || _)
            e.prj_lpv := Mux(e.prj_lpv.orR || e.prj === 0.U, e.prj_lpv << 1.U, Mux1H(io.wake_bus.map(_.prd === e.prj), io.wake_bus.map(_.lpv)))
            e.prk_lpv := Mux(e.prk_lpv.orR || e.prk === 0.U, e.prk_lpv << 1.U, Mux1H(io.wake_bus.map(_.prd === e.prk), io.wake_bus.map(_.lpv)))
        }
    }
    /* replay */
    iq.foreach{case (qq) =>
        qq.foreach{case (e) =>
            when(e.prj_lpv.orR && io.rply_bus.replay){
                e.inst_exi := true.B
                e.prj_wk := e.prj === 0.U
            }
            when(e.prk_lpv.orR && io.rply_bus.replay){
                e.inst_exi := true.B
                e.prk_wk := e.prk === 0.U
            }
        }
    }

    /* select dw items from iq */

    flst.io.enq.foreach(_.valid := false.B)
    flst.io.enq.foreach(_.bits := DontCare)
    for(i <- 0 until dw){
        // get the oldest valid existing instruction
        val issue_valid = iq(i).map{ case (e) => e.prj_wk && e.prk_wk && e.inst_vld && e.inst_exi}
        val issue_age = iq(i).map(_.age)
        val select_item = VecInit.tabulate(len)(j => Select_Item(
            idx_1h = (1 << j).U(len.W),
            vld = issue_valid(j),
            age = issue_age(j)
        )).reduceTree((a, b) => Mux(a.vld, Mux(b.vld, Mux(esltu(a.age, b.age), a, b), a), b))
        
        io.deq(i).valid := select_item.vld
        io.deq(i).bits := Mux1H(select_item.idx_1h, iq(i))
        // make the selected instruction not exist
        iq(i).zipWithIndex.foreach{ case (e, j) =>
            when(select_item.idx_1h(j)){ 
                e.inst_exi := false.B 
                // e.prj_wk := false.B
                // e.prk_wk := false.B
            }
        }
    }

    var flst_insert_ptr     = 1.U(n.W)
    val port_map_flst       = VecInit.fill(dw)(0.U(dw.W))
    val port_map_trav_flst  = VecInit.fill(dw)(0.U(dw.W))
    val ready_to_recycle    = iq.map{ case (qq) => qq.map{ case (e) => !e.inst_exi && !e.prj_lpv.orR && !e.prk_lpv.orR && e.inst_vld } }
    val select_recycle_idx  = ready_to_recycle.map{ case (qq) => VecInit(PriorityEncoderOH(qq)).asUInt }
    for(i <- 0 until dw) {
        port_map_flst(i) := Mux(select_recycle_idx(i).orR, flst_insert_ptr, 0.U)
        flst_insert_ptr = Mux(select_recycle_idx(i).orR, shift_add_1(flst_insert_ptr), flst_insert_ptr)
    }
    for(i <- 0 until dw) {
        port_map_trav_flst(i) := VecInit(port_map_flst.map(_(i))).asUInt
    }
    flst.io.enq.zipWithIndex.foreach{ case (e, i) =>
        e.valid := port_map_trav_flst(i).orR
        e.bits.offset := OHToUInt(Mux1H(port_map_trav_flst(i), select_recycle_idx))
        e.bits.qidx := Mux1H(port_map_trav_flst(i), VecInit.tabulate(dw)(j => j.U(log2Ceil(dw).W)))
    }
    io.enq.foreach(_.ready := DontCare)
}