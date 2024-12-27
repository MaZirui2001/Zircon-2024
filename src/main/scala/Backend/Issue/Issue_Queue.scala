import chisel3._
import chisel3.util._
import CPU_Config.RegisterFile._
import CPU_Config.ReserveQueue._
import CPU_Config.Issue._
import Zircon_Util._

class Cluster_Entry(n1: Int, n2: Int) extends Bundle {
    val qidx = UInt(n2.W)
    val offset = UInt(n1.W)
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
    val prj         = UInt(wpreg.W)
    val prj_wk      = Bool()
    val prk         = UInt(wpreg.W)
    val prk_wk      = Bool()
    val prd         = UInt(wpreg.W)
    val rd_vld      = Bool()
    val op          = ALU_BR_Op()

    val pcq_idx     = UInt(wpcq.W)
    val imq_idx     = UInt(wimq.W)
    val rob_idx     = UInt(wrob.W)

    val age         = UInt((wrob+1).W)
}

object IQ_Entry{
    def apply(inst_vld: Bool, prj: UInt, prj_wk: Bool, prk: UInt, prk_wk: Bool, prd: UInt, rd_vld: Bool, op: ALU_BR_Op.Type, pcq_idx: UInt, imq_idx: UInt, rob_idx: UInt, age: UInt): IQ_Entry = {
        val ie = Wire(new IQ_Entry)
        ie.inst_vld := inst_vld; ie.prj := prj; ie.prj_wk := prj_wk; ie.prk := prk; ie.prk_wk := prk_wk; ie.prd := prd; ie.rd_vld := rd_vld; ie.op := op
        ie
    }
    def apply(): IQ_Entry = {
        IQ_Entry(false.B, 0.U, false.B, 0.U, false.B, 0.U, false.B, ALU_BR_Op.ADD, 0.U, 0.U, 0.U, 0.U)
    }
}

class IQ_FList(ew: Int, dw: Int, num: Int) extends Module{
    assert(ew <= dw, "enq width must be smaller than or equal to deq width")
    val n = dw
    val len = num / n
    val io = IO(new Bundle{
        val enq     = Vec(ew, Flipped(DecoupledIO(new Cluster_Entry(log2Ceil(len), log2Ceil(n)))))
        val deq     = Vec(dw, DecoupledIO(new Cluster_Entry(log2Ceil(len), log2Ceil(n))))
        val flush   = Input(Bool())
    })
    
    val flst = VecInit.tabulate(n)(i =>
        Module(new FIFO(new Cluster_Entry(log2Ceil(len), log2Ceil(n)), len, false, true, (i << log2Ceil(len)))).io)

    // deq
    val deq_ptr = RegInit(VecInit.tabulate(dw)(i => (1 << i).U(n.W)))
    val deq_ptr_trn = VecInit.tabulate(n)(i => VecInit.tabulate(dw)(j => deq_ptr(j)(i)).asUInt)
    val all_deq_valid = flst.map(_.deq.valid).reduce(_ && _)
    io.deq.zipWithIndex.foreach{ case (deq, i) => 
        deq.valid := MuxOH(deq_ptr(i), flst.map(_.deq.valid)) && all_deq_valid
        deq.bits := MuxOH(deq_ptr(i), flst.map(_.deq.bits))
    }
    flst.zipWithIndex.foreach{ case (fifo, i) => 
        fifo.deq.ready := MuxOH(deq_ptr_trn(i), io.deq.map(_.ready))
    }
    when(io.flush){
        deq_ptr.zipWithIndex.foreach{ case (ptr, i) => ptr := (1 << i).U(n.W) }
    }.otherwise{
        val counter = PopCount(io.deq.map(_.valid).zip(io.deq.map(_.ready)).map{ case (v, r) => v && r}).take(log2Ceil(n))
        deq_ptr.foreach{ ptr => ptr := VecInit.tabulate(n)(i => shift_add_n(ptr, i))(counter)}
    }

    // enq
    val enq_ptr = RegInit(VecInit.tabulate(ew)(i => (1 << i).U(n.W)))
    flst.zipWithIndex.foreach{ case (fifo, i) => 
        fifo.enq.valid := MuxOH(enq_ptr(i), io.enq.map(_.valid))
        fifo.enq.bits := MuxOH(enq_ptr(i), io.enq.map(_.bits))
    }
    io.enq.foreach(_.ready := true.B)

    when(io.flush){
        enq_ptr.zipWithIndex.foreach{ case (ptr, i) => ptr := (1 << i).U(n.W) }
    }.otherwise{
        val counter = PopCount(io.enq.map(_.valid)).take(log2Ceil(n))
        enq_ptr.foreach{ ptr => ptr := VecInit.tabulate(n)(i => shift_sub_n(ptr, i))(counter)}
    }

    flst.foreach(_.flush := io.flush)
}
object IQ_FList{
    def apply(ew: Int, dw: Int, num: Int): IQ_FList = Module(new IQ_FList(ew, dw, num))
}
class Issue_Queue_IO(ew: Int, dw: Int) extends Bundle {
    val enq         = Vec(ew, Flipped(DecoupledIO(new IQ_Entry)))
    val deq         = Vec(dw, DecoupledIO(new IQ_Entry))
    val wake_bus    = Input(Vec(wissue, UInt(wpreg.W)))
    val flush       = Input(Bool())
}

class Issue_Queue(ew: Int, dw: Int, num: Int) extends Module {
    val io = IO(new Issue_Queue_IO(ew, dw))
    assert(ew >= dw, "enq width must be greater than or equal to deq width")
    assert(num % ew == 0, "Issue Queue length must be a multiple of enq width")

    val len = num / ew
    val n = ew

    val iq = RegInit(
        VecInit.fill(n)(VecInit.fill(len)(0.U.asTypeOf(new IQ_Entry)))
    )
    val flst = IQ_FList(dw, ew, num)

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
            e.prj_wk := io.wake_bus.map(_ === e.prd).reduce(_ || _)
            e.prk_wk := io.wake_bus.map(_ === e.prd).reduce(_ || _)
        }
    }

    /* select dw items from iq */
    var flst_insert_ptr = 1.U(n.W)
    flst.io.enq.foreach(_.valid := false.B)
    flst.io.enq.foreach(_.bits := DontCare)
    for(i <- 0 until dw){
        val issue_valid = iq(i).map{ case (e) => e.prj_wk && e.prk_wk && e.inst_vld}
        val issue_age = iq(i).map(_.age)
        val select_item = VecInit.tabulate(len)(j => Select_Item(
            idx_1h = (1 << j).U(len.W),
            vld = issue_valid(j),
            age = issue_age(j)
        )).reduceTree((a, b) => Mux(a.vld, Mux(b.vld, Mux(esltu(a.age, b.age), a, b), a), b))
        
        io.deq(i).valid := select_item.vld
        io.deq(i).bits := MuxOH(select_item.idx_1h, iq(i))
        flst.io.enq.zipWithIndex.foreach{ case (enq, j) => 
            when(flst_insert_ptr(j)){
                enq.valid := select_item.vld
                enq.bits.offset := OHToUInt(select_item.idx_1h)
                enq.bits.qidx := i.U
            }
        }
        flst_insert_ptr = Mux(select_item.vld, shift_add_1(flst_insert_ptr), flst_insert_ptr)
    }
    io.enq.foreach(_.ready := flst.io.deq.map(_.valid).reduce(_ && _))
}