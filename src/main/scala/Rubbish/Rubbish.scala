// class IQ_FList(ew: Int, dw: Int, num: Int) extends Module{
//     assert(ew <= dw, "enq width must be smaller than or equal to deq width")
//     val n = dw
//     val len = num / n
//     val io = IO(new Bundle{
//         val enq     = Vec(ew, Flipped(DecoupledIO(new Flist_Entry(log2Ceil(len), log2Ceil(n)))))
//         val deq     = Vec(dw, DecoupledIO(new Flist_Entry(log2Ceil(len), log2Ceil(n))))
//         val flush   = Input(Bool())
//     })
    
//     val flst = VecInit.tabulate(n)(i =>
//         Module(new FIFO(new Flist_Entry(log2Ceil(len), log2Ceil(n)), len, false, true, (i << log2Ceil(len)))).io)

//     // deq
//     val deq_ptr = RegInit(VecInit.tabulate(dw)(i => (1 << i).U(n.W)))
//     val deq_ptr_trn = VecInit.tabulate(n)(i => VecInit.tabulate(dw)(j => deq_ptr(j)(i)).asUInt)
//     val all_deq_valid = flst.map(_.deq.valid).reduce(_ && _)
//     io.deq.zipWithIndex.foreach{ case (deq, i) => 
//         deq.valid := Mux1H(deq_ptr(i), flst.map(_.deq.valid)) && all_deq_valid
//         deq.bits := Mux1H(deq_ptr(i), flst.map(_.deq.bits))
//     }
//     flst.zipWithIndex.foreach{ case (fifo, i) => 
//         fifo.deq.ready := Mux1H(deq_ptr_trn(i), io.deq.map(_.ready))
//     }
//     when(io.flush){
//         deq_ptr.zipWithIndex.foreach{ case (ptr, i) => ptr := (1 << i).U(n.W) }
//     }.otherwise{
//         val counter = PopCount(io.deq.map(_.valid).zip(io.deq.map(_.ready)).map{ case (v, r) => v && r}).take(log2Ceil(n))
//         deq_ptr.foreach{ ptr => ptr := VecInit.tabulate(n)(i => ShiftAddN(ptr, i))(counter)}
//     }

//     // enq
//     val enq_ptr = RegInit(VecInit.tabulate(ew)(i => (1 << i).U(n.W)))
//     val enq_ptr_trn = VecInit.tabulate(n)(i => VecInit.tabulate(ew)(j => enq_ptr(j)(i)).asUInt)
//     flst.zipWithIndex.foreach{ case (fifo, i) => 
//         fifo.enq.valid := Mux1H(enq_ptr(i), io.enq.map(_.valid))
//         fifo.enq.bits := Mux1H(enq_ptr(i), io.enq.map(_.bits))
//     }
//     io.enq.foreach(_.ready := true.B)

//     when(io.flush){
//         enq_ptr.zipWithIndex.foreach{ case (ptr, i) => ptr := (1 << i).U(n.W) }
//     }.otherwise{
//         val counter = PopCount(io.enq.map(_.valid)).take(log2Ceil(n))
//         enq_ptr.foreach{ ptr => ptr := VecInit.tabulate(n)(i => ShiftSubN(ptr, i))(counter)}
//     }

//     flst.foreach(_.flush := io.flush)
// }
// object IQ_FList{
//     def apply(ew: Int, dw: Int, num: Int): IQ_FList = Module(new IQ_FList(ew, dw, num))
// }



// import chisel3._
// import chisel3.util._
// import Zircon_Config.RegisterFile._
// import Zircon_Config.Decode._
// import Zircon_Config.Commit._
// import Zircon_Config.Issue._

// class ROB_Frontend_Entry extends Bundle{
//     val rd_vld      = Bool()
//     val rd          = UInt(wlreg.W)
//     val prd         = UInt(wpreg.W)
//     val pprd        = UInt(wpreg.W)
//     val pc          = UInt(32.W)
//     val pred_type   = UInt(2.W)
//     val is_store    = Bool()
//     val is_priv     = Bool()
// }

// class ROB_Backend_Entry extends Bundle{
//     val complete    = Bool()
//     val jump_en     = Bool()
//     val pred_fail   = Bool()
//     val exception   = UInt(8.W)
//     val result      = UInt(32.W)
//     val nxt_cmt_en  = Bool()
// }

// class ROB_Entry extends Bundle{
//     val fte = new ROB_Frontend_Entry
//     val bke = new ROB_Backend_Entry
    
//     def apply(fte: ROB_Frontend_Entry, bke: ROB_Backend_Entry): ROB_Entry = {
//         val entry = Wire(new ROB_Entry)
//         entry.fte := fte
//         entry.bke := bke
//         entry
//     }
// }

// class ROB_Frontend_IO extends Bundle{
//     val enq     = Vec(ndcd, Flipped(Decoupled(new ROB_Frontend_Entry)))
//     val enq_idx = Output(Vec(ndcd, new Cluster_Entry(nrob_q, ndcd)))
// }

// class ROB_Backend_IO extends Bundle{
//     val widx    = Input(Vec(nis, new Cluster_Entry(nrob_q, ndcd)))
//     val wen     = Input(Vec(nis, Bool()))
//     val wdata   = Input(Vec(nis, new ROB_Backend_Entry))
// }

// class ROB_Commit_IO extends Bundle{
//     val deq     = Vec(ncommit, Decoupled(new ROB_Entry))
//     // val deq_idx = Output(Vec(ncommit, new Cluster_Entry(nrob_q, ncommit)))
//     val flush   = Input(Bool())
// }

// class Reorder_Buffer_IO extends Bundle{
//     val fte = new ROB_Frontend_IO
//     val bke = new ROB_Backend_IO
//     val cmt = new ROB_Commit_IO
// }


// class Reorder_Buffer extends Module{
//     val io = IO(new Reorder_Buffer_IO)

//     val q = Module(new Cluster_Index_FIFO(new ROB_Entry, nrob, ndcd, ncommit, 0, nis))

//     // 1. frontend: in dispatch stage, each instruction will enqueue into the ROB
//     q.io.enq.zip(io.fte.enq).foreach{case (enq, fte) =>
//         enq.bits.fte := fte.bits
//         enq.bits.bke := DontCare
//         enq.bits.bke.complete := false.B
//         enq.valid := fte.valid
//         fte.ready := enq.ready
//     }
//     io.fte.enq_idx := q.io.enq_idx
//     // 2. backend: in writeback stage, some instruction will write some data into the ROB
//     q.io.wdata.zip(io.bke.wdata).foreach{case (wdata, bke) =>
//         wdata.fte := DontCare
//         wdata.bke := bke
//     }
//     q.io.widx := io.bke.widx
//     q.io.wen := io.bke.wen
//     // 3. commit: in commit stage, some instruction will be committed
//     q.io.flush := false.B
//     io.cmt := DontCare
//     for(i <- 0 until ncommit){
//         io.cmt.deq(i).bits := q.io.deq(i).bits
//         io.cmt.deq(i).valid := (
//             if(i == 0) q.io.deq(i).bits.bke.complete
//             else q.io.deq(i).bits.bke.complete 
//               && q.io.deq.take(i).map(_.bits.bke.complete).reduce(_ && _) 
//               && io.cmt.deq(i-1).bits.bke.nxt_cmt_en
//         )
//         q.io.deq(i).ready := io.cmt.deq(i).valid
//     }
// }



// import chisel3._
// import chisel3.util._
// import Zircon_Config.RegisterFile._
// import Zircon_Util._

// class SRat_RAM(rn_w: Int) extends Module {
//     val io = IO(new Bundle {
//         val addr  = Input(Vec(3 * rn_w, UInt(nlreg.W)))
//         val rdata = Output(Vec(3 * rn_w, UInt(wpreg.W)))
//         val wdata = Input(Vec(3 * rn_w, UInt(wpreg.W)))
//         val wen   = Input(Vec(3 * rn_w, Bool()))
//         val dbg   = Output(Vec(nlreg, UInt(wpreg.W)))
//     })
//     val ram = RegInit(VecInit.tabulate(nlreg)(i => 0.U(wpreg.W)))
//     for(i <- 0 until 3 * rn_w){
//         when(io.wen(i)){
//             for(j <- 0 until nlreg){
//                 when(io.addr(i)(j)){
//                     ram(j) := io.wdata(i)
//                 }
//             }
//         }
//         // io.rdata(i) := VecInit(ram.zip(io.addr(i).asBools).zipWithIndex.map({case ((r, a), j) => 
//         //     r & Fill(npreg, a)
//         // })).reduceTree(_ | _)
//         io.rdata(i) := Mux1H(io.addr(i), ram)
//     }
//     io.dbg := ram
// }

// class SRat_IO(rn_w: Int) extends Bundle{
//     val rj          = Input(Vec(rn_w, UInt(wlreg.W)))
//     val rk          = Input(Vec(rn_w, UInt(wlreg.W)))
//     val rd          = Input(Vec(rn_w, UInt(wlreg.W)))
//     val rd_vld      = Input(Vec(rn_w, Bool()))
//     val prd         = Input(Vec(rn_w, UInt(wpreg.W)))
//     val prj         = Output(Vec(rn_w, UInt(wpreg.W)))
//     val prk         = Output(Vec(rn_w, UInt(wpreg.W)))
//     val pprd        = Output(Vec(rn_w, UInt(wpreg.W)))

//     val rdrct_en    = Input(Bool())
//     val arat        = Input(Vec(nlreg, UInt(wpreg.W)))
//     val srat_rdy    = Output(Bool())

//     val dbg_srat    = Output(Vec(nlreg, UInt(wpreg.W)))
// }


// class SRat(rn_w: Int) extends Module {
//     val io  = IO(new SRat_IO(rn_w))
//     val rat = Module(new SRat_RAM(rn_w))

//     // rw_w: Read/Write Width
//     val rw_w                = 3 * rn_w
//     val rdrct_cnt_max       = ((31 + rw_w) / rw_w)
//     val rdrct_cnt           = RegInit(Fill(log2Ceil(rdrct_cnt_max)+1, true.B).asUInt)
//     val free                = rdrct_cnt(log2Ceil(rdrct_cnt_max))
//     val rdrct_wr_idx_1h     = RegInit(VecInit.tabulate(rw_w)(i => (1 << i).U(32.W)))
//     val rdrct_wdata_init    = MixedVecInit(Seq.tabulate(rw_w)(i => 
//                               VecInit.tabulate((31+rw_w-i)/rw_w)(j => io.arat(j * rw_w + i))))
//     val rdrct_wdata         = RegInit(rdrct_wdata_init)

//     when(!rdrct_cnt(log2Ceil(rdrct_cnt_max))){
//         rdrct_cnt := rdrct_cnt - 1.U
//         rdrct_wr_idx_1h.foreach{ r => r := r.take(r.getWidth-rw_w) ## 0.U(rw_w.W)}
//         rdrct_wdata.foreach{ r => r := (r(r.length-1).asUInt ## VecInit(r.drop(1)).asUInt).asTypeOf(r)}
//     }.elsewhen(io.rdrct_en){
//         rdrct_cnt := (rdrct_cnt_max - 1).U
//         rdrct_wr_idx_1h := VecInit.tabulate(rw_w)(i => (1 << i).U(32.W))
//         rdrct_wdata := rdrct_wdata_init
//     }

//     for(i <- 0 until 3 * rn_w){
//         // rj
//         if (i < rn_w){
//             rat.io.addr(i) := Mux(free, UIntToOH(io.rj(i)), rdrct_wr_idx_1h(i))
//             rat.io.wen(i) := !free && rdrct_wr_idx_1h(i).orR
//             rat.io.wdata(i) := rdrct_wdata(i)(0)
//             io.prj(i) := rat.io.rdata(i)
//         }
//         // rk
//         else if (i < 2 * rn_w){
//             rat.io.addr(i) := Mux(free, UIntToOH(io.rk(i % rn_w)), rdrct_wr_idx_1h(i))
//             rat.io.wen(i) := !free && rdrct_wr_idx_1h(i).orR
//             rat.io.wdata(i) := rdrct_wdata(i)(0)
//             io.prk(i % rn_w) := rat.io.rdata(i)
//         }
//         // rd
//         else{
//             rat.io.addr(i) := Mux(free, UIntToOH(io.rd(i % rn_w)), rdrct_wr_idx_1h(i))
//             rat.io.wen(i) := Mux(free, io.rd_vld(i % rn_w), rdrct_wr_idx_1h(i).orR)
//             rat.io.wdata(i) := Mux(free, io.prd(i % rn_w), rdrct_wdata(i)(0))
//             io.pprd(i % rn_w) := rat.io.rdata(i)
//         }
//     }
//     io.srat_rdy := free
//     io.dbg_srat := rat.io.dbg
// }