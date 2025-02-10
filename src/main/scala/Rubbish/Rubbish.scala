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
//         deq_ptr.foreach{ ptr => ptr := VecInit.tabulate(n)(i => shift_add_n(ptr, i))(counter)}
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
//         enq_ptr.foreach{ ptr => ptr := VecInit.tabulate(n)(i => shift_sub_n(ptr, i))(counter)}
//     }

//     flst.foreach(_.flush := io.flush)
// }
// object IQ_FList{
//     def apply(ew: Int, dw: Int, num: Int): IQ_FList = Module(new IQ_FList(ew, dw, num))
// }