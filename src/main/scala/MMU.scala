import chisel3._
import chisel3.util._

class DirectTransIO extends Bundle {
    val csr = new Bundle{
        val crmdTrans  = Input(UInt(6.W))
        val dmw0        = Input(UInt(32.W))
        val dmw1        = Input(UInt(32.W))
    }

}

class MMUIO(isDtlb: Boolean) extends TLBIO(isDtlb) {
    val dtr = new DirectTransIO
}

class MMU(isDtlb: Boolean) extends Module {
    val io = IO(new MMUIO(isDtlb))

    val tlb = Module(new TLB(isDtlb))
    tlb.io.srch <> io.srch
    tlb.io.inv  <> io.inv
    tlb.io.rwf  <> io.rwf
    tlb.io.ptr  <> io.ptr

    val ptr = io.ptr
    val dtr = io.dtr

    val isDa = dtr.csr.crmdTrans(0)
    val isPg = dtr.csr.crmdTrans(1)

    // dmw visit
    val dmw0PlvValid = RegInit(false.B)
    val dmw1PlvValid = RegInit(false.B)
    when(ShiftRegister(ptr.csrWe, 1, false.B, true.B)){
        dmw0PlvValid := dtr.csr.dmw0(3, 0)(ptr.csr.plv)
        dmw1PlvValid := dtr.csr.dmw1(3, 0)(ptr.csr.plv)
    }
    val dmw0Hit = dtr.csr.dmw0(31, 29) === ptr.vaddr(31, 29) && dmw0PlvValid
    val dmw1Hit = dtr.csr.dmw1(31, 29) === ptr.vaddr(31, 29) && dmw1PlvValid

    // memory map
    io.ptr.paddr     := Mux(isDa, ptr.vaddr,
                        Mux(dmw0Hit, dtr.csr.dmw0(27, 25) ## ptr.vaddr(28, 0),
                        Mux(dmw1Hit, dtr.csr.dmw1(27, 25) ## ptr.vaddr(28, 0), tlb.io.ptr.paddr)))
    io.ptr.uncache    := Mux(isDa, !dtr.csr.crmdTrans(2),
                        Mux(dmw0Hit, !dtr.csr.dmw0(4),
                        Mux(dmw1Hit, !dtr.csr.dmw1(4), tlb.io.ptr.uncache)))
    io.ptr.exception  := Mux(ShiftRegister(isDa || dmw0Hit || dmw1Hit, 1, false.B, !ptr.stall), 0.U, tlb.io.ptr.exception)
}