import chisel3._
import chisel3.util._

class Direct_Trans_IO extends Bundle {
    val csr = new Bundle{
        val crmd_trans  = Input(UInt(6.W))
        val dmw0        = Input(UInt(32.W))
        val dmw1        = Input(UInt(32.W))
    }

}

class MMU_IO(is_dtlb: Boolean) extends TLB_IO(is_dtlb) {
    val dtr = new Direct_Trans_IO
}

class MMU(is_dtlb: Boolean) extends Module {
    val io = IO(new MMU_IO(is_dtlb))

    val tlb = Module(new TLB(is_dtlb))
    tlb.io.srch <> io.srch
    tlb.io.inv  <> io.inv
    tlb.io.rwf  <> io.rwf
    tlb.io.ptr  <> io.ptr

    val ptr = io.ptr
    val dtr = io.dtr

    val is_da = dtr.csr.crmd_trans(0)
    val is_pg = dtr.csr.crmd_trans(1)

    // dmw visit
    val dmw0_plv_valid = RegInit(false.B)
    val dmw1_plv_valid = RegInit(false.B)
    when(ShiftRegister(ptr.csr_we, 1, false.B, true.B)){
        dmw0_plv_valid := dtr.csr.dmw0(3, 0)(ptr.csr.plv)
        dmw1_plv_valid := dtr.csr.dmw1(3, 0)(ptr.csr.plv)
    }
    val dmw0_hit = dtr.csr.dmw0(31, 29) === ptr.vaddr(31, 29) && dmw0_plv_valid
    val dmw1_hit = dtr.csr.dmw1(31, 29) === ptr.vaddr(31, 29) && dmw1_plv_valid

    // memory map
    io.ptr.paddr     := Mux(is_da, ptr.vaddr,
                        Mux(dmw0_hit, dtr.csr.dmw0(27, 25) ## ptr.vaddr(28, 0),
                        Mux(dmw1_hit, dtr.csr.dmw1(27, 25) ## ptr.vaddr(28, 0), tlb.io.ptr.paddr)))
    io.ptr.uncache    := Mux(is_da, !dtr.csr.crmd_trans(2),
                        Mux(dmw0_hit, !dtr.csr.dmw0(4),
                        Mux(dmw1_hit, !dtr.csr.dmw1(4), tlb.io.ptr.uncache)))
    io.ptr.exception  := Mux(ShiftRegister(is_da || dmw0_hit || dmw1_hit, 1, false.B, !ptr.stall), 0.U, tlb.io.ptr.exception)
}