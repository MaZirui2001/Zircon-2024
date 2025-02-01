import chisel3._
import chisel3.util._
import Exception._
import CPU_Config.TLB._
import Zircon_Util._

class TLB_Entry_T extends Bundle {
    val vppn        = UInt(19.W)
    val ps          = UInt(6.W)
    val g           = Bool()
    val asid        = UInt(10.W)
    val e           = Bool()
    val ppn0        = UInt(20.W)
    val plv0        = UInt(2.W)
    val mat0        = UInt(2.W)
    val d0          = Bool()
    val v0          = Bool()
    val ppn1        = UInt(20.W)
    val plv1        = UInt(2.W)
    val mat1        = UInt(2.W)
    val d1          = Bool()
    val v1          = Bool()
    def apply(vppn: UInt, ps: UInt, g: Bool, asid: UInt, e: Bool, ppn0: UInt, plv0: UInt, mat0: UInt, d0: Bool, v0: Bool, ppn1: UInt, plv1: UInt, mat1: UInt, d1: Bool, v1: Bool): TLB_Entry_T = {
        val r = Wire(new TLB_Entry_T)
        r.vppn := vppn; r.ps := ps; r.g := g; r.asid := asid; r.e := e;  
        r.ppn0 := ppn0; r.plv0 := plv0; r.mat0 := mat0; r.d0 := d0; r.v0 := v0
        r.ppn1 := ppn1; r.plv1 := plv1; r.mat1 := mat1; r.d1 := d1; r.v1 := v1
        r
    }
    
}
class TLB_Mem_T extends TLB_Entry_T {
    val hit_valid   =     Bool() // previous caculate hit check result
    def apply(tlb_entry: TLB_Entry_T, asid: UInt): TLB_Mem_T = {
        val r = Wire(new TLB_Mem_T)
        inheritFields(r, tlb_entry)
        r.hit_valid := tlb_entry.e && (tlb_entry.g || tlb_entry.asid === asid)
        r
    }
    def process_update(asid: UInt) = {
        this.hit_valid := this.e && (this.g || this.asid === asid)
    }
}
class TLB_Hit_T extends Bundle {
    val vppn = UInt(19.W)
    val ps   = UInt(6.W)
    val g    = Bool()
    val asid = UInt(10.W)
    val e    = Bool()
    val ppn  = UInt(20.W)
    val plv  = UInt(2.W)
    val mat  = UInt(2.W)
    val d    = Bool()
    val v    = Bool()

    def apply(entry: TLB_Mem_T, last: Bool): TLB_Hit_T = {
        val r = Wire(new TLB_Hit_T)
        r.vppn := entry.vppn; r.ps := entry.ps; r.g := entry.g; r.asid := entry.asid; r.e := entry.e;  
        r.ppn := Mux(last, entry.ppn1, entry.ppn0)
        r.plv := Mux(last, entry.plv1, entry.plv0)
        r.mat := Mux(last, entry.mat1, entry.mat0)
        r.d   := Mux(last, entry.d1, entry.d0)
        r.v   := Mux(last, entry.v1, entry.v0)
        r
    }
}

class TLB_Trans_IO(is_dtlb: Boolean) extends Bundle {
    val rvalid        = Input(Bool())
    val wvalid        = if(is_dtlb) Some(Input(Bool())) else None
    val vaddr         = Input(UInt(32.W))
    val paddr         = Output(UInt(32.W))
    val uncache       = Output(Bool())
    val exception     = Output(UInt(8.W))
    val stall         = Input(Bool())
    val csr_we        = Input(Bool())
    val csr           = new Bundle{
        val asid = Input(UInt(10.W))
        val plv  = Input(UInt(2.W))
    }
}

class TLB_RWF_IO(is_dtlb: Boolean) extends Bundle {
    val tlbwr_en       = Input(Bool())
    val tlbfill_idx    = Input(UInt(log2Ceil(ENTRY_NUM).W))
    val tlbfill_en     = Input(Bool())

    val csr = new Bundle{
        val tlbidx = Input(UInt(log2Ceil(ENTRY_NUM).W))
        val tlbrd_entry = if(is_dtlb) Some(Output(new TLB_Entry_T)) else None
        val tlbwr_entry = Input(new TLB_Entry_T)
    }
}

class TLB_INV_IO extends Bundle {
    val invtlb_en      = Input(Bool())
    val invtlb_op      = Input(UInt(5.W))
    val invtlb_asid    = Input(UInt(10.W))
    val invtlb_vaddr   = Input(UInt(32.W))
}

class TLB_Srch_IO(is_dtlb: Boolean) extends Bundle {
    val csr = new Bundle{
        val tlbehi          = if(is_dtlb) Some(Input(UInt(19.W))) else None
        val tlbsrch_idx     = if(is_dtlb) Some(Output(UInt(log2Ceil(ENTRY_NUM).W))) else None
        val tlbsrch_hit     = if(is_dtlb) Some(Output(Bool())) else None
    }
}

class TLB_IO(is_dtlb: Boolean) extends Bundle {
    // for tlbsrch
    val srch = new TLB_Srch_IO(is_dtlb)
    // for tlbrd, write, fill
    val rwf = new TLB_RWF_IO(is_dtlb)
    // for invtlb
    val inv = new TLB_INV_IO
    // for tlb trans
    val ptr = new TLB_Trans_IO(is_dtlb)
}


class TLB(is_dtlb: Boolean) extends Module{
    val io = IO(new TLB_IO(is_dtlb))

    val tr = io.ptr
    val sr = io.srch
    val rwf = io.rwf
    val inv = io.inv

    val tlb = RegInit(VecInit.fill(ENTRY_NUM)(0.U.asTypeOf(new TLB_Mem_T)))

    // for tlbsrch
    if(is_dtlb){
        val csr_tlbehi_vppn   = sr.csr.tlbehi.get
        val tlbsrch_hit       = WireDefault(VecInit.fill(ENTRY_NUM)(false.B))
        val tlbsrch_hit_idx   = OHToUInt(tlbsrch_hit)
        for(i <- 0 until ENTRY_NUM){
            val tlb_vppn = Mux(tlb(i).ps(3), tlb(i).vppn, tlb(i).vppn(18, 10) ## 0.U(10.W))
            val csr_vppn = Mux(tlb(i).ps(3), csr_tlbehi_vppn, csr_tlbehi_vppn(18, 10) ## 0.U(10.W))
            tlbsrch_hit(i) := tlb(i).hit_valid && (tlb_vppn === csr_vppn)
        }
        io.srch.csr.tlbsrch_idx.get := tlbsrch_hit_idx
        io.srch.csr.tlbsrch_hit.get :=  tlbsrch_hit.asUInt.orR
    }


    // for tlbrd
    if(is_dtlb){
        io.rwf.csr.tlbrd_entry.get := tlb(rwf.csr.tlbidx).asInstanceOf[TLB_Entry_T]
    }
    // io.rwf.csr.tlbrd_entry := (if(is_dtlb) tlb(rwf.csr.tlbidx).asInstanceOf[TLB_Entry_T] else 0.U.asTypeOf(new TLB_Entry_T))

    // for tlbwr and tlbfill
    when(rwf.tlbwr_en || rwf.tlbfill_en){
        val tlb_idx           = Mux(rwf.tlbwr_en, rwf.csr.tlbidx, rwf.tlbfill_idx)
        tlb(tlb_idx)          := (new TLB_Mem_T)(rwf.csr.tlbwr_entry, tr.csr.asid)
    }

    // for invtlb
    val invtlb_op = inv.invtlb_op
    val invtlb_asid = inv.invtlb_asid
    val invtlb_vaddr = inv.invtlb_vaddr
    when(inv.invtlb_en){
        tlb.foreach{ case (entry) =>
            switch(invtlb_op){
                is(0.U){
                    // clear all tlb entries
                    entry.e := false.B
                }
                is(1.U){
                    // clear all tlb entries
                    entry.e := false.B
                }
                is(2.U){
                    // clear all tlb entries with g = 1
                    when(entry.g){
                        entry.e := false.B
                    }
                }
                is(3.U){
                    // clear all tlb entries with g = 0
                    when(!entry.g){
                        entry.e := false.B
                    }
                }
                is(4.U){
                    // clear all tlb entries with asid = invtlb_asid and g = 0
                    when((entry.asid === invtlb_asid) && !entry.g){
                        entry.e := false.B
                    }
                }
                is(5.U){
                    // clear all tlb entries with asid = invtlb_asid and g = 0 and va[31:13] = invtlb_vaddr[31:13]
                    when((entry.asid === invtlb_asid) && !entry.g && (entry.vppn === invtlb_vaddr(31, 13))){
                        entry.e := false.B
                    }
                }
                is(6.U){
                    // clear all tlb entries with asid = invtlb_asid or g = 1,  and va[31:13] = invtlb_vaddr[31:13]
                    when(((entry.asid === invtlb_asid) || entry.g) && (entry.vppn === invtlb_vaddr(31, 13))){
                        entry.e := false.B
                    }
                }
            }
        }
    }

    def Signal_Exception(tlb_hit: Bool, tlb_hit_entry: TLB_Hit_T, csr_plv: UInt, rvalid: Bool, wvalid: Bool): UInt = {
        val exception = WireDefault(0.U(8.W))
        when(!tlb_hit){
            exception := 1.U(1.W) ## TLBR
        }.elsewhen(!tlb_hit_entry.v){
            if(is_dtlb){
                exception := Mux(rvalid, 1.U(1.W) ## PIF, 0.U)
            }else{
                exception := Mux(rvalid, 1.U(1.W) ## PIL, Mux(wvalid, 1.U(1.W) ## PIS, 0.U))
            }
        }.elsewhen(csr_plv === 3.U && tlb_hit_entry.plv === 0.U){
            exception := 1.U(1.W) ## PPI
        }.elsewhen(wvalid && !tlb_hit_entry.d){
            exception := 1.U(1.W) ## PME
        }
        exception
    }
    // process change
    when(ShiftRegister(tr.csr_we, 1, false.B, true.B)){
        tlb.foreach(_.process_update(tr.csr.asid))
    }
    
    // tlb trans
    val tlb_hit       = WireDefault(VecInit.fill(ENTRY_NUM)(false.B))
    val tlb_entry = Mux1H(tlb_hit, tlb)
    val tlb_hit_entry = (new TLB_Hit_T)(tlb_entry, Mux(tlb_entry.ps(3), tr.vaddr(12), tr.vaddr(21)))

    for(i <- 0 until ENTRY_NUM){
        val tlb_vppn    = Mux(tlb(i).ps(3), tlb(i).vppn, tlb(i).vppn(18, 10) ## 0.U(10.W))
        val d_vppn      = Mux(tlb(i).ps(3), tr.vaddr(31, 13), tr.vaddr(31, 23) ## 0.U(10.W))
        tlb_hit(i)      := tlb(i).hit_valid && tlb_vppn === d_vppn
    }
    io.ptr.uncache      := tlb_hit_entry.mat(0)
    io.ptr.paddr        := Mux(tlb_hit_entry.ps(3), 
                           Cat(tlb_hit_entry.ppn, tr.vaddr(11, 0)),
                           Cat(tlb_hit_entry.ppn(19, 9), tr.vaddr(20, 0)))
    val tlb_hit_reg       = ShiftRegister(tlb_hit, 1, VecInit.fill(ENTRY_NUM)(false.B), !tr.stall)
    val tlb_hit_entry_reg = ShiftRegister(tlb_hit_entry, 1, 0.U.asTypeOf(new TLB_Hit_T), !tr.stall)
    val csr_plv_reg       = ShiftRegister(tr.csr.plv, 1, 0.U, !tr.stall)
    val rvalid_reg        = ShiftRegister(tr.rvalid, 1, false.B, !tr.stall)
    val wvalid_reg        = if(is_dtlb) ShiftRegister(tr.wvalid.get, 1, false.B, !tr.stall) else false.B
    val paddr_reg         = ShiftRegister(tr.paddr, 1, 0.U, !tr.stall)
    io.ptr.exception      := (if(is_dtlb) Mux(csr_plv_reg === 3.U && paddr_reg(31), 1.U(1.W) ## ADEM, Signal_Exception(tlb_hit_reg.asUInt.orR, tlb_hit_entry_reg, csr_plv_reg, rvalid_reg, wvalid_reg))
                              else Signal_Exception(tlb_hit_reg.asUInt.orR, tlb_hit_entry_reg, csr_plv_reg, rvalid_reg, wvalid_reg))
}