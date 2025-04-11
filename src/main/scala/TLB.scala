import chisel3._
import chisel3.util._
import Exception._
import ZirconConfig.TLB._
import ZirconUtil._

class TLBEntryT extends Bundle {
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
    def apply(vppn: UInt, ps: UInt, g: Bool, asid: UInt, e: Bool, ppn0: UInt, plv0: UInt, mat0: UInt, d0: Bool, v0: Bool, ppn1: UInt, plv1: UInt, mat1: UInt, d1: Bool, v1: Bool): TLBEntryT = {
        val r = Wire(new TLBEntryT)
        r.vppn := vppn; r.ps := ps; r.g := g; r.asid := asid; r.e := e;  
        r.ppn0 := ppn0; r.plv0 := plv0; r.mat0 := mat0; r.d0 := d0; r.v0 := v0
        r.ppn1 := ppn1; r.plv1 := plv1; r.mat1 := mat1; r.d1 := d1; r.v1 := v1
        r
    }
    
}
class TLBMemT extends TLBEntryT {
    val hitValid   =     Bool() // previous caculate hit check result
    def apply(tlbEntry: TLBEntryT, asid: UInt): TLBMemT = {
        val r = Wire(new TLBMemT)
        InheritFields(r, tlbEntry)
        r.hitValid := tlbEntry.e && (tlbEntry.g || tlbEntry.asid === asid)
        r
    }
    def processUpdate(asid: UInt) = {
        this.hitValid := this.e && (this.g || this.asid === asid)
    }
}
class TLBHitT extends Bundle {
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

    def apply(entry: TLBMemT, last: Bool): TLBHitT = {
        val r = Wire(new TLBHitT)
        r.vppn := entry.vppn; r.ps := entry.ps; r.g := entry.g; r.asid := entry.asid; r.e := entry.e;  
        r.ppn := Mux(last, entry.ppn1, entry.ppn0)
        r.plv := Mux(last, entry.plv1, entry.plv0)
        r.mat := Mux(last, entry.mat1, entry.mat0)
        r.d   := Mux(last, entry.d1, entry.d0)
        r.v   := Mux(last, entry.v1, entry.v0)
        r
    }
}

class TLBTransIO(isDtlb: Boolean) extends Bundle {
    val rvalid        = Input(Bool())
    val wvalid        = if(isDtlb) Some(Input(Bool())) else None
    val vaddr         = Input(UInt(32.W))
    val paddr         = Output(UInt(32.W))
    val uncache       = Output(Bool())
    val exception     = Output(UInt(8.W))
    val stall         = Input(Bool())
    val csrWe        = Input(Bool())
    val csr           = new Bundle{
        val asid = Input(UInt(10.W))
        val plv  = Input(UInt(2.W))
    }
}

class TLBRWFIO(isDtlb: Boolean) extends Bundle {
    val tlbwrEn       = Input(Bool())
    val tlbfillIdx    = Input(UInt(log2Ceil(ENTRYNUM).W))
    val tlbfillEn     = Input(Bool())

    val csr = new Bundle{
        val tlbidx = Input(UInt(log2Ceil(ENTRYNUM).W))
        val tlbrdEntry = if(isDtlb) Some(Output(new TLBEntryT)) else None
        val tlbwrEntry = Input(new TLBEntryT)
    }
}

class TLBINVIO extends Bundle {
    val invtlbEn      = Input(Bool())
    val invtlbOp      = Input(UInt(5.W))
    val invtlbAsid    = Input(UInt(10.W))
    val invtlbVaddr   = Input(UInt(32.W))
}

class TLBSrchIO(isDtlb: Boolean) extends Bundle {
    val csr = new Bundle{
        val tlbehi          = if(isDtlb) Some(Input(UInt(19.W))) else None
        val tlbsrchIdx     = if(isDtlb) Some(Output(UInt(log2Ceil(ENTRYNUM).W))) else None
        val tlbsrchHit     = if(isDtlb) Some(Output(Bool())) else None
    }
}

class TLBIO(isDtlb: Boolean) extends Bundle {
    // for tlbsrch
    val srch = new TLBSrchIO(isDtlb)
    // for tlbrd, write, fill
    val rwf = new TLBRWFIO(isDtlb)
    // for invtlb
    val inv = new TLBINVIO
    // for tlb trans
    val ptr = new TLBTransIO(isDtlb)
}


class TLB(isDtlb: Boolean) extends Module{
    val io = IO(new TLBIO(isDtlb))

    val tr = io.ptr
    val sr = io.srch
    val rwf = io.rwf
    val inv = io.inv

    val tlb = RegInit(VecInit.fill(ENTRYNUM)(0.U.asTypeOf(new TLBMemT)))

    // for tlbsrch
    if(isDtlb){
        val csrTlbehiVppn   = sr.csr.tlbehi.get
        val tlbsrchHit       = WireDefault(VecInit.fill(ENTRYNUM)(false.B))
        val tlbsrchHitIdx   = OHToUInt(tlbsrchHit)
        for(i <- 0 until ENTRYNUM){
            val tlbVppn = Mux(tlb(i).ps(3), tlb(i).vppn, tlb(i).vppn(18, 10) ## 0.U(10.W))
            val csrVppn = Mux(tlb(i).ps(3), csrTlbehiVppn, csrTlbehiVppn(18, 10) ## 0.U(10.W))
            tlbsrchHit(i) := tlb(i).hitValid && (tlbVppn === csrVppn)
        }
        io.srch.csr.tlbsrchIdx.get := tlbsrchHitIdx
        io.srch.csr.tlbsrchHit.get :=  tlbsrchHit.asUInt.orR
    }


    // for tlbrd
    if(isDtlb){
        io.rwf.csr.tlbrdEntry.get := tlb(rwf.csr.tlbidx).asInstanceOf[TLBEntryT]
    }
    // io.rwf.csr.tlbrdEntry := (if(isDtlb) tlb(rwf.csr.tlbidx).asInstanceOf[TLBEntryT] else 0.U.asTypeOf(new TLBEntryT))

    // for tlbwr and tlbfill
    when(rwf.tlbwrEn || rwf.tlbfillEn){
        val tlbIdx           = Mux(rwf.tlbwrEn, rwf.csr.tlbidx, rwf.tlbfillIdx)
        tlb(tlbIdx)          := (new TLBMemT)(rwf.csr.tlbwrEntry, tr.csr.asid)
    }

    // for invtlb
    val invtlbOp = inv.invtlbOp
    val invtlbAsid = inv.invtlbAsid
    val invtlbVaddr = inv.invtlbVaddr
    when(inv.invtlbEn){
        tlb.foreach{ case (entry) =>
            switch(invtlbOp){
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
                    // clear all tlb entries with asid = invtlbAsid and g = 0
                    when((entry.asid === invtlbAsid) && !entry.g){
                        entry.e := false.B
                    }
                }
                is(5.U){
                    // clear all tlb entries with asid = invtlbAsid and g = 0 and va[31:13] = invtlbVaddr[31:13]
                    when((entry.asid === invtlbAsid) && !entry.g && (entry.vppn === invtlbVaddr(31, 13))){
                        entry.e := false.B
                    }
                }
                is(6.U){
                    // clear all tlb entries with asid = invtlbAsid or g = 1,  and va[31:13] = invtlbVaddr[31:13]
                    when(((entry.asid === invtlbAsid) || entry.g) && (entry.vppn === invtlbVaddr(31, 13))){
                        entry.e := false.B
                    }
                }
            }
        }
    }

    def SignalException(tlbHit: Bool, tlbHitEntry: TLBHitT, csrPlv: UInt, rvalid: Bool, wvalid: Bool): UInt = {
        val exception = WireDefault(0.U(8.W))
        when(!tlbHit){
            exception := 1.U(1.W) ## TLBR
        }.elsewhen(!tlbHitEntry.v){
            if(isDtlb){
                exception := Mux(rvalid, 1.U(1.W) ## PIF, 0.U)
            }else{
                exception := Mux(rvalid, 1.U(1.W) ## PIL, Mux(wvalid, 1.U(1.W) ## PIS, 0.U))
            }
        }.elsewhen(csrPlv === 3.U && tlbHitEntry.plv === 0.U){
            exception := 1.U(1.W) ## PPI
        }.elsewhen(wvalid && !tlbHitEntry.d){
            exception := 1.U(1.W) ## PME
        }
        exception
    }
    // process change
    when(ShiftRegister(tr.csrWe, 1, false.B, true.B)){
        tlb.foreach(_.processUpdate(tr.csr.asid))
    }
    
    // tlb trans
    val tlbHit       = WireDefault(VecInit.fill(ENTRYNUM)(false.B))
    val tlbEntry = Mux1H(tlbHit, tlb)
    val tlbHitEntry = (new TLBHitT)(tlbEntry, Mux(tlbEntry.ps(3), tr.vaddr(12), tr.vaddr(21)))

    for(i <- 0 until ENTRYNUM){
        val tlbVppn    = Mux(tlb(i).ps(3), tlb(i).vppn, tlb(i).vppn(18, 10) ## 0.U(10.W))
        val dVppn      = Mux(tlb(i).ps(3), tr.vaddr(31, 13), tr.vaddr(31, 23) ## 0.U(10.W))
        tlbHit(i)      := tlb(i).hitValid && tlbVppn === dVppn
    }
    io.ptr.uncache      := tlbHitEntry.mat(0)
    io.ptr.paddr        := Mux(tlbHitEntry.ps(3), 
                           Cat(tlbHitEntry.ppn, tr.vaddr(11, 0)),
                           Cat(tlbHitEntry.ppn(19, 9), tr.vaddr(20, 0)))
    val tlbHitReg       = ShiftRegister(tlbHit, 1, VecInit.fill(ENTRYNUM)(false.B), !tr.stall)
    val tlbHitEntryReg = ShiftRegister(tlbHitEntry, 1, 0.U.asTypeOf(new TLBHitT), !tr.stall)
    val csrPlvReg       = ShiftRegister(tr.csr.plv, 1, 0.U, !tr.stall)
    val rvalidReg        = ShiftRegister(tr.rvalid, 1, false.B, !tr.stall)
    val wvalidReg        = if(isDtlb) ShiftRegister(tr.wvalid.get, 1, false.B, !tr.stall) else false.B
    val paddrReg         = ShiftRegister(tr.paddr, 1, 0.U, !tr.stall)
    io.ptr.exception      := (if(isDtlb) Mux(csrPlvReg === 3.U && paddrReg(31), 1.U(1.W) ## ADEM, SignalException(tlbHitReg.asUInt.orR, tlbHitEntryReg, csrPlvReg, rvalidReg, wvalidReg))
                              else SignalException(tlbHitReg.asUInt.orR, tlbHitEntryReg, csrPlvReg, rvalidReg, wvalidReg))
}