import chisel3._
import chisel3.util._
import Exception._
import CSRNAME._

import ZirconConfig._
import Exception._

class CSRRWIO extends Bundle{
    val raddr   = Input(UInt(14.W))
    val waddr   = Input(UInt(14.W))
    val we      = Input(Bool())
    val wdata   = Input(UInt(32.W))
    val rdata   = Output(UInt(32.W))
}

class CSRExceptionIO extends Bundle{
    val exception           = Input(UInt(8.W))
    val badvExp            = Input(UInt(32.W))
    val isEret             = Input(Bool())
    val pcExp              = Input(UInt(32.W))
    val eentry              = Output(UInt(32.W))
    val tlbreentry          = Output(UInt(32.W))
}

class CSRIntIO extends Bundle{
    val interrupt       = Input(UInt(8.W))
    val ipInt          = Input(Bool())
    val interruptVec   = Output(UInt(12.W))
}

class CSRMMUIO(isDtlb: Boolean) extends Bundle{
    val tlb = new Bundle{
        val srch        = Flipped((new TLBSrchIO(isDtlb)).csr)
        val dtr         = Flipped((new DirectTransIO).csr)
        val rwf         = Flipped((new TLBRWFIO(isDtlb)).csr)
        val ptr         = Flipped((new TLBTransIO(isDtlb)).csr)
    }
    val tlbsrchEn  = if(isDtlb) Some(Input(Bool())) else None
    val tlbrdEn    = if(isDtlb) Some(Input(Bool())) else None
}


class CSRLLBitIO extends Bundle{
    val llbitSet   = Input(Bool())
    val llbitClear = Input(Bool())
    val llbit = Output(Bool())
}

class CSRDebugIO extends Bundle{
    val estat_13 = Output(UInt(13.W))
}

class CSRIO extends Bundle{
    // read & write
    val rw = new CSRRWIO

    // exception and ertn
    val exp = new CSRExceptionIO

    // interrupt
    val int = new CSRIntIO

    // mmu
    val imu = new CSRMMUIO(false)
    val dmu = new CSRMMUIO(true)

    // llbit
    val llb = new CSRLLBitIO

    // debug
    val debug = new CSRDebugIO
}

class CSR(PALEN: 32, TIMERINITWIDTH: 30) extends Module{
    val TLBINDEXWIDTH = log2Ceil(TLB.ENTRYNUM)
    
    val io          = IO(new CSRIO)
    val we          = io.rw.we
    val waddr       = io.rw.waddr
    val raddr       = io.rw.raddr
    val wdata       = io.rw.wdata
    val exception   = io.exp.exception
    val isEret     = io.exp.isEret

    val timerIntReg = RegInit(false.B)
    val timerInt = timerIntReg

    // CRMD：当前模式信息
    // for immu and dmmu
    val crmds = RegInit(VecInit.fill(4)(8.U(32.W)))
    val prmd = RegInit(0.U(32.W))
    val estat = RegInit(0.U(32.W))
    val hasTlbr = estat(21, 16) === 0x3f.U
    val isTlbr = exception(6, 0) === 0x3f.U
    val vppnSave = isTlbr || exception(6, 0) >= PIL && exception(6, 0) <= PPI
    val badvSave = vppnSave || exception(5, 0) === 0x8.U || exception(6, 0) === ALE

    when(exception(7)){
        crmds.foreach{case(c) => c := c(31, 5) ## Mux(isTlbr, 1.U(2.W), c(4, 3))  ## 0.U(3.W)}
    }.elsewhen(isEret){
        crmds.foreach{case(c) => c := c(31, 5) ## Mux(hasTlbr, 2.U(2.W), c(4, 3)) ## prmd(2, 0)}
    }.elsewhen(we && waddr === CRMD){
        crmds.foreach{case(c) => c := 0.U(23.W) ## wdata(8, 0)}
    }
    io.imu.tlb.ptr.plv := crmds(0)(1, 0)
    io.dmu.tlb.ptr.plv := crmds(1)(1, 0)
    io.imu.tlb.dtr.crmdTrans := crmds(0)(8, 3)
    io.dmu.tlb.dtr.crmdTrans := crmds(1)(8, 3)
    
    // PRMD：例外前模式信息
    when(exception(7)){
        prmd := prmd(31, 3) ## crmds(2)(2, 0)
    }.elsewhen(we && waddr === PRMD){
        prmd := 0.U(29.W) ## wdata(2, 0)
    }

    // EUEN：扩展部件使能
    val euen = RegInit(0.U(32.W))
    when(we && waddr === EUEN){
        euen := 0.U(31.W) ## wdata(0)
    }

    // ECFG：例外控制
    val ecfg = RegInit(0.U(32.W))
    when(we && waddr === ECFG){
        ecfg := 0.U(19.W) ## wdata(12, 11) ## 0.U(1.W) ## wdata(9, 0)
    }

    // ESTAT：例外状态
    when(exception(7)){
        estat := estat(31) ## 0.U(8.W) ## exception(6, 0) ## estat(15, 0)
    }.elsewhen(we && waddr === ESTAT){
        estat := 0.U(1.W) ## estat(30, 16) ## 0.U(3.W) ## io.int.ipInt ## timerInt ## 0.U(1.W) ## io.int.interrupt ## wdata(1, 0)
    }.otherwise{
        estat := 0.U(1.W) ## estat(30, 16) ## 0.U(3.W) ## io.int.ipInt ## timerInt ## 0.U(1.W) ## io.int.interrupt ## estat(1, 0)
    }
    io.debug.estat_13 := estat(13, 0)

    // ERA：例外返回地址
    val era = RegInit(0.U(32.W))
    when(exception(7)){
        era := io.exp.pcExp
    }.elsewhen(we && waddr === ERA){
        era := wdata
    }

    // BADV：出错虚地址
    val badv = RegInit(0.U(32.W))
    // when(exception === 0x88.U(8.W)){
    //     badv := io.pcExp
    // }.elsewhen(exception === 0x89.U(8.W)){
    //     badv := io.badvExp
    when(exception(7) && badvSave){
        badv := io.exp.badvExp
    }.elsewhen(we && waddr === BADV){
        badv := wdata
    }

    // EENTRY：例外入口地址
    val eentry = RegInit(0.U(32.W))
    when(we && waddr === EENTRY){
        eentry := wdata(31, 6) ## 0.U(6.W)
    }

    // CPUID：处理器编号
    val cpuid = RegInit(0.U(32.W))
    when(we && waddr === CPUID){
        cpuid := 0.U(23.W) ## cpuid(8, 0)
    }

    // SAVE0：数据保存
    val save0 = RegInit(0.U(32.W))
    when(we && waddr === SAVE0){
        save0 := wdata
    }

    // SAVE1：数据保存
    val save1 = RegInit(0.U(32.W))
    when(we && waddr === SAVE1){
        save1 := wdata
    }

    // SAVE2：数据保存
    val save2 = RegInit(0.U(32.W))
    when(we && waddr === SAVE2){
        save2 := wdata
    }

    // SAVE3：数据保存
    val save3 = RegInit(0.U(32.W))
    when(we && waddr === SAVE3){
        save3 := wdata
    }

    // LLBCTL：LLBit控制
    val llbctl = RegInit(0.U(32.W))
    when(io.llb.llbitSet){
        llbctl := 0.U(29.W) ## llbctl(2) ## 1.U(2.W)
    }.elsewhen(io.llb.llbitClear){
        llbctl := 0.U(29.W) ## llbctl(2) ## 0.U(2.W)
    }.elsewhen(isEret){
        llbctl := 0.U(31.W) ## Mux(llbctl(2), llbctl(0), 0.U(1.W))
    }.elsewhen(we && waddr === LLBCTL){
        llbctl := 0.U(29.W) ## wdata(2) ## 0.U(1.W) ## Mux(wdata(1), 0.U(1.W), llbctl(0)) 
    }
    val tlbentryIn = io.dmu.tlb.rwf.tlbrdEntry.get
    io.llb.llbit := llbctl(0)

    // TLBIDX：TLB索引
    val tlbidxs = RegInit(VecInit.fill(3)(0.U(32.W)))
    when(io.dmu.tlbsrchEn.get){
        when(wdata(TLBINDEXWIDTH) === 1.U){
            // hit
            tlbidxs.foreach{case(idx) => idx := 0.U(1.W) ## idx(30, TLBINDEXWIDTH) ## wdata(TLBINDEXWIDTH-1, 0)}
        }.otherwise{
            // not hit
            tlbidxs.foreach{case(idx) => idx := 1.U(1.W) ## idx(30, 0)}
        }
    }.elsewhen(io.dmu.tlbrdEn.get){
        tlbidxs.foreach{case(idx) => idx := !tlbentryIn.e ## 0.U(1.W) ## Mux(tlbentryIn.e, tlbentryIn.ps, 0.U(6.W)) ## idx(23, 0)}
    }.elsewhen(we && waddr === TLBIDX){
        tlbidxs.foreach{case(idx) => idx := wdata(31) ## 0.U(1.W) ## wdata(29, 24) ## 0.U((24-TLBINDEXWIDTH).W) ## wdata(TLBINDEXWIDTH-1, 0)}
    }
    io.imu.tlb.rwf.tlbidx := tlbidxs(0)(TLBINDEXWIDTH-1, 0)
    io.dmu.tlb.rwf.tlbidx := tlbidxs(1)(TLBINDEXWIDTH-1, 0)

    // TLBEHI：TLB表项高位
    val tlbehis = RegInit(VecInit.fill(3)(0.U(32.W)))
    when(exception(7) && vppnSave){
        tlbehis.foreach{case(hi) => hi := io.exp.badvExp(31, 13) ## 0.U(13.W)}
    }.elsewhen(io.dmu.tlbrdEn.get){
        tlbehis.foreach{case(hi) => hi := Mux(tlbentryIn.e, tlbentryIn.vppn ## 0.U(13.W), 0.U(32.W))}
    }.elsewhen(we && waddr === TLBEHI){
        tlbehis.foreach{case(hi) => hi := wdata(31, 13) ## 0.U(13.W)}
    }
    io.dmu.tlb.srch.tlbehi.get := tlbehis(1)

    // TLBELO0：TLB表项低位
    val tlbelo0s = RegInit(VecInit.fill(3)(0.U(32.W)))
    when(io.dmu.tlbrdEn.get){
        tlbelo0s.foreach{case(lo) => lo := Mux(tlbentryIn.e, 
                                            lo(31, PALEN-4) ## tlbentryIn.ppn0 ## 0.U(1.W) ## tlbentryIn.g ## tlbentryIn.mat0 ## tlbentryIn.plv0 ## tlbentryIn.d0 ## tlbentryIn.v0,
                                            0.U(32.W))}
    }.elsewhen(we && waddr === TLBELO0){
        tlbelo0s.foreach{case(lo) => lo := 0.U((36 - PALEN).W) ## wdata(PALEN - 5, 8) ## 0.U(1.W) ## wdata(6, 0)}
    }

    // TLBELO1：TLB表项低位
    val tlbelo1s = RegInit(VecInit.fill(3)(0.U(32.W)))
    when(io.dmu.tlbrdEn.get){
        tlbelo1s.foreach{case(lo) => lo := Mux(tlbentryIn.e, 
                                            lo(31, PALEN-4) ## tlbentryIn.ppn1 ## 0.U(1.W) ## tlbentryIn.g ## tlbentryIn.mat1 ## tlbentryIn.plv1 ## tlbentryIn.d1 ## tlbentryIn.v1,
                                            0.U(32.W))}
    }.elsewhen(we && waddr === TLBELO1){
        tlbelo1s.foreach{case(lo) => lo := 0.U((36 - PALEN).W) ## wdata(PALEN - 5, 8) ## 0.U(1.W) ## wdata(6, 0)}
    }

    // ASID：地址空间标识符
    val asids = RegInit(VecInit.fill(2)(0.U(32.W)))
    when(io.dmu.tlbrdEn.get){
        asids.foreach{case(a) => a := a(31, 10) ## Mux(tlbentryIn.e, tlbentryIn.asid, 0.U(10.W))}
    }.elsewhen(we && waddr === ASID){
        asids.foreach{case(a) => a := 0.U(22.W) ## wdata(9, 0)}
    }
    // io.asidGlobal := asid(9, 0)
    io.imu.tlb.ptr.asid := asids(0)(9, 0)
    io.dmu.tlb.ptr.asid := asids(1)(9, 0)

    // PGDL：低半地址空间全局目录基址
    val pgdl = RegInit(0.U(32.W))
    when(we && waddr === PGDL){
        pgdl := wdata(31, 12) ## 0.U(12.W)
    }

    // PGDH：高半地址空间全局目录基址
    val pgdh = RegInit(0.U(32.W))
    when(we && waddr === PGDH){
        pgdh := wdata(31, 12) ## 0.U(12.W)
    }
    
    // PGD：全局目录基址
    val pgd = RegInit(0.U(32.W))
    when(we && waddr === PGD){
        pgd := wdata(31, 12) ## 0.U(12.W)
    }

    // TLBRENTRY：TLB表项重填例外入口地址
    val tlbreentry = RegInit(0.U(32.W))
    when(we && waddr === TLBRENTRY){
        tlbreentry := wdata(31, 6) ## 0.U(6.W)
    }

    // DMW0：直接映射窗口
    val dmw0s = RegInit(VecInit.fill(3)(0.U(32.W)))
    when(we && waddr === DMW0){
        dmw0s.foreach{case(d) => d := wdata(31, 29) ## 0.U(1.W) ## wdata(27, 25) ## 0.U(19.W) ## wdata(5, 3) ## 0.U(2.W) ## wdata(0)}
    }
    io.imu.tlb.dtr.dmw0 := dmw0s(0)
    io.dmu.tlb.dtr.dmw0 := dmw0s(1)


    // DMW1：直接映射窗口
    val dmw1s = RegInit(VecInit.fill(3)(0.U(32.W)))
    when(we && waddr === DMW1){
        dmw1s.foreach{case(d) => d := wdata(31, 29) ## 0.U(1.W) ## wdata(27, 25) ## 0.U(19.W) ## wdata(5, 3) ## 0.U(2.W) ## wdata(0)}
    }
    io.imu.tlb.dtr.dmw1 := dmw1s(0)
    io.dmu.tlb.dtr.dmw1 := dmw1s(1)

    // TID：定时器编号
    val tid = RegInit(0.U(32.W))
    when(we && waddr === TID){
        tid := wdata
    }

    // TCFG：定时器配置
    val tcfg = RegInit(0.U(32.W))
    when(we && waddr === TCFG){
        tcfg := 0.U((32 - TIMERINITWIDTH).W) ## wdata(TIMERINITWIDTH - 1, 0)
    }

    // TVAL：定时器数值
    val tval = RegInit(0.U(32.W))
    when(we && waddr === TCFG){
        tval := 0.U((32 - TIMERINITWIDTH).W) ## wdata(TIMERINITWIDTH - 1, 2) ## 1.U(2.W)
    }.elsewhen(tcfg(0) === 1.U){
        when(tval === 0.U){
            tval := 0.U((32 - TIMERINITWIDTH).W) ## Mux(tcfg(1), tcfg(TIMERINITWIDTH - 1, 2) ## 1.U(2.W), 
                                                                   0.U(TIMERINITWIDTH.W))
        }.otherwise{
            tval := tval - 1.U
        }
    }

    // TICLR：定时器中断清除
    val ticlr = RegInit(0.U(32.W))
    val tvalEdge = ShiftRegister(tval, 1)
    when(we && waddr === TICLR && wdata(0) === 1.U){
        timerIntReg := false.B
    }.elsewhen(tcfg(0) === 1.U && tval === 0.U && tvalEdge === 1.U){
        timerIntReg := true.B
    }

    io.int.interruptVec := Mux(!crmds(2)(2), 0.U(12.W), (estat(12, 11) & ecfg(12, 11)) ## (estat(9, 0) & ecfg(9, 0)))  

    io.imu.tlb.rwf.tlbwrEntry := (new TLBEntryT)(tlbehis(0)(31, 13), tlbidxs(0)(29, 24), tlbelo0s(0)(6) && tlbelo1s(0)(6), asids(0)(9, 0), Mux(estat(21, 16) === 0x3f.U, true.B, !tlbidxs(0)(31)), 
                                        tlbelo0s(0)(PALEN-5, 8), tlbelo0s(0)(3, 2), tlbelo0s(0)(5, 4), tlbelo0s(0)(1), tlbelo0s(0)(0), 
                                        tlbelo1s(0)(PALEN-5, 8), tlbelo1s(0)(3, 2), tlbelo1s(0)(5, 4), tlbelo1s(0)(1), tlbelo1s(0)(0))
    io.dmu.tlb.rwf.tlbwrEntry := (new TLBEntryT)(tlbehis(1)(31, 13), tlbidxs(1)(29, 24), tlbelo0s(1)(6) && tlbelo1s(1)(6), asids(1)(9, 0), Mux(estat(21, 16) === 0x3f.U, true.B, !tlbidxs(1)(31)), 
                                        tlbelo0s(1)(PALEN-5, 8), tlbelo0s(1)(3, 2), tlbelo0s(1)(5, 4), tlbelo0s(1)(1), tlbelo0s(1)(0), 
                                        tlbelo1s(1)(PALEN-5, 8), tlbelo1s(1)(3, 2), tlbelo1s(1)(5, 4), tlbelo1s(1)(1), tlbelo1s(1)(0))

    val rdata = MuxLookup(raddr, 0.U(32.W))(Seq(
        CRMD        -> crmds.last,
        PRMD        -> prmd,
        EUEN        -> euen,
        ECFG        -> ecfg,
        ESTAT       -> estat,
        ERA         -> era,
        BADV        -> badv,
        EENTRY      -> eentry,
        CPUID       -> cpuid,
        SAVE0       -> save0,
        SAVE1       -> save1,
        SAVE2       -> save2,
        SAVE3       -> save3,
        LLBCTL      -> llbctl,
        TLBIDX      -> tlbidxs.last,
        TLBEHI      -> tlbehis.last,
        TLBELO0     -> tlbelo0s.last,
        TLBELO1     -> tlbelo1s.last,
        ASID        -> asids.last,
        PGDL        -> pgdl,
        PGDH        -> pgdh,
        PGD         -> pgd,
        TLBRENTRY   -> tlbreentry,
        DMW0        -> dmw0s.last,
        DMW1        -> dmw1s.last,
        TID         -> tid,
        TCFG        -> tcfg,
        TVAL        -> tval,
        TICLR       -> ticlr
    ))
    io.rw.rdata       := rdata
    io.exp.eentry     := eentry
    io.exp.tlbreentry := tlbreentry

}