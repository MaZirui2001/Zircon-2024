import chisel3._
import chisel3.util._
import ZirconConfig.Cache._
import ZirconUtil._

class L2CacheFSMCacheIO(ic: Boolean) extends Bundle {
    val rreq    = Input(Bool())
    val wreq    = if(ic) None else Some(Input(Bool()))
    val uncache = Input(Bool())
    val hit     = Input(UInt(l2Way.W))
    val cmiss   = Output(Bool())
    val tagvWe  = Output(Vec(l1Way, Bool()))
    val memWe   = Output(Vec(l1Way, Bool()))
    val addrOH  = Output(UInt(3.W))
    val r1H     = Output(UInt(2.W))
    // write buffer
    val wbufWe  = Output(Bool())
    // lru
    val lru     = Input(UInt(2.W))
    val lruUpd  = Output(UInt(2.W))
    // dirty
    val drty    = if(ic) None else Some(Input(Vec(l1Way, Bool())))
    val drtyWe  = if(ic) None else Some(Output(Vec(l1Way, Bool())))
    val drtyD   = if(ic) None else Some(Output(Vec(l1Way, Bool())))
    // valid for c1
    val vldInv  = if(ic) None else Some(Output(Vec(l1Way, Bool())))
}
class L2CacheFSMMEMIO(ic: Boolean) extends Bundle {
    val rreq  = Output(Bool())
    val rrsp  = Input(Bool())
    val rlast = Input(Bool())

    val wreq  = if(ic) None else Some(Output(Bool()))
    val wrsp  = if(ic) None else Some(Input(Bool()))
    val wlast = if(ic) None else Some(Output(Bool()))
}

class L2CacheDBG extends CacheDBG

class L2CacheFSMIO(ic: Boolean) extends Bundle {
    val cc  = new L2CacheFSMCacheIO(ic)
    val mem = new L2CacheFSMMEMIO(ic)
    val dbg = Output(new L2CacheDBG)
}

class L2CacheFSM(ic: Boolean = false) extends Module{
    val io      = IO(new L2CacheFSMIO(ic))
    val ioc     = io.cc
    val iom     = io.mem
    val iocWreq = ioc.wreq.getOrElse(false.B)
    // main fsm: for read
    val mIdle :: mMiss :: mRefill :: mWait ::  Nil = Enum(4)
    val mState   = RegInit(mIdle)

    val cmiss    = WireDefault(false.B)
    val tagvWe   = WireDefault(VecInit.fill(l1Way)(false.B))
    val memWe    = WireDefault(VecInit.fill(l1Way)(false.B))
    val lruUpd   = WireDefault(0.U(2.W))
    val drtyWe   = WireDefault(VecInit.fill(l1Way)(false.B))
    val drtyD    = WireDefault(VecInit.fill(l1Way)(false.B))
    val addrOH   = WireDefault(1.U(3.W)) // choose s1 addr
    val r1H      = WireDefault(1.U(2.W)) // choose mem
    val vldInv   = WireDefault(VecInit.fill(l1Way)(false.B))

    val wfsmEn   = WireDefault(false.B)
    val wfsmRst  = WireDefault(false.B)
    val wfsmOk   = WireDefault(false.B)
    val wbufWe   = WireDefault(false.B)

    val lru      = RegInit(0.U(2.W))

    io.mem.rreq  := false.B
    val hit      = (if(ic) ioc.hit.orR else ioc.hit(3, 2).orR)
    val hitBits  = (if(ic) ioc.hit(1, 0) else ioc.hit(3, 2)) // for wen of mem, tag and lru

    val visitReg = RegInit(0.U(64.W))
    val hitReg   = RegInit(0.U(64.W))
    visitReg     := visitReg + (mState === mIdle && ioc.rreq && !ioc.uncache)
    hitReg       := hitReg + (mState === mIdle && ioc.rreq && !ioc.uncache && hit)
    io.dbg.visit := visitReg
    io.dbg.hit   := hitReg
    switch(mState){
        is(mIdle){
            when(ioc.rreq || iocWreq){
                when(ioc.uncache){ // uncache
                    mState := Mux(iocWreq, mWait, mMiss)
                    wfsmEn := true.B
                    wbufWe := true.B
                }.elsewhen(!hit){ // cache but !hit
                    mState := mMiss
                    wfsmEn := true.B
                    wbufWe := true.B
                    lru    := ioc.lru
                }.otherwise{ // cache and hit
                    mState := mIdle
                    memWe  := hitBits.asBools.map(_ && iocWreq)
                    addrOH := Mux(iocWreq, 4.U, 1.U) // choose s3 addr when write
                    if(ic){
                        lruUpd := Mux(hitBits.orR, ~hitBits, 0.U(2.W))
                    } else {
                        lruUpd := Mux(hitBits.orR, ~hitBits, 0.U(2.W))
                        drtyD  := VecInit.fill(l1Way)(true.B)
                        drtyWe := memWe
                    }
                }
            }
        }
        is(mMiss){
            io.mem.rreq := true.B
            when(iom.rrsp && iom.rlast){
                mState := Mux(ioc.uncache, mWait, mRefill)
            }
        }
        is(mRefill){
            mState := mWait
            addrOH := 4.U // choose s3 addr
            lruUpd := ~lru
            tagvWe := lru.asBools
            memWe  := tagvWe
            if(!ic){
                drtyWe := tagvWe
                drtyD  := VecInit.fill(l1Way)(iocWreq)
                vldInv := ioc.hit(1, 0).asBools // if dcache data in way0 or way1, invalidate it and fetch from mem 

            }
        }
        is(mWait){
            wfsmRst := true.B
            if(ic){
                mState := Mux(ShiftRegister(mState === mWait, 1, false.B, true.B), mIdle, mWait)
                cmiss  := Mux(ShiftRegister(mState === mWait, 1, false.B, true.B), false.B, true.B)
                addrOH := Mux(ShiftRegister(mState === mWait, 1, false.B, true.B), 1.U, 2.U)
                r1H    := 2.U
            } else {
                mState := Mux(ShiftRegister(mState === mWait && wfsmOk, 1, false.B, true.B), mIdle, mWait)
                cmiss  := Mux(ShiftRegister(mState === mWait && wfsmOk, 1, false.B, true.B), false.B, Mux(wfsmOk, true.B, false.B))
                addrOH := Mux(ShiftRegister(mState === mWait && wfsmOk, 1, false.B, true.B), 1.U, Mux(wfsmOk, 2.U, 1.U))
                r1H    := 2.U
            }
        }
    }
    io.cc.cmiss  := cmiss
    io.cc.tagvWe := tagvWe
    io.cc.memWe  := memWe
    io.cc.lruUpd := lruUpd
    io.cc.addrOH := addrOH
    io.cc.r1H    := r1H
    io.cc.wbufWe := wbufWe
    if(!ic){
        io.cc.drtyWe.get := drtyWe
        io.cc.drtyD.get  := drtyD
        io.cc.vldInv.get := vldInv
    }

    if(!ic){
        iom.wreq.get  := false.B
        iom.wlast.get := false.B
        // write fsm 
        val wIdle :: wWrite :: wFinish :: Nil = Enum(3)
        val wState     = RegInit(wIdle)

        val wCntBits = log2Ceil(l2LineBits / 32) + 1
        val wCnt     = RegInit(0.U((wCntBits.W)))
        when(wfsmEn){
            wCnt := Mux(ioc.uncache, Fill(wCntBits, 1.U), (l2LineBits / 32 - 2).U)
        }.elsewhen(!wCnt(wCntBits-1) && iom.wreq.get && iom.wrsp.get){
            wCnt := wCnt - 1.U
        }
        switch(wState){
            is(wIdle){
                when(wfsmEn){
                    when(ioc.uncache){
                        wState := Mux(iocWreq, wWrite, wFinish)
                    }.otherwise{
                        wState := Mux(Mux1H(ioc.lru, ioc.drty.get), wWrite, wFinish)
                    }
                }
            }
            is(wWrite){
                iom.wreq.get := true.B
                iom.wlast.get := wCnt(wCntBits-1)
                when(iom.wrsp.get && iom.wlast.get){
                    wState := wFinish
                }
            }
            is(wFinish){
                wfsmOk := true.B
                wState := Mux(wfsmRst, wIdle, wFinish)
            }
        }
    }
}   