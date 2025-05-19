import chisel3._
import chisel3.util._
import ZirconConfig.Cache._
import ZirconConfig.StoreBuffer._
import ZirconUtil._

class DCacheFSMCacheIO extends Bundle {
    val rreq      = Input(Bool())
    val wreq      = Input(Bool())
    val uncache   = Input(Bool())
    val hit       = Input(UInt(l1Way.W))
    val isLatest  = Input(Bool())
    val cmiss     = Output(Bool())
    val tagvWe    = Output(Vec(l1Way, Bool()))
    val memWe     = Output(Vec(l1Way, Bool()))
    val addrOH    = Output(UInt(3.W))
    val r1H       = Output(UInt(2.W))
    val rbufClear = Output(Bool())
    
    // lru
    val lru       = Input(UInt(2.W))
    val lruUpd    = Output(UInt(2.W))

    val sbClear   = Input(Bool())
    val sbFull    = Input(Bool())
    val c2Wreq    = Input(Bool())
    val sbLock    = Output(Bool())
    val flush     = Input(Bool())
}

class DCacheFSML2IO extends Bundle {
    val rreq      = Output(Bool())
    val rrsp      = Input(Bool())
    val miss      = Input(Bool())
}

class DCacheFSMIO extends Bundle {
    val cc   = new DCacheFSMCacheIO
    val l2   = new DCacheFSML2IO
    val dbg  = Output(new DCacheReadDBG)
}


class DCacheFSM extends Module {
    val io = IO(new DCacheFSMIO)

    // FSM states and registers
    val mIdle :: mHold :: mMiss :: mRefill :: mWait :: Nil = Enum(5)
    val mState = RegInit(mIdle)
    val lruReg = RegInit(0.U(2.W))

    // Output signals (default values)
    io.cc.cmiss        := false.B
    io.cc.tagvWe       := VecInit.fill(l1Way)(false.B)
    io.cc.memWe        := VecInit.fill(l1Way)(false.B)
    io.cc.addrOH       := 1.U  // default: s1 addr
    io.cc.r1H          := 1.U      // default: mem
    io.cc.lruUpd       := 0.U
    io.cc.rbufClear    := false.B
    io.cc.sbLock       := false.B
    io.l2.rreq         := false.B

    val visitReg       = RegInit(0.U(64.W))
    val hitReg         = RegInit(0.U(64.W))
    val missCycleReg   = RegInit(0.U(64.W))
    val sbFullCycleReg = RegInit(0.U(64.W))
    visitReg           := visitReg + (mState === mIdle && io.cc.rreq && !io.cc.uncache)
    hitReg             := hitReg + (mState === mIdle && io.cc.rreq && !io.cc.uncache && io.cc.hit.orR)
    missCycleReg       := missCycleReg + (mState =/= mIdle)
    sbFullCycleReg     := sbFullCycleReg + io.cc.sbFull
    io.dbg.visit       := visitReg
    io.dbg.hit         := hitReg
    io.dbg.missCycle   := missCycleReg
    io.dbg.sbFullCycle := sbFullCycleReg

    // State transitions
    switch(mState) {
        is(mIdle) {
            when(io.cc.rreq) {
                when(io.cc.isLatest) {
                    mState := Mux(io.cc.uncache, mHold, Mux(io.cc.hit.orR, mIdle, mMiss))
                }.otherwise {
                    // not latest and uncache must !miss
                    mState := Mux(io.cc.uncache, mIdle, Mux(io.cc.hit.orR, mIdle, mMiss))
                }
                lruReg := io.cc.lru
                when(!io.cc.uncache && io.cc.hit.orR) {
                    io.cc.lruUpd := ~io.cc.hit
                }
                when(!(io.cc.uncache || io.cc.hit.orR)) {
                    io.cc.rbufClear := true.B
                }
            }
            io.cc.addrOH := Mux(io.cc.sbFull, 2.U, 1.U)
        }

        is(mHold) {
            when(io.cc.flush) {
                mState := mWait
            }.elsewhen(io.cc.rreq) {
                mState := Mux(io.cc.sbClear, mMiss, mHold) // TODO: sb clear does not means load is the latest
            }
            // .elsewhen(io.cc.wreq) {
            //     mState := Mux(io.cc.sbClear, mWait, mHold)
            // }
        }

        is(mMiss) {
            when(io.l2.rrsp) {
                mState := Mux(io.cc.uncache, mWait, mRefill)
            }
            io.cc.sbLock := true.B
            io.l2.rreq := true.B
        }

        is(mRefill) {
            // lock the sb, and when the c2 is empty, refill the cache line
            mState := Mux(io.cc.c2Wreq, mRefill, mWait)
            io.cc.sbLock := true.B
            io.cc.addrOH := 4.U  // choose s3 addr
            when(io.cc.rreq && !io.cc.c2Wreq) {
                io.cc.lruUpd := ~lruReg
                io.cc.tagvWe := lruReg.asBools
                io.cc.memWe := lruReg.asBools
            }
        }

        is(mWait) {
            mState := Mux(ShiftRegister(mState === mWait, 1, false.B, true.B), mIdle, mWait)
            io.cc.addrOH := Mux(ShiftRegister(mState === mWait, 1, false.B, true.B), 1.U, 2.U)
            io.cc.cmiss  := Mux(ShiftRegister(mState === mWait, 1, false.B, true.B), false.B, true.B)
            io.cc.r1H    := 2.U
        }
    }
}
