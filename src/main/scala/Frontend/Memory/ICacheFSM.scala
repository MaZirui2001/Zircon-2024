import chisel3._
import chisel3.util._
import ZirconConfig.Cache._
import ZirconConfig.StoreBuffer._
import ZirconUtil._

class ICacheFSMCacheIO extends Bundle {
    val rreq    = Input(Bool())
    val uncache = Input(Bool())
    val hit     = Input(UInt(l1Way.W))
    val cmiss   = Output(Bool())
    val tagvWe  = Output(Vec(l1Way, Bool()))
    val memWe   = Output(Vec(l1Way, Bool()))
    val addrOH  = Output(UInt(3.W))
    val r1H     = Output(UInt(2.W))
    
    // lru
    val lru     = Input(UInt(2.W))
    val lruUpd  = Output(UInt(2.W))
    val stall   = Input(Bool())
    val flush   = Input(Bool())

}

class ICacheFSML2IO extends Bundle {
    val rreq = Output(Bool())
    val rrsp = Input(Bool())
    val miss = Input(Bool())
}

class ICacheFSMIO extends Bundle {
    val cc  = new ICacheFSMCacheIO
    val l2  = new ICacheFSML2IO
    val dbg = Output(new ICacheDBG)
}


class ICacheFSM extends Module {
    val io = IO(new ICacheFSMIO)

    // FSM states and registers
    val mIdle :: mMiss :: mRefill :: mWait :: Nil = Enum(4)
    val mState = RegInit(mIdle)
    val lruReg = RegInit(0.U(2.W))

    // Output signals (default values)
    io.cc.cmiss        := false.B
    io.cc.tagvWe       := VecInit.fill(l1Way)(false.B)
    io.cc.memWe        := VecInit.fill(l1Way)(false.B)
    io.cc.addrOH       := 1.U  // default: s1 addr
    io.cc.r1H          := Mux(mState === mWait, 2.U, 1.U)      // default: mem
    io.cc.lruUpd       := 0.U
    io.l2.rreq         := false.B

    val visitReg       = RegInit(0.U(64.W))
    val hitReg         = RegInit(0.U(64.W))
    val missCycleReg   = RegInit(0.U(64.W))
    visitReg           := visitReg + (mState === mIdle && io.cc.rreq && !io.cc.uncache)
    hitReg             := hitReg + (mState === mIdle && io.cc.rreq && !io.cc.uncache && io.cc.hit.orR)
    missCycleReg       := missCycleReg + (mState =/= mIdle)
    io.dbg.visit       := visitReg
    io.dbg.hit         := hitReg
    io.dbg.missCycle   := missCycleReg

    // State transitions
    switch(mState) {
        is(mIdle) {
            when(io.cc.rreq) {
                mState := Mux(io.cc.uncache, mMiss, Mux(io.cc.hit.orR, mIdle, mMiss))
                lruReg := io.cc.lru
                when(!io.cc.uncache && io.cc.hit.orR) {
                    io.cc.lruUpd := ~io.cc.hit
                }
            }
            io.cc.addrOH := Mux(io.cc.stall && !io.cc.flush, 2.U, 1.U)
        }

        is(mMiss) {
            when(io.l2.rrsp) {
                mState := Mux(io.cc.uncache, mWait, mRefill)
            }
            io.l2.rreq := true.B
        }

        is(mRefill) {
            // if next is stall, stall at here
            mState := Mux(io.cc.stall, mRefill, mWait)
            io.cc.addrOH := 4.U  // choose s3 addr
            when(!io.cc.stall) {
                io.cc.lruUpd := ~lruReg
                io.cc.tagvWe := lruReg.asBools
                io.cc.memWe  := lruReg.asBools
            }
        }
        // mWait must last for 2 cycles, to eliminate the mPause state
        is(mWait) {
            mState       := Mux(ShiftRegister(mState === mWait, 1, false.B, true.B), mIdle, mWait)
            io.cc.addrOH := Mux(ShiftRegister(mState === mWait, 1, false.B, true.B), 1.U, Mux(io.cc.flush, 1.U, 2.U))
            io.cc.cmiss  := Mux(ShiftRegister(mState === mWait, 1, false.B, true.B), false.B, true.B)
            // io.cc.r1H    := 2.U
        }
    }
}
