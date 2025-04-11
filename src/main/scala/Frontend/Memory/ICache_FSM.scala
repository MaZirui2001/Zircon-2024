import chisel3._
import chisel3.util._
import ZirconConfig.Cache._
import ZirconConfig.StoreBuffer._
import ZirconUtil._

class ICacheFSMCacheIO extends Bundle {
    val rreq        = Input(Bool())
    val uncache     = Input(Bool())
    val hit         = Input(UInt(l1Way.W))
    val cmiss       = Output(Bool())
    val tagvWe     = Output(Vec(l1Way, Bool()))
    val memWe      = Output(Vec(l1Way, Bool()))
    val addr_1H     = Output(UInt(3.W))
    val r1H         = Output(UInt(2.W))
    
    // lru
    val lru         = Input(UInt(2.W))
    val lruUpd     = Output(UInt(2.W))
    val stall       = Input(Bool())
    val flush       = Input(Bool())
}


class ICacheFSML2IO extends Bundle {
    val rreq        = Output(Bool())
    val rrsp        = Input(Bool())
    val miss        = Input(Bool())
}

class ICacheFSMIO extends Bundle {
    val cc = new ICacheFSMCacheIO
    val l2 = new ICacheFSML2IO
}


class ICacheFSM extends Module {
    val io = IO(new ICacheFSMIO)

    // FSM states and registers
    val mIdle :: mMiss :: mRefill :: mWait :: mPause :: Nil = Enum(5)
    val mState = RegInit(mIdle)
    val lruReg = RegInit(0.U(2.W))

    // Output signals (default values)
    io.cc.cmiss         := false.B
    io.cc.tagvWe       := VecInit.fill(l1Way)(false.B)
    io.cc.memWe        := VecInit.fill(l1Way)(false.B)
    io.cc.addr_1H       := 1.U  // default: s1 addr
    io.cc.r1H           := 1.U      // default: mem
    io.cc.lruUpd       := 0.U
    io.l2.rreq          := false.B

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
            io.cc.addr_1H := Mux(io.cc.stall && !io.cc.flush, 2.U, 1.U)
        }

        is(mMiss) {
            // send two requests to L2: this line and next line
            when(io.l2.rrsp) {
                mState := Mux(io.cc.uncache, mWait, mRefill)
            }
            io.l2.rreq := true.B
        }

        is(mRefill) {
            // if next is stall, stall at here
            mState := Mux(io.cc.stall, mRefill, mWait)
            io.cc.addr_1H := 4.U  // choose s3 addr
            when(!io.cc.stall) {
                io.cc.lruUpd := ~lruReg
                io.cc.tagvWe := lruReg.asBools
                io.cc.memWe := lruReg.asBools
            }
        }

        is(mWait) {
            mState := mPause
            // io.cc.addr_1H := 2.U  // choose s2 addr
            io.cc.addr_1H := Mux(io.cc.flush, 1.U, 2.U)
            io.cc.cmiss := true.B
        }

        is(mPause) {
            mState := mIdle
            io.cc.r1H := 2.U
        }
    }
}
