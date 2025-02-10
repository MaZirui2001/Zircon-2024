import chisel3._
import chisel3.util._
import CPU_Config.Cache._
import CPU_Config.StoreBuffer._
import Zircon_Util._

class DCache_FSM_Cache_IO extends Bundle {
    val rreq        = Input(Bool())
    val wreq        = Input(Bool())
    val uncache     = Input(Bool())
    val hit         = Input(UInt(l1_way.W))
    val is_latest   = Input(Bool())
    val cmiss       = Output(Bool())
    val tagv_we     = Output(Vec(l1_way, Bool()))
    val mem_we      = Output(Vec(l1_way, Bool()))
    val addr_1H     = Output(UInt(3.W))
    val r1H         = Output(UInt(2.W))
    val rbuf_clear  = Output(Bool())
    
    // lru
    val lru         = Input(UInt(2.W))
    val lru_upd     = Output(UInt(2.W))

    val sb_clear    = Input(Bool())
    val sb_full     = Input(Bool())
    val sb_lock     = Output(Bool())
    val flush       = Input(Bool())
}


class DCache_FSM_L2_IO extends Bundle {
    val rreq        = Output(Bool())
    val rrsp        = Input(Bool())
    val miss        = Input(Bool())
}

class DCache_FSM_IO extends Bundle {
    val cc = new DCache_FSM_Cache_IO
    val l2 = new DCache_FSM_L2_IO
}


class DCache_FSM extends Module {
    val io = IO(new DCache_FSM_IO)

    // FSM states and registers
    val m_idle :: m_hold :: m_miss :: m_refill :: m_wait :: m_pause :: Nil = Enum(6)
    val m_state = RegInit(m_idle)
    val lru_reg = RegInit(0.U(2.W))

    // Output signals (default values)
    io.cc.cmiss         := false.B
    io.cc.tagv_we       := VecInit.fill(l1_way)(false.B)
    io.cc.mem_we        := VecInit.fill(l1_way)(false.B)
    io.cc.addr_1H       := 1.U  // default: s1 addr
    io.cc.r1H           := 1.U      // default: mem
    io.cc.lru_upd       := 0.U
    io.cc.rbuf_clear    := false.B
    io.cc.sb_lock       := false.B
    io.l2.rreq          := false.B

    // State transitions
    switch(m_state) {
        is(m_idle) {
            when(io.cc.wreq) {
                m_state := Mux(io.cc.uncache, m_hold, m_idle)
            }.elsewhen(io.cc.rreq) {
                when(io.cc.is_latest) {
                    m_state := Mux(io.cc.uncache, m_hold, Mux(io.cc.hit.orR, m_idle, m_miss))
                    lru_reg := io.cc.lru
                    when(!io.cc.uncache && io.cc.hit.orR) {
                        io.cc.lru_upd := ~io.cc.hit
                    }
                    when(!(io.cc.uncache || io.cc.hit.orR)) {
                        io.cc.rbuf_clear := true.B
                    }
                }.otherwise {
                    // not latest and uncache must !miss
                    m_state := Mux(io.cc.uncache, m_idle, m_hold)
                }
            }
            io.cc.addr_1H := Mux(io.cc.sb_full, 2.U, 1.U)
        }

        is(m_hold) {
            when(io.cc.flush) {
                m_state := m_wait
            }.elsewhen(io.cc.rreq) {
                m_state := Mux(io.cc.sb_clear, m_miss, m_hold) // TODO: sb clear does not means load is the latest
            }.elsewhen(io.cc.wreq) {
                m_state := Mux(io.cc.sb_clear, m_wait, m_hold)
            }
        }

        is(m_miss) {
            when(io.l2.rrsp) {
                m_state := Mux(io.cc.uncache, m_wait, m_refill)
            }
            io.cc.sb_lock := true.B
            io.l2.rreq := true.B
        }

        is(m_refill) {
            // lock the sb, and when the c2 is empty, refill the cache line
            m_state := m_wait
            io.cc.sb_lock := true.B
            io.cc.addr_1H := 4.U  // choose s3 addr
            when(io.cc.rreq) {
                io.cc.lru_upd := ~lru_reg
                io.cc.tagv_we := lru_reg.asBools
                io.cc.mem_we := lru_reg.asBools
            }
        }

        is(m_wait) {
            m_state := m_pause
            io.cc.addr_1H := 2.U  // choose s2 addr
            io.cc.cmiss := true.B
        }

        is(m_pause) {
            m_state := m_idle
            io.cc.r1H := 2.U
        }
    }
}
