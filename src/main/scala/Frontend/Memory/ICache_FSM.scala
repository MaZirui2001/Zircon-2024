import chisel3._
import chisel3.util._
import CPU_Config.Cache._
import CPU_Config.StoreBuffer._
import Zircon_Util._

class ICache_FSM_Cache_IO extends Bundle {
    val rreq        = Input(Bool())
    val uncache     = Input(Bool())
    val hit         = Input(UInt(l1_way.W))
    val cmiss       = Output(Bool())
    val tagv_we     = Output(Vec(l1_way, Bool()))
    val mem_we      = Output(Vec(l1_way, Bool()))
    val addr_1H     = Output(UInt(3.W))
    val r1H         = Output(UInt(2.W))
    
    // lru
    val lru         = Input(UInt(2.W))
    val lru_upd     = Output(UInt(2.W))
    val stall       = Input(Bool())
}


class ICache_FSM_L2_IO extends Bundle {
    val rreq        = Output(Bool())
    val rrsp        = Input(Bool())
    val miss        = Input(Bool())
}

class ICache_FSM_IO extends Bundle {
    val cc = new ICache_FSM_Cache_IO
    val l2 = new ICache_FSM_L2_IO
}


class ICache_FSM extends Module {
    val io = IO(new ICache_FSM_IO)

    // FSM states and registers
    val m_idle :: m_miss :: m_refill :: m_wait :: m_pause :: Nil = Enum(5)
    val m_state = RegInit(m_idle)
    val lru_reg = RegInit(0.U(2.W))

    // Output signals (default values)
    io.cc.cmiss         := false.B
    io.cc.tagv_we       := VecInit.fill(l1_way)(false.B)
    io.cc.mem_we        := VecInit.fill(l1_way)(false.B)
    io.cc.addr_1H       := 1.U  // default: s1 addr
    io.cc.r1H           := 1.U      // default: mem
    io.cc.lru_upd       := 0.U
    io.l2.rreq          := false.B

    // State transitions
    switch(m_state) {
        is(m_idle) {
            when(io.cc.rreq) {
                m_state := Mux(io.cc.uncache, m_miss, Mux(io.cc.hit.orR, m_idle, m_miss))
                lru_reg := io.cc.lru
                when(!io.cc.uncache && io.cc.hit.orR) {
                    io.cc.lru_upd := ~io.cc.hit
                }
            }
            io.cc.addr_1H := Mux(io.cc.stall, 2.U, 1.U)
        }

        is(m_miss) {
            // send two requests to L2: this line and next line
            when(io.l2.rrsp) {
                m_state := Mux(io.cc.uncache, m_wait, m_refill)
            }
            io.l2.rreq := true.B
        }

        is(m_refill) {
            // if next is stall, stall at here
            m_state := Mux(io.cc.stall, m_refill, m_wait)
            io.cc.addr_1H := 4.U  // choose s3 addr
            when(!io.cc.stall) {
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
