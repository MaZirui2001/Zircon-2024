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
    
    // lru
    val lru         = Input(UInt(2.W))
    val lru_upd     = Output(UInt(2.W))

    val sb_clear    = Input(Bool())
    val sb_full     = Input(Bool())
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

    val m_idle :: m_hold :: m_miss :: m_refill :: m_wait :: m_pause :: Nil = Enum(6)
    val m_state = RegInit(m_idle)

    val lru_reg     = RegInit(0.U(2.W))

    val cmiss   = WireDefault(false.B)
    val tagv_we = WireDefault(VecInit.fill(l1_way)(false.B))
    val mem_we  = WireDefault(VecInit.fill(l1_way)(false.B))
    val lru_upd = WireDefault(0.U(2.W))
    val addr_1H = WireDefault(1.U(3.W)) // choose s1 addr
    val r1H     = WireDefault(1.U(2.W)) // choose mem


    io.l2.rreq := false.B
    switch(m_state){
        is(m_idle){
            addr_1H := Mux(io.cc.sb_full, 2.U, 1.U)
            when(io.cc.wreq){
                m_state := Mux(io.cc.uncache, m_hold, m_idle)
            }.elsewhen(io.cc.rreq){
                when(io.cc.is_latest){
                    m_state := Mux(io.cc.uncache || !io.cc.hit.orR, m_hold, m_idle)
                    lru_reg := io.cc.lru
                    when(!io.cc.uncache && io.cc.hit.orR){
                        lru_upd := ~io.cc.hit
                    }
                }.otherwise{
                    // not latest and uncache must !miss
                    m_state := Mux(io.cc.uncache, m_idle, m_hold)
                }
            }
        }
        is(m_hold){
            when(io.cc.flush){
                m_state := m_wait
            }.elsewhen(io.cc.rreq){
                m_state := Mux(io.cc.sb_clear, m_miss, m_hold) // TODO: sb clear does not means load is the latest
            }.elsewhen(io.cc.wreq){
                m_state := Mux(io.cc.sb_clear, m_wait, m_hold)
            }
        }
        is(m_miss){
            io.l2.rreq := true.B
            when(io.l2.rrsp){
                m_state := Mux(io.cc.uncache, m_wait, m_refill)
            }
        }
        is(m_refill){
            m_state := m_wait
            addr_1H := 4.U // choose s3 addr, for refill the line
            // if has been flushed, do not update anything in cache
            lru_upd := Mux(io.cc.rreq, ~lru_reg, 0.U)
            tagv_we := Mux(io.cc.rreq, lru_reg, 0.U).asBools
            mem_we  := Mux(io.cc.rreq, lru_reg, 0.U).asBools
        }
        is(m_wait){
            m_state := m_pause
            addr_1H := 2.U // choose s2 addr, for read the s2 addr
            cmiss := true.B
        }
        is(m_pause){
            m_state := m_idle
            r1H := 2.U
        }
    }

    io.cc.cmiss := cmiss
    io.cc.tagv_we := tagv_we
    io.cc.mem_we := mem_we
    io.cc.addr_1H := addr_1H
    io.cc.r1H := r1H
    io.cc.lru_upd := lru_upd
    
}
