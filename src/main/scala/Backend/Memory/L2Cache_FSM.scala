import chisel3._
import chisel3.util._
import CPU_Config.Cache._
import Zircon_Util._

class L2Cache_FSM_Cache_IO(ic: Boolean = false) extends Bundle {
    val rreq    = Input(Bool())
    val wreq    = if(ic) None else Some(Input(Bool()))
    val uc_in   = Input(Bool())
    val hit     = Input(UInt(l2_way.W))
    val cmiss   = Output(Bool())
    val tagv_we = Output(Vec(l2_way, Bool()))
    val mem_we  = Output(Vec(l2_way, Bool()))
    val addr_1H = Output(UInt(3.W))
    val r1H     = Output(UInt(2.W))
    // write buffer
    val wbuf_we = Output(Bool())
    // lru
    val lru     = Input(UInt(2.W))
    val lru_upd = Output(UInt(2.W))
    // dirty
    val drty    = Input(Vec(l1_way, Bool()))
    val drty_we = Output(Vec(l2_way, Bool()))
    val drty_d  = Output(Vec(l2_way, Bool()))
}
class L2Cache_FSM_MEM_IO extends Bundle {
    val rreq    = Output(Bool())
    val rrsp    = Input(Bool())
    val rlast   = Input(Bool())

    val wreq    = Output(Bool())
    val wrsp    = Input(Bool())
    val wlast   = Output(Bool())
}

class L2Cache_FSM_IO(ic: Boolean = false) extends Bundle {
    val cache   = new L2Cache_FSM_Cache_IO(ic)
    val mem     = new L2Cache_FSM_MEM_IO
}

class L2Cache_FSM(ic: Boolean = false) extends Module{
    val io = IO(new L2Cache_FSM_IO(ic))
    val ioc = io.cache
    val iom = io.mem
    val ioc_wreq = ioc.wreq.getOrElse(false.B)
    // main fsm: for read
    val m_idle :: m_miss :: m_refill :: m_wait :: m_pause :: Nil = Enum(5)
    val m_state     = RegInit(m_idle)

    val cmiss       = WireDefault(false.B)
    val tagv_we     = WireDefault(VecInit.fill(l2_way)(false.B))
    val mem_we      = WireDefault(VecInit.fill(l2_way)(false.B))
    val lru_upd     = WireDefault(0.U(2.W))
    val drty_we     = WireDefault(VecInit.fill(l2_way)(false.B))
    val drty_d      = WireDefault(VecInit.fill(l2_way)(false.B))
    val addr_1H     = WireDefault(1.U(3.W)) // choose s1 addr
    val r1H         = WireDefault(1.U(2.W)) // choose mem

    val wfsm_en     = WireDefault(false.B)
    val wfsm_rst    = WireDefault(false.B)
    val wfsm_ok     = WireDefault(false.B)
    val wbuf_we     = WireDefault(false.B)

    val lru         = RegInit(0.U(2.W))

    io.mem.rreq     := false.B
    switch(m_state){
        is(m_idle){
            when(ioc.rreq || ioc_wreq){
                when(ioc.uc_in){ // uncache
                    m_state := Mux(ioc_wreq, m_wait, m_miss)
                    wfsm_en := true.B
                    wbuf_we := true.B
                }.elsewhen(!ioc.hit.orR){ // cache but !hit
                    m_state := m_miss
                    wfsm_en := true.B
                    wbuf_we := true.B
                    lru     := ioc.lru
                }.otherwise{ // cache and hit
                    m_state := m_idle
                    lru_upd := (if(ic) ~ioc.hit else (~ioc.hit)(3, 2))
                    mem_we  := ioc.hit.asBools.map(_ && ioc_wreq)
                    drty_d  := VecInit.fill(l2_way)(true.B)
                    drty_we := VecInit.tabulate(l2_way)(i => ioc.hit(i) && ioc_wreq)
                    addr_1H := Mux(ioc_wreq, 4.U, 1.U) // choose s3 addr when write
                }
            }
        }
        is(m_miss){
            io.mem.rreq := true.B
            when(iom.rrsp && iom.rlast){
                m_state := m_refill
            }
        }
        is(m_refill){
            m_state := m_wait
            tagv_we := (if(ic) (0.U(2.W) ## lru).asBools else (lru ## 0.U(2.W)).asBools)
            mem_we  := (if(ic) (0.U(2.W) ## lru).asBools else (lru ## 0.U(2.W)).asBools)
            addr_1H := 4.U // choose s3 addr
            lru_upd := ~lru
            drty_d  := VecInit.fill(l2_way)(ioc_wreq)
            drty_we := (if(ic) VecInit.fill(l2_way)(false.B) else (lru ## 0.U(2.W)).asBools)
        }
        is(m_wait){
            wfsm_rst := true.B
            when(wfsm_ok){
                m_state := m_pause
                cmiss   := true.B
                addr_1H := 2.U // choose s2 addr
            }
        }
        is(m_pause){
            m_state := m_idle
            r1H     := 2.U // choose rbuf
        }
    }
    io.cache.cmiss      := cmiss
    io.cache.tagv_we    := tagv_we
    io.cache.mem_we     := mem_we
    io.cache.lru_upd    := lru_upd
    io.cache.drty_we    := drty_we
    io.cache.drty_d     := drty_d
    io.cache.addr_1H    := addr_1H
    io.cache.r1H        := r1H
    io.cache.wbuf_we    := (if(ic) false.B else wbuf_we)

    // write fsm 
    val w_idle :: w_write :: w_finish :: Nil = Enum(3)
    val w_state     = RegInit(w_idle)

    val w_cnt_bits = log2Ceil(l2_line_bits / 32) + 1
    val w_cnt = RegInit(0.U((w_cnt_bits.W)))
    when(wfsm_en){
        w_cnt := Mux(ioc.uc_in, Fill(w_cnt_bits, 1.U), (l2_line_bits / 32 - 2).U)
    }.elsewhen(!w_cnt(w_cnt_bits-1) && iom.wreq && iom.wrsp){
        w_cnt := w_cnt - 1.U
    }

    iom.wreq    := false.B
    iom.wlast   := false.B
    switch(w_state){
        is(w_idle){
            when(wfsm_en){
                when(ioc.uc_in){
                    w_state := Mux(ioc_wreq, w_write, w_finish)
                }.otherwise{
                    w_state := Mux(Mux1H(ioc.lru, ioc.drty), w_write, w_finish)
                }
            }
        }
        is(w_write){
            iom.wreq := true.B
            iom.wlast := w_cnt(w_cnt_bits-1)
            when(iom.wrsp && iom.wlast){
                w_state := w_finish
            }
        }
        is(w_finish){
            wfsm_ok := true.B
            w_state := Mux(wfsm_rst, w_idle, w_finish)
        }
    }
}   