import chisel3._
import chisel3.util._
import CPU_Config.Cache._

class L2Cache_FSM_Cache_IO extends Bundle {
    val rreq    = Input(Bool())
    val rrsp    = Output(Bool())
    val wreq    = Input(Bool())
    val wrsp    = Output(Bool())
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
    val lru     = Input(Vec(l2_way, Bool()))
    val lru_upd = Output(Vec(l2_way, Bool()))
    // dirty
    val drty    = Input(Vec(l2_way, Bool()))
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

class L2Cache_FSM_IO extends Bundle {
    val cache   = new L2Cache_FSM_Cache_IO
    val mem     = new L2Cache_FSM_MEM_IO
}

class L2Cache_FSM(ic: Boolean = false) extends Module{
    val io = IO(new L2Cache_FSM_IO)
    val ioc = io.cache
    val iom = io.mem
    // main fsm: for read
    val m_idle :: m_miss :: m_refill :: m_wait :: m_pause :: Nil = Enum(5)
    val m_state     = RegInit(m_idle)

    val rrsp        = WireDefault(false.B)
    val wrsp        = WireDefault(false.B)
    val cmiss       = WireDefault(false.B)
    val tagv_we     = WireDefault(VecInit.fill(l2_way)(false.B))
    val mem_we      = WireDefault(VecInit.fill(l2_way)(false.B))
    val lru_upd     = WireDefault(VecInit.fill(l2_way)(false.B))
    val drty_we     = WireDefault(VecInit.fill(l2_way)(false.B))
    val drty_d      = WireDefault(VecInit.fill(l2_way)(false.B))
    val addr_1H     = WireDefault(1.U(3.W)) // choose s1 addr
    val r1H         = WireDefault(1.U(2.W)) // choose mem

    val wfsm_en     = WireDefault(false.B)
    val wfsm_rst    = WireDefault(false.B)
    val wfsm_ok     = WireDefault(false.B)
    val wbuf_we     = WireDefault(false.B)

    io.mem.rreq     := false.B
    switch(m_state){
        is(m_idle){
            when(if(ic) ioc.rreq else ioc.rreq || ioc.wreq){
                when(ioc.uc_in){ // uncache
                    m_state := (if(ic) m_miss else Mux(ioc.wreq, m_wait, m_miss))
                    wfsm_en := true.B
                    wbuf_we := true.B
                }.elsewhen(!ioc.hit.orR){ // cache but !hit
                    m_state := m_miss
                    wfsm_en := true.B
                    wbuf_we := true.B
                }.otherwise{ // cache and hit
                    m_state := m_idle
                    rrsp    := true.B
                    wrsp    := true.B
                    lru_upd := ioc.hit.asBools
                    if(!ic){
                        mem_we  := ioc.hit.asBools.map(_ && ioc.wreq)
                        drty_d  := VecInit.fill(l2_way)(true.B)
                        drty_we := VecInit.tabulate(l2_way)(i => ioc.hit(i) && ioc.wreq)
                    }
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
            m_state := m_pause
            tagv_we := ioc.lru
            mem_we  := ioc.lru
            addr_1H := 4.U // choose s3 addr
            lru_upd := (~ioc.lru.asUInt).asBools
            if(!ic){
                drty_d  := VecInit.fill(l2_way)(ioc.wreq)
                drty_we := VecInit.fill(l2_way)(ioc.wreq || ioc.rreq)
            }
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
            rrsp    := true.B
            wrsp    := true.B
            m_state := m_idle
            r1H     := 2.U // choose rbuf
        }
    }
    io.cache.rrsp       := rrsp
    io.cache.wrsp       := wrsp
    io.cache.cmiss      := cmiss
    io.cache.tagv_we    := tagv_we
    io.cache.mem_we     := mem_we
    io.cache.lru_upd    := lru_upd
    io.cache.drty_we    := drty_we
    io.cache.drty_d     := drty_d
    io.cache.addr_1H    := addr_1H
    io.cache.r1H        := r1H
    io.cache.wbuf_we    := wbuf_we

    // write fsm 
    val w_idle :: w_write :: w_finish :: Nil = Enum(3)
    val w_state     = RegInit(w_idle)

    val w_cnt_bits = log2Ceil(l2_line_bits / 32) + 1
    val w_cnt = RegInit(0.U((w_cnt_bits.W)))
    when(wfsm_en){
        w_cnt := Mux(ioc.uc_in, Fill(w_cnt_bits, 1.U), (l2_line_bits / 32 - 2).U)
    }.elsewhen(!w_cnt(w_cnt_bits-1)){
        w_cnt := w_cnt - 1.U
    }

    iom.wreq    := false.B
    iom.wlast   := false.B
    switch(w_state){
        is(w_idle){
            when(wfsm_en){
                when(ioc.uc_in){
                    if(ic){
                        w_state :=(if(ic) w_finish else Mux(ioc.wreq, w_write, w_finish))
                    }else{
                        w_state := Mux(ioc.wreq, w_write, w_finish)
                    }
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