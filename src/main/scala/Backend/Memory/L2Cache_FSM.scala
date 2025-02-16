import chisel3._
import chisel3.util._
import CPU_Config.Cache._
import Zircon_Util._

class L2Cache_FSM_Cache_IO(ic: Boolean) extends Bundle {
    val rreq    = Input(Bool())
    val wreq    = if(ic) None else Some(Input(Bool()))
    val uncache = Input(Bool())
    val hit     = Input(UInt(l2_way.W))
    val cmiss   = Output(Bool())
    val tagv_we = Output(Vec(l1_way, Bool()))
    val mem_we  = Output(Vec(l1_way, Bool()))
    val addr_1H = Output(UInt(3.W))
    val r1H     = Output(UInt(2.W))
    // write buffer
    val wbuf_we = Output(Bool())
    // lru
    val lru     = Input(UInt(2.W))
    val lru_upd = Output(UInt(2.W))
    // dirty
    val drty    = if(ic) None else Some(Input(Vec(l1_way, Bool())))
    val drty_we = if(ic) None else Some(Output(Vec(l1_way, Bool())))
    val drty_d  = if(ic) None else Some(Output(Vec(l1_way, Bool())))
    // valid for c1
    val vld_inv = if(ic) None else Some(Output(Vec(l1_way, Bool())))
}
class L2Cache_FSM_MEM_IO(ic: Boolean) extends Bundle {
    val rreq    = Output(Bool())
    val rrsp    = Input(Bool())
    val rlast   = Input(Bool())

    val wreq    = if(ic) None else Some(Output(Bool()))
    val wrsp    = if(ic) None else Some(Input(Bool()))
    val wlast   = if(ic) None else Some(Output(Bool()))
}

class L2Cache_FSM_IO(ic: Boolean) extends Bundle {
    val cc   = new L2Cache_FSM_Cache_IO(ic)
    val mem     = new L2Cache_FSM_MEM_IO(ic)
}

class L2Cache_FSM(ic: Boolean = false) extends Module{
    val io = IO(new L2Cache_FSM_IO(ic))
    val ioc = io.cc
    val iom = io.mem
    val ioc_wreq = ioc.wreq.getOrElse(false.B)
    // main fsm: for read
    val m_idle :: m_miss :: m_refill :: m_wait :: m_pause :: Nil = Enum(5)
    val m_state     = RegInit(m_idle)

    val cmiss       = WireDefault(false.B)
    val tagv_we     = WireDefault(VecInit.fill(l1_way)(false.B))
    val mem_we      = WireDefault(VecInit.fill(l1_way)(false.B))
    val lru_upd     = WireDefault(0.U(2.W))
    val drty_we     = WireDefault(VecInit.fill(l1_way)(false.B))
    val drty_d      = WireDefault(VecInit.fill(l1_way)(false.B))
    val addr_1H     = WireDefault(1.U(3.W)) // choose s1 addr
    val r1H         = WireDefault(1.U(2.W)) // choose mem
    val vld_inv     = WireDefault(VecInit.fill(l1_way)(false.B))

    val wfsm_en     = WireDefault(false.B)
    val wfsm_rst    = WireDefault(false.B)
    val wfsm_ok     = WireDefault(false.B)
    val wbuf_we     = WireDefault(false.B)

    val lru         = RegInit(0.U(2.W))

    io.mem.rreq     := false.B
    val hit        = (if(ic) ioc.hit.orR else ioc.hit(3, 2).orR)
    val hit_bits   = (if(ic) ioc.hit(1, 0) else ioc.hit(3, 2)) // for wen of mem, tag and lru
    switch(m_state){
        is(m_idle){
            when(ioc.rreq || ioc_wreq){
                when(ioc.uncache){ // uncache
                    m_state := Mux(ioc_wreq, m_wait, m_miss)
                    wfsm_en := true.B
                    wbuf_we := true.B
                }.elsewhen(!hit){ // cache but !hit
                    m_state := m_miss
                    wfsm_en := true.B
                    wbuf_we := true.B
                    lru     := ioc.lru
                }.otherwise{ // cache and hit
                    m_state := m_idle
                    mem_we  := hit_bits.asBools.map(_ && ioc_wreq)
                    addr_1H := Mux(ioc_wreq, 4.U, 1.U) // choose s3 addr when write
                    if(ic){
                        lru_upd := Mux(hit_bits.orR, ~hit_bits, 0.U(2.W))
                    } else {
                        lru_upd := Mux(hit_bits.orR, ~hit_bits, 0.U(2.W))
                        drty_d  := VecInit.fill(l1_way)(true.B)
                        drty_we := mem_we
                    }
                }
            }
        }
        is(m_miss){
            io.mem.rreq := true.B
            when(iom.rrsp && iom.rlast){
                m_state := Mux(ioc.uncache, m_wait, m_refill)
            }
        }
        is(m_refill){
            m_state := m_wait
            addr_1H := 4.U // choose s3 addr
            lru_upd := ~lru
            tagv_we := lru.asBools
            mem_we  := tagv_we
            if(!ic){
                drty_we := tagv_we
                drty_d  := VecInit.fill(l1_way)(ioc_wreq)
                vld_inv := ioc.hit(1, 0).asBools // if dcache data in way0 or way1, invalidate it and fetch from mem 

            }
        }
        is(m_wait){
            wfsm_rst := true.B
            if(ic){
                m_state := m_pause
                cmiss   := true.B
                addr_1H := 2.U // choose s2 addr
            } else {
                when(wfsm_ok){
                    m_state := m_pause
                    cmiss   := true.B
                    addr_1H := 2.U // choose s2 addr
                }
            }
        }
        is(m_pause){
            m_state := m_idle
            r1H     := 2.U // choose rbuf
        }
    }
    io.cc.cmiss      := cmiss
    io.cc.tagv_we    := tagv_we
    io.cc.mem_we     := mem_we
    io.cc.lru_upd    := lru_upd
    io.cc.addr_1H    := addr_1H
    io.cc.r1H        := r1H
    io.cc.wbuf_we    := wbuf_we
    if(!ic){
        io.cc.drty_we.get    := drty_we
        io.cc.drty_d.get     := drty_d
        io.cc.vld_inv.get    := vld_inv
    }

    if(!ic){
        iom.wreq.get    := false.B
        iom.wlast.get   := false.B
        // write fsm 
        val w_idle :: w_write :: w_finish :: Nil = Enum(3)
        val w_state     = RegInit(w_idle)

        val w_cnt_bits = log2Ceil(l2_line_bits / 32) + 1
        val w_cnt = RegInit(0.U((w_cnt_bits.W)))
        when(wfsm_en){
            w_cnt := Mux(ioc.uncache, Fill(w_cnt_bits, 1.U), (l2_line_bits / 32 - 2).U)
        }.elsewhen(!w_cnt(w_cnt_bits-1) && iom.wreq.get && iom.wrsp.get){
            w_cnt := w_cnt - 1.U
        }
        switch(w_state){
            is(w_idle){
                when(wfsm_en){
                    when(ioc.uncache){
                        w_state := Mux(ioc_wreq, w_write, w_finish)
                    }.otherwise{
                        w_state := Mux(Mux1H(ioc.lru, ioc.drty.get), w_write, w_finish)
                    }
                }
            }
            is(w_write){
                iom.wreq.get := true.B
                iom.wlast.get := w_cnt(w_cnt_bits-1)
                when(iom.wrsp.get && iom.wlast.get){
                    w_state := w_finish
                }
            }
            is(w_finish){
                wfsm_ok := true.B
                w_state := Mux(wfsm_rst, w_idle, w_finish)
            }
        }
    }
}   