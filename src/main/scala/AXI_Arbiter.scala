import chisel3._
import chisel3.util._

class AXI_Arbiter_IO extends Bundle {
    val l2              = Vec(2, Flipped(new Mem_IO))
    // for Main Memory
    val araddr          = Output(UInt(32.W))
    val arburst         = Output(UInt(2.W))
    val arid            = Output(UInt(4.W))
    val arlen           = Output(UInt(8.W))      
    val arready         = Input(Bool())
    val arsize          = Output(UInt(3.W))
    val arvalid         = Output(Bool())

    val awaddr          = Output(UInt(32.W))
    val awburst         = Output(UInt(2.W))
    val awid            = Output(UInt(4.W))
    val awlen           = Output(UInt(8.W))
    val awready         = Input(Bool())
    val awsize          = Output(UInt(3.W))
    val awvalid         = Output(Bool())

    val bid             = Input(UInt(4.W))
    val bready          = Output(Bool())
    val bresp           = Input(UInt(2.W))
    val bvalid          = Input(Bool())

    val rdata           = Input(UInt(32.W))
    val rid             = Input(UInt(4.W))
    val rlast           = Input(Bool())
    val rready          = Output(Bool())
    val rresp           = Input(UInt(2.W))
    val rvalid          = Input(Bool())

    val wdata           = Output(UInt(32.W))
    val wlast           = Output(Bool())
    val wready          = Input(Bool())
    val wstrb           = Output(UInt(4.W))
    val wvalid          = Output(Bool())
}

class AXI_Arbiter extends Module{
    val io = IO(new AXI_Arbiter_IO)

    // read arbiter
    io.l2.foreach{ l2 =>
        l2.rrsp     := false.B
        l2.rlast    := false.B
        l2.rdata    := io.rdata
    }

    io.araddr   := io.l2(0).raddr
    io.arburst  := 1.U
    io.arid     := 0.U
    io.arlen    := io.l2(0).rlen
    io.arsize   := io.l2(0).rsize
    io.arvalid  := false.B
    io.rready   := false.B

    // read FSM
    val r_idle :: r_iar :: r_ir :: r_dar :: r_dr :: Nil = Enum(5)

    val r_state = RegInit(r_idle)
    switch(r_state){
        is(r_idle){
            // idle state
            r_state := Mux(io.l2(1).rreq, r_dar, Mux(io.l2(0).rreq, r_iar, r_idle))
        }
        is(r_iar){
            // icache ar shake hand state
            io.arvalid  := true.B
            io.araddr   := io.l2(0).raddr
            io.arsize   := io.l2(0).rsize
            io.arlen    := io.l2(0).rlen
            r_state     := Mux(io.arready, r_ir, r_iar)
        }
        is(r_ir){
            // icache read data state
            io.l2(0).rrsp   := io.rvalid
            io.l2(0).rlast  := io.rlast
            io.rready       := true.B
            r_state         := Mux(io.rvalid && io.rlast && io.rready, r_idle, r_ir)
        }
        is(r_dar){
            // dcache ar shake hand state
            io.arvalid  := true.B
            io.araddr   := io.l2(1).raddr
            io.arsize   := io.l2(1).rsize
            io.arlen    := io.l2(1).rlen
            r_state     := Mux(io.arready, r_dr, r_dar)
        }
        is(r_dr){
            // dcache read data state
            io.l2(1).rrsp   := io.rvalid
            io.l2(1).rlast  := io.rlast
            io.rready       := true.B
            r_state         := Mux(io.rvalid && io.rlast && io.rready, r_idle, r_dr)
        }
    }

    // write FSM
    val w_idle :: w_iaw :: w_iw :: w_ib :: w_daw :: w_dw :: w_db :: Nil = Enum(7)
    val w_state = RegInit(w_idle)

    io.l2.foreach{ l2 => l2.wrsp := false.B }
    io.awaddr   := io.l2(0).waddr
    io.awburst  := 1.U
    io.awid     := 0.U
    io.awlen    := io.l2(0).wlen
    io.awsize   := io.l2(0).wsize
    io.awvalid  := false.B
    io.wdata    := io.l2(0).wdata
    io.wlast    := false.B
    io.wstrb    := io.l2(0).wstrb
    io.wvalid   := false.B
    io.bready   := false.B

    switch(w_state){
        is(w_idle){
            // idle state
            w_state := Mux(io.l2(1).wreq, w_daw, Mux(io.l2(0).wreq, w_iaw, w_idle))
        }
        is(w_iaw){
            // icache aw shake hand state
            io.awvalid  := true.B
            io.awaddr   := io.l2(0).waddr
            io.awsize   := io.l2(0).wsize
            io.awlen    := io.l2(0).wlen
            w_state     := Mux(io.awready, w_iw, w_iaw)
        }
        is(w_iw){
            // icache write data state
            io.l2(0).wrsp   := io.wready
            io.wvalid       := io.l2(0).wreq
            io.wdata        := io.l2(0).wdata
            io.wstrb        := io.l2(0).wstrb
            io.wlast        := io.l2(0).wlast
            w_state         := Mux(io.wready && io.wlast && io.wvalid, w_ib, w_iw)
        }
        is(w_ib){
            // icache write response state
            io.bready   := true.B
            w_state     := Mux(io.bvalid, w_idle, w_ib)
        }
        is(w_daw){
            // dcache aw shake hand state
            io.awvalid  := true.B
            io.awaddr   := io.l2(1).waddr
            io.awsize   := io.l2(1).wsize
            io.awlen    := io.l2(1).wlen
            w_state     := Mux(io.awready, w_dw, w_daw)
        }
        is(w_dw){
            // dcache write data state
            io.l2(1).wrsp   := io.wready
            io.wvalid       := io.l2(1).wreq
            io.wdata        := io.l2(1).wdata
            io.wstrb        := io.l2(1).wstrb
            io.wlast        := io.l2(1).wlast
            w_state         := Mux(io.wready && io.wlast && io.wvalid, w_db, w_dw)
        }
        is(w_db){
            // dcache write response state
            io.bready   := true.B
            w_state     := Mux(io.bready && io.bvalid, w_idle, w_db)
        }
    }
}