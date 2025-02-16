import chisel3._
import chisel3.util._

class AXI_IO extends Bundle {
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

class AXI_Arbiter_IO extends Bundle {
    val l2  = MixedVec(Seq(Flipped(new Mem_IO(true)), Flipped(new Mem_IO(false))))
    // for Main Memory
    val axi = new AXI_IO
}

class AXI_Arbiter extends Module{
    val io = IO(new AXI_Arbiter_IO)

    // read arbiter
    io.l2.foreach{ l2 =>
        l2.rrsp     := false.B
        l2.rlast    := false.B
        l2.rdata    := io.axi.rdata
    }

    io.axi.araddr   := io.l2(0).raddr
    io.axi.arburst  := 1.U
    io.axi.arid     := 0.U
    io.axi.arlen    := io.l2(0).rlen
    io.axi.arsize   := io.l2(0).rsize
    io.axi.arvalid  := false.B
    io.axi.rready   := false.B

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
            io.axi.arvalid  := true.B
            io.axi.araddr   := io.l2(0).raddr
            io.axi.arsize   := io.l2(0).rsize
            io.axi.arlen    := io.l2(0).rlen
            r_state         := Mux(io.axi.arready, r_ir, r_iar)
        }
        is(r_ir){
            // icache read data state
            io.l2(0).rrsp   := io.axi.rvalid
            io.l2(0).rlast  := io.axi.rlast
            io.axi.rready   := true.B
            r_state         := Mux(io.axi.rvalid && io.axi.rlast && io.axi.rready, r_idle, r_ir)
        }
        is(r_dar){
            // dcache ar shake hand state
            io.axi.arvalid  := true.B
            io.axi.araddr   := io.l2(1).raddr
            io.axi.arsize   := io.l2(1).rsize
            io.axi.arlen    := io.l2(1).rlen
            r_state         := Mux(io.axi.arready, r_dr, r_dar)
        }
        is(r_dr){
            // dcache read data state
            io.l2(1).rrsp   := io.axi.rvalid
            io.l2(1).rlast  := io.axi.rlast
            io.axi.rready   := true.B
            r_state         := Mux(io.axi.rvalid && io.axi.rlast && io.axi.rready, r_idle, r_dr)
        }
    }

    // write FSM
    val w_idle :: w_daw :: w_dw :: w_db :: Nil = Enum(4)
    val w_state = RegInit(w_idle)

    // io.l2.foreach{ l2 => l2.wrsp.get := false.B }
    io.l2(1).wrsp.get := false.B
    io.axi.awaddr   := io.l2(1).waddr.get
    io.axi.awburst  := 1.U
    io.axi.awid     := 0.U
    io.axi.awlen    := io.l2(1).wlen.get
    io.axi.awsize   := io.l2(1).wsize.get
    io.axi.awvalid  := false.B
    io.axi.wdata    := io.l2(1).wdata.get
    io.axi.wlast    := false.B
    io.axi.wstrb    := io.l2(1).wstrb.get
    io.axi.wvalid   := false.B
    io.axi.bready   := false.B

    switch(w_state){
        is(w_idle){
            // idle state
            w_state := Mux(io.l2(1).wreq.get, w_daw, w_idle)
        }
        is(w_daw){
            // dcache aw shake hand state
            io.axi.awvalid  := true.B
            w_state         := Mux(io.axi.awready, w_dw, w_daw)
        }
        is(w_dw){
            // dcache write data state
            io.l2(1).wrsp.get   := io.axi.wready
            io.axi.wvalid   := io.l2(1).wreq.get
            io.axi.wlast    := io.l2(1).wlast.get
            w_state         := Mux(io.axi.wready && io.axi.wlast && io.axi.wvalid, w_db, w_dw)
        }
        is(w_db){
            // dcache write response state
            io.axi.bready   := true.B
            w_state         := Mux(io.axi.bready && io.axi.bvalid, w_idle, w_db)
        }
    }
}