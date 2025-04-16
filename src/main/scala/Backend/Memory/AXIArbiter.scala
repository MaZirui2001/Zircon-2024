import chisel3._
import chisel3.util._

class AXIIO extends Bundle {
    val araddr  = Output(UInt(32.W))
    val arburst = Output(UInt(2.W))
    val arid    = Output(UInt(4.W))
    val arlen   = Output(UInt(8.W))      
    val arready = Input(Bool())
    val arsize  = Output(UInt(3.W))
    val arvalid = Output(Bool())

    val awaddr  = Output(UInt(32.W))
    val awburst = Output(UInt(2.W))
    val awid    = Output(UInt(4.W))
    val awlen   = Output(UInt(8.W))
    val awready = Input(Bool())
    val awsize  = Output(UInt(3.W))
    val awvalid = Output(Bool())

    val bid     = Input(UInt(4.W))
    val bready  = Output(Bool())
    val bresp   = Input(UInt(2.W))
    val bvalid  = Input(Bool())

    val rdata   = Input(UInt(32.W))
    val rid     = Input(UInt(4.W))
    val rlast   = Input(Bool())
    val rready  = Output(Bool())
    val rresp   = Input(UInt(2.W))
    val rvalid  = Input(Bool())

    val wdata   = Output(UInt(32.W))
    val wlast   = Output(Bool())
    val wready  = Input(Bool())
    val wstrb   = Output(UInt(4.W))
    val wvalid  = Output(Bool())
}

class AXIArbiterIO extends Bundle {
    val l2  = MixedVec(Seq(Flipped(new MemIO(true)), Flipped(new MemIO(false))))
    // for Main Memory
    val axi = new AXIIO
}

class AXIArbiter extends Module{
    val io = IO(new AXIArbiterIO)

    // read arbiter
    io.l2.foreach{ l2 =>
        l2.rrsp  := false.B
        l2.rlast := false.B
        l2.rdata := io.axi.rdata
    }

    io.axi.araddr  := io.l2(0).raddr
    io.axi.arburst := 1.U
    io.axi.arid    := 0.U
    io.axi.arlen   := io.l2(0).rlen
    io.axi.arsize  := io.l2(0).rsize
    io.axi.arvalid := false.B
    io.axi.rready  := false.B

    // read FSM
    val rIdle :: rIar :: rIr :: rDar :: rDr :: Nil = Enum(5)

    val rState = RegInit(rIdle)
    switch(rState){
        is(rIdle){
            // idle state
            rState := Mux(io.l2(1).rreq, rDar, Mux(io.l2(0).rreq, rIar, rIdle))
        }
        is(rIar){
            // icache ar shake hand state
            io.axi.arvalid := true.B
            io.axi.araddr  := io.l2(0).raddr
            io.axi.arsize  := io.l2(0).rsize
            io.axi.arlen   := io.l2(0).rlen
            rState         := Mux(io.axi.arready, rIr, rIar)
        }
        is(rIr){
            // icache read data state
            io.l2(0).rrsp  := io.axi.rvalid
            io.l2(0).rlast := io.axi.rlast
            io.axi.rready  := true.B
            rState         := Mux(io.axi.rvalid && io.axi.rlast && io.axi.rready, rIdle, rIr)
        }
        is(rDar){
            // dcache ar shake hand state
            io.axi.arvalid := true.B
            io.axi.araddr  := io.l2(1).raddr
            io.axi.arsize  := io.l2(1).rsize
            io.axi.arlen   := io.l2(1).rlen
            rState         := Mux(io.axi.arready, rDr, rDar)
        }
        is(rDr){
            // dcache read data state
            io.l2(1).rrsp  := io.axi.rvalid
            io.l2(1).rlast := io.axi.rlast
            io.axi.rready  := true.B
            rState         := Mux(io.axi.rvalid && io.axi.rlast && io.axi.rready, rIdle, rDr)
        }
    }

    // write FSM
    val wIdle :: wDaw :: wDw :: wDb :: Nil = Enum(4)
    val wState = RegInit(wIdle)

    // io.l2.foreach{ l2 => l2.wrsp.get := false.B }
    io.l2(1).wrsp.get := false.B
    io.axi.awaddr     := io.l2(1).waddr.get
    io.axi.awburst    := 1.U
    io.axi.awid       := 0.U
    io.axi.awlen      := io.l2(1).wlen.get
    io.axi.awsize     := io.l2(1).wsize.get
    io.axi.awvalid    := false.B
    io.axi.wdata      := io.l2(1).wdata.get
    io.axi.wlast      := false.B
    io.axi.wstrb      := io.l2(1).wstrb.get
    io.axi.wvalid     := false.B
    io.axi.bready     := false.B

    switch(wState){
        is(wIdle){
            // idle state
            wState := Mux(io.l2(1).wreq.get, wDaw, wIdle)
        }
        is(wDaw){
            // dcache aw shake hand state
            io.axi.awvalid := true.B
            wState         := Mux(io.axi.awready, wDw, wDaw)
        }
        is(wDw){
            // dcache write data state
            io.l2(1).wrsp.get := io.axi.wready
            io.axi.wvalid     := io.l2(1).wreq.get
            io.axi.wlast      := io.l2(1).wlast.get
            wState            := Mux(io.axi.wready && io.axi.wlast && io.axi.wvalid, wDb, wDw)
        }
        is(wDb){
            // dcache write response state
            io.axi.bready := true.B
            wState        := Mux(io.axi.bready && io.axi.bvalid, wIdle, wDb)
        }
    }
}