import chisel3._
import chisel3.util._


object Adder {
    // select a kind of Adder
    class Adder_IO(n: Int) extends Bundle{
        val src1 = Input(UInt(n.W))
        val src2 = Input(UInt(n.W))
        val cin  = Input(UInt(1.W))
        val res  = Output(UInt(n.W))
        val cout = Output(UInt(1.W))
    }

    def BLevel_Carry4(p: UInt, g: UInt, c: UInt): (UInt, UInt, UInt) = {
        assert(p.getWidth == 4, "p must be 4 bits wide")
        assert(g.getWidth == 4, "g must be 4 bits wide")
        assert(c.getWidth == 1, "c must be 1 bits wide")
        val pn = p.andR
        val gn = g(3) | (p(3) & g(2)) | (p(3) & p(2) & g(1)) | (p(3) & p(2) & p(1) & g(0))
        val cn = Wire(Vec(4, UInt(1.W)))
        cn(0) := g(0) | (p(0) & c)
        cn(1) := g(1) | (p(1) & g(0)) | (p(1) & p(0) & c)
        cn(2) := g(2) | (p(2) & g(1)) | (p(2) & p(1) & g(0)) | (p(2) & p(1) & p(0) & c)
        cn(3) := gn | (pn & c)

        (pn, gn, cn.asUInt)
    }

    class BLevel_PAdder32 extends Module{
        val io = IO(new Adder_IO(32))
        val pi = io.src1 | io.src2;
        val gi = io.src1 & io.src2;

        val p = Wire(MixedVec(Vec(8, UInt(1.W)), Vec(2, UInt(1.W))))
        val g = Wire(MixedVec(Vec(8, UInt(1.W)), Vec(2, UInt(1.W))))
        val c = Wire(MixedVec(Vec(8, UInt(4.W)), Vec(2, UInt(4.W)), Vec(1, UInt(4.W))))

        for (i <- 0 until 8){
            val cin = if(i == 0) io.cin else if(i == 4) c(2).asUInt(0) else c(1).asUInt(i-1)
            val (p0n, g0n, c0n) = BLevel_Carry4(pi(i*4+3, i*4), gi(i*4+3, i*4), cin)
            p(0)(i) := p0n
            g(0)(i) := g0n
            c(0)(i) := c0n
        }
        for (i <- 0 until 2){
            val cin = if(i == 0) io.cin else c(2).asUInt(i-1)
            val (p1n, g1n, c1n) = BLevel_Carry4(p(0).asUInt(i*4+3, i*4), g(0).asUInt(i*4+3, i*4), cin)
            p(1)(i) := p1n
            g(1)(i) := g1n
            c(1)(i) := c1n
        }

        val (p2n, g2n, c2n) = BLevel_Carry4(0.U(2.W) ## p(1).asUInt, 0.U(2.W) ## g(1).asUInt, io.cin)
        c(2)(0) := c2n

        io.res := io.src1 ^ io.src2 ^ (c(0).asUInt(30, 0) ## io.cin)
        io.cout := c(0).asUInt(31)
    }

    class BLevel_PAdder64 extends Module{
        val io = IO(new Adder_IO(64))
        val pi = io.src1 | io.src2;
        val gi = io.src1 & io.src2;

        val p = Wire(MixedVec(Vec(16, UInt(1.W)), Vec(4, UInt(1.W))))
        val g = Wire(MixedVec(Vec(16, UInt(1.W)), Vec(4, UInt(1.W))))
        val c = Wire(MixedVec(Vec(16, UInt(4.W)), Vec(4, UInt(4.W)), Vec(1, UInt(4.W))))

        for (i <- 0 until 16){
            val cin = if(i == 0) io.cin else if(i % 4 == 0) c(2).asUInt(i / 4 - 1) else c(1).asUInt(i-1)
            val (p0n, g0n, c0n) = BLevel_Carry4(pi(i*4+3, i*4), gi(i*4+3, i*4), cin)
            p(0)(i) := p0n
            g(0)(i) := g0n
            c(0)(i) := c0n
        }
        for (i <- 0 until 4){
            val cin = if(i == 0) io.cin else c(2).asUInt(i-1)
            val (p1n, g1n, c1n) = BLevel_Carry4(p(0).asUInt(i*4+3, i*4), g(0).asUInt(i*4+3, i*4), cin)
            p(1)(i) := p1n
            g(1)(i) := g1n
            c(1)(i) := c1n
        }

        val (p2n, g2n, c2n) = BLevel_Carry4(p(1).asUInt, g(1).asUInt, io.cin)
        c(2)(0) := c2n

        io.res := io.src1 ^ io.src2 ^ (c(0).asUInt(62, 0) ## io.cin)
        io.cout := c(0)(63)
    }
    class Xilinx_Adder(n: Int) extends Module{
        val io = IO(new Adder_IO(n))
        var c = io.cin
        val res = Wire(Vec(n/4, UInt(4.W)))
        for (i <- 0 until n){
            val a = io.src1(i)
            val b = io.src2(i)
            val (s, co) = Carry4(c, a, b)
            io.res(i) := s
            c = co
        }
        io.cout := c
    }

    object BLevel_PAdder32{
        def apply(src1: UInt, src2: UInt, cin: UInt): BLevel_PAdder32 = {
            val adder = Module(new BLevel_PAdder32)
            adder.io.src1 := src1
            adder.io.src2 := src2
            adder.io.cin := cin
            adder
        }
    }

    object BLevel_PAdder64{
        def apply(src1: UInt, src2: UInt, cin: UInt): BLevel_PAdder64 = {
            val adder = Module(new BLevel_PAdder64)
            adder.io.src1 := src1
            adder.io.src2 := src2
            adder.io.cin := cin
            adder
        }
    }

    object Xilinx_Adder{
        def apply(src1: UInt, src2: UInt, cin: UInt): Xilinx_Adder = {
            val adder = Module(new Xilinx_Adder(src1.getWidth))
            adder.io.src1 := src1
            adder.io.src2 := src2
            adder.io.cin := cin
            adder
        }
    }
    
}