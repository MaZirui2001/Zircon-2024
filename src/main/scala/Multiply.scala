import chisel3._
import chisel3.util._
import Adder._
import MDU_Op._

object Multiply{
    class Multiply_IO extends Bundle {
        val src1 = Input(UInt(32.W))
        val src2 = Input(UInt(32.W))
        val op   = Input(MDU_Op())
        val res  = Output(UInt(32.W))
    }
    class Booth2 extends Module {
        val io = IO(new Bundle{
            val src1 = Input(UInt(64.W))
            val src2 = Input(UInt(3.W))
            val res  = Output(UInt(64.W))
            val add1 = Output(Bool())
        })
        assert(io.src2.getWidth == 3, "src2 must be 3 bits wide")
        val code = WireDefault(0.U(64.W))
        switch(io.src2){
            is(0.U){ code := 0.U }
            is(1.U){ code := io.src1 }
            is(2.U){ code := io.src1 }
            is(3.U){ code := io.src1(62, 0) ## 0.U(1.W) }
            is(4.U){ code := ~(io.src1(62, 0) ## 0.U(1.W)) }
            is(5.U){ code := ~io.src1 }
            is(6.U){ code := ~io.src1 }
            is(7.U){ code := 0.U }
        }
        val add_1 = io.src2(2) && !io.src2.andR // 4, 5, 6
        io.res := code
        io.add1 := add_1
    }

    def CSA(src1: UInt, src2: UInt, cin: UInt): (UInt, UInt) = {
        val sum = src1 ^ src2 ^ cin
        val cout = (src1 & src2) | (src1 & cin) | (src2 & cin)
        (sum, cout)
    }
    class Wallce_Tree17_Cin15 extends Module {
        val io = IO(new Bundle{
            val src = Input(UInt(17.W))
            val cin = Input(UInt(15.W))
            val sum = Output(UInt(1.W))
            val cout = Output(UInt(16.W))
        })
        val src = io.src
        val cin = io.cin
        assert(src.getWidth == 17, "src must be 17 bits wide")
        assert(cin.getWidth == 15, "cin must be 15 bits wide")
        // allocate the source of each level
        // 17 -> 12 -> 8 -> 6 -> 4 -> 3 -> 2
        val sum = Wire(MixedVec( // level 1-6
            Vec(6, UInt(1.W)),
            Vec(4, UInt(1.W)),
            Vec(2, UInt(1.W)),
            Vec(2, UInt(1.W)),
            Vec(1, UInt(1.W)),
            Vec(1, UInt(1.W)),
        ))
        val cout = Wire(MixedVec( 
            Vec(6, UInt(1.W)),
            Vec(4, UInt(1.W)),
            Vec(2, UInt(1.W)),
            Vec(2, UInt(1.W)),
            Vec(1, UInt(1.W)),
            Vec(1, UInt(1.W))
        ))
        val s1 = MixedVecInit(
            VecInit(src(1), src(4), src(7), src(10), src(13), src(16)),
            VecInit(cin(0), cin(3), sum(0)(0), sum(0)(3)),
            VecInit(cin(6), sum(1)(1)),
            VecInit(cin(8), cin(11)),
            VecInit(cin(12)),
            VecInit(cin(13))
        )
        val s2 = MixedVecInit(
            VecInit(src(0), src(3), src(6), src(9), src(12), src(15)),
            VecInit(cin(1), cin(4), sum(0)(1), sum(0)(4)),
            VecInit(cin(7), sum(1)(2)),
            VecInit(cin(9), sum(2)(0)),
            VecInit(sum(3)(0)),
            VecInit(cin(14))
        )
        val s3 = MixedVecInit(
            VecInit(0.U, src(2), src(5), src(8), src(11), src(14)),
            VecInit(cin(2), cin(5), sum(0)(2), sum(0)(5)),
            VecInit(sum(1)(0), sum(1)(3)),
            VecInit(cin(10), sum(2)(1)),
            VecInit(sum(3)(1)),
            VecInit(sum(4)(0)),
        )
        for(i <- 0 until 6){
            for(j <- 0 until sum(i).length){
                val (s, c) = CSA(s1(i)(j), s2(i)(j), s3(i)(j))
                sum(i)(j) := s
                cout(i)(j) := c
            }
        }
        // (sum(5).asUInt, cout.asUInt)
        io.sum := sum(5).asUInt
        io.cout := cout.asUInt
    }
    class Mul_Booth2_Wallce extends Module {
        val io = IO(new Multiply_IO)
        // extend
        val src1 = Mux(io.op === MULHU, 0.U(32.W) ## io.src1, (Fill(32, io.src1(31)) ## io.src1))
        val src2 = Mux(io.op === MULHU, 0.U(32.W) ## io.src2, (Fill(32, io.src2(31)) ## io.src2))

        // Booth2 encoder x 17
        val add1_booth = Wire(Vec(17, Bool()))
        val pp_booth = Wire(Vec(17, UInt(64.W)))
        add1_booth(0) := src2(0)
        pp_booth(0) := Mux(src2(0), ~src1, 0.U)
        for(i <- 1 until 17){
            val booth = Booth2(src1 << (2*i-1).U, src2(2*i, 2*i-2))
            add1_booth(i) := booth.io.add1
            pp_booth(i) := booth.io.res
        }

        // Wallace Tree
        val add1_wallce = RegNext(add1_booth)
        val pp_wallce = RegNext(pp_booth)
        val sum_wallce = Wire(Vec(64, UInt(1.W)))
        val cout_wallce = Wire(Vec(64, UInt(16.W)))

        for(i <- 0 until 64){
            val cin = (if(i == 0) VecInit(add1_wallce.take(15)).asUInt else cout_wallce(i-1)(14, 0))
            val wallce = Wallce_Tree17_Cin15(VecInit(pp_wallce.map(_(i))).asUInt, cin).io
            sum_wallce(i) := wallce.sum
            cout_wallce(i) := wallce.cout
        }

        // full adder
        val fadd_src1 = RegNext(sum_wallce.asUInt)
        val fadd_src2 = RegNext(VecInit(cout_wallce.map(_(15))).asUInt(62, 0) ## add1_wallce(15))
        val fadd_cin  = RegNext(add1_wallce(16).asUInt)

        val fadd = BLevel_PAdder64(fadd_src1, fadd_src2, fadd_cin)
        io.res := fadd.io.res(63, 32)
        switch(io.op){
            is(MUL){ io.res := fadd.io.res(31, 0) }
            is(MULH){ io.res := fadd.io.res(63, 32) }
            is(MULHU){ io.res := fadd.io.res(63, 32) }
        }
    }
    object Booth2{
        def apply(src1: UInt, src2: UInt): Booth2 = {
            val booth2 = Module(new Booth2)
            booth2.io.src1 := src1
            booth2.io.src2 := src2
            booth2
        }
    }
     object Wallce_Tree17_Cin15{
        def apply(src: UInt, cin: UInt): Wallce_Tree17_Cin15 = {
            val wallce = Module(new Wallce_Tree17_Cin15)
            wallce.io.src := src
            wallce.io.cin := cin
            wallce
        }
    }
}
