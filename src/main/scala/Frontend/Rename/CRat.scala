import chisel3._
import chisel3.util._
import CPU_Config.RegisterFile._
import RegFile_Utils.WF_Read
import Zircon_Util._

class Rat_T extends Bundle{
    val lr          = UInt(log2Ceil(nlreg).W)
    val vld         = Bool()
    val free        = Bool()
}
object Rat_T{
    def apply(lr: UInt, vld: Bool, free: Bool): Rat_T = {
        val r = Wire(new Rat_T)
        r.lr := lr
        r.vld := vld
        r.free := free
        r
    }
}
object RegFile_Utils{
    def WF_Read[T <: Data](rdata: T, ridx: UInt, widx: Vec[UInt], wdata: Vec[T], wen: Vec[Bool]): T = {
        val n = wdata.size
        val whit = VecInit.tabulate(n)(i => ridx === widx(i) && wen(i))
        Mux(whit.asUInt.orR, Mux1H(whit, wdata), rdata)
    }
}
// dn: decode width, iw: issue width
class CRat_IO(dw: Int, iw: Int) extends Bundle{
    val rj          = Input(Vec(dw, UInt(log2Ceil(nlreg).W)))
    val rk          = Input(Vec(dw, UInt(log2Ceil(nlreg).W)))
    val rd          = Input(Vec(dw, UInt(log2Ceil(nlreg).W)))
    val rd_vld      = Input(Vec(dw, Bool()))
    val prd         = Input(Vec(dw, UInt(log2Ceil(npreg).W)))
    val prj         = Output(Vec(dw, UInt(log2Ceil(npreg).W)))
    val prk         = Output(Vec(dw, UInt(log2Ceil(npreg).W)))
    val pprd        = Output(Vec(dw, UInt(log2Ceil(npreg).W)))
    val prj_free    = Output(Vec(dw, Bool()))
    val prk_free    = Output(Vec(dw, Bool()))

    val wk_preg     = Input(Vec(iw, UInt(log2Ceil(npreg).W)))

    val rdrct       = Input(Bool())
    val arat_vld    = Input(Vec(npreg, Bool()))
}

class CRat(dw: Int, iw: Int) extends Module {
    val io = IO(new CRat_IO(dw, iw))

    val rat = RegInit(VecInit.tabulate(npreg)(i => Rat_T(0.U, false.B, true.B)))

    // redirect: flush all the entries iw the RAT
    when(io.rdrct){
        rat.zipWithIndex.map({case (r, i) => r.vld := io.arat_vld(i)})
        rat.map(_.free := true.B)
    }
    // write prd-rd mapping and remove pprd-rd mapping
    .otherwise{
        for(i <- 0 until dw){
            rat(io.prd(i)).lr := io.rd(i)
            when(io.rd_vld(i)){ 
                rat(io.prd(i)).vld := true.B
                rat(io.pprd(i)).vld := false.B
                rat(io.prd(i)).free := false.B
            }
        }
        for(i <- 0 until iw){
            rat(io.wk_preg(i)).free := true.B
        }
    }

    // read rj-prj, rk-prk and rd-pprd mapping
    for(i <- 0 until dw){
        val rj_hit_1h = VecInit.tabulate(npreg)(j => rat(j).vld && rat(j).lr === io.rj(i))
        val rk_hit_1h = VecInit.tabulate(npreg)(j => rat(j).vld && rat(j).lr === io.rk(i))
        val rd_hit_1h = VecInit.tabulate(npreg)(j => rat(j).vld && rat(j).lr === io.rd(i))
        io.prj(i) := OHToUInt(rj_hit_1h)
        io.prk(i) := OHToUInt(rk_hit_1h)
        io.pprd(i) := OHToUInt(rd_hit_1h)
        io.prj_free(i) := WF_Read(Mux1H(rj_hit_1h, rat.map(_.free)), io.prj(i), io.wk_preg, VecInit.fill(iw)(true.B), VecInit.fill(iw)(true.B))
        io.prk_free(i) := WF_Read(Mux1H(rk_hit_1h, rat.map(_.free)), io.prk(i), io.wk_preg, VecInit.fill(iw)(true.B), VecInit.fill(iw)(true.B))
    }

}