import chisel3._
import chisel3.util._
import ZirconUtil._
import ZirconConfig.Fetch._
import ZirconConfig.JumpOp._

/* Pre-Decoder: 
    In order to shorten the frontend pipleline, we need to decode the instruction
    in the previous stage.
    However, because the decode may be a critical path by connected behind the icache, 
    we can only decode something that will use in rename stage or the branch predictor
    in this stage.
*/
class RegisterInfo extends Bundle {
    val rdVld = Bool()
    val rd    = UInt(5.W)
    val rjVld = Bool()
    val rj    = UInt(5.W)
    val rkVld = Bool()
    val rk    = UInt(5.W)
}
class PreDecoderIO extends Bundle {
    val instPkg    = Input(new FrontendPackage)
    val rinfo      = Output(new RegisterInfo)
    val npc        = Flipped(new NPCPreDecodeIO)
    val predOffset = Output(UInt(32.W))
    val predType   = Output(UInt(2.W))
    // val praData    = Input(UInt(32.W))
    val isBr       = Output(Bool())
    val jumpEn     = Output(Bool())
    val returnOffset = Input(UInt(32.W))
}

class PreDecoder extends Module {
    val io       = IO(new PreDecoderIO)
    val instPkg  = io.instPkg
    val predInfo = instPkg.predInfo
    val inst     = instPkg.inst

    // rd
    val rdVld = inst(3, 0) === 0x3.U && !(
        inst(6, 4) === 0x6.U || // store
        inst(6, 4) === 0x2.U    // branch
    ) || inst(2, 0) === 0x7.U

    val rd = inst(11, 7)
    io.rinfo.rdVld := Mux(rd === 0.U, false.B, rdVld)
    io.rinfo.rd := Mux(rdVld, rd, 0.U)

    // rj
    val rjVld = (
        inst(2, 0) === 0x3.U && !(inst(6, 3) === 0xe.U && inst(14)) ||
        inst(6, 0) === 0x67.U || // jalr
        inst(3, 0) === 0xf.U
    )

    val rj = inst(19, 15)
    io.rinfo.rjVld := Mux(rj === 0.U, false.B, rjVld)
    io.rinfo.rj := Mux(rjVld, rj, 0.U)

    // rk
    val rkVld = (
        inst(6, 0) === 0x63.U || // branch
        inst(6, 0) === 0x23.U || // store
        inst(6, 0) === 0x33.U || // reg-reg
        inst(6, 0) === 0x2f.U // atom
    )
    val rk = inst(24, 20)
    io.rinfo.rkVld := Mux(rk === 0.U, false.B, rkVld)
    io.rinfo.rk := Mux(rkVld, rk, 0.U)

    /* branch */
    // jalr: not care
    // jal: must jump, and jumpTgt must be pc + imm
    // branch: if not pred, static predict; if pred, jumpTgt must be pc + predOffset
    // if not jump or branch, shouldn't predict jump

    val isJalr = inst(6, 0) === 0x67.U
    val isJal  = inst(6, 0) === 0x6f.U
    val isBr   = inst(6, 0) === 0x63.U
    val isnJ   = !(isJalr || isJal || isBr)

    val imm = Mux1H(Seq(
        isJal  -> SE(inst(31) ## inst(19, 12) ## inst(20) ## inst(30, 21) ## 0.U(1.W)),
        // isJalr -> (instPkg.pc + predInfo.offset),
        isBr   -> SE(inst(31) ## inst(7) ## inst(30, 25) ## inst(11, 8) ## 0.U(1.W))
    ))

    io.npc.flush := Mux1H(Seq(
        isnJ   -> (predInfo.offset =/= 4.U), // isn't jump: predictor must be wrong if it predicts jump
        isJal  -> (predInfo.offset =/= imm), 
        // isJalr -> !(predInfo.vld && rj =/= 1.U), // TEMP: only fix return or not valid
        isJalr -> (!predInfo.vld),
        isBr   -> Mux(predInfo.vld, predInfo.offset =/= Mux(predInfo.jumpEn, imm, 4.U), imm(31))
    )) && io.instPkg.valid

    // if the rj is 1, the jalr is a return, so we need to add 4 to the predicted offset
    // if the rj is not 1, the jalr is a call or branch, so we need to add pc the predicted offset
    val jalrAdderRes = BLevelPAdder32(Mux(predInfo.vld, Mux(rj =/= 1.U, instPkg.pc, 4.U), 4.U), Mux(predInfo.vld, predInfo.offset, io.returnOffset << 2), 0.U).io.res
    io.npc.jumpOffset := Mux(isnJ, 4.U, Mux(isJalr, io.returnOffset << 2, imm))
    io.npc.pc         := Mux(isJalr, 4.U, instPkg.pc)
    io.predOffset := Mux1H(Seq(
        isnJ   -> 4.U,
        isJal  -> imm,
        isJalr -> jalrAdderRes,
        isBr   -> Mux(predInfo.vld, Mux(predInfo.jumpEn, imm, 4.U), Mux(imm(31), imm, 4.U))
    ))
    io.isBr := !isnJ
    io.jumpEn := Mux1H(Seq(
        isnJ   -> false.B,
        isJal  -> true.B,
        isJalr -> true.B,
        isBr   -> Mux(predInfo.vld, predInfo.jumpEn, imm(31))
    ))
    io.predType := Mux1H(Seq(
        isnJ   -> NOP,
        isJal  -> Mux(rd =/= 0.U, CALL, BR),
        isJalr -> Mux(rj === 1.U, RET, Mux(rd =/= 0.U, CALL, BR)),
        isBr   -> BR
    ))

}

class PreDecodersIO extends Bundle {
    val instPkg    = Input(Vec(nfch, new FrontendPackage))
    val rinfo      = Output(Vec(nfch, new RegisterInfo))
    val validMask  = Output(Vec(nfch, Bool()))
    val npc        = Flipped(new NPCPreDecodeIO)
    val pr         = Flipped(new PredictPreDecodeIO)
    val predOffset = Vec(nfch, Output(UInt(32.W)))
    // val praData    = Input(UInt(32.W))
}

class PreDecoders extends Module {
    val io = IO(new PreDecodersIO)
    val pds = VecInit.fill(nfch)(Module(new PreDecoder).io)
    for (i <- 0 until nfch) {
        pds(i).instPkg     := io.instPkg(i)
        io.rinfo(i)        := pds(i).rinfo
        // pds(i).praData     := io.praData
        pds(i).returnOffset := io.pr.ras.returnOffset
        io.validMask(i)    := (if(i == 0) true.B else !pds.map(_.npc.flush).take(i).reduce(_ || _)) && io.instPkg(i).valid
        io.pr.gs.isBr(i)   := pds(i).isBr && io.validMask(i)
        io.pr.gs.jumpEn(i) := pds(i).jumpEn && io.validMask(i)
    }
    io.npc.flush      := pds.map(_.npc.flush).reduce(_ || _)
    io.npc.jumpOffset := Mux1H(Log2OHRev(pds.map(_.npc.flush)), pds.map(_.npc.jumpOffset))
    io.npc.pc         := Mux1H(Log2OHRev(pds.map(_.npc.flush)), pds.map(_.npc.pc))
    io.pr.gs.flush    := io.npc.flush
    io.predOffset     := pds.map(_.predOffset)
    io.pr.ras.flush   := io.npc.flush
    io.pr.ras.predType := Mux1H(Log2OH(io.validMask), pds.map(_.predType))
    io.pr.ras.pc       := Mux1H(Log2OH(io.validMask), pds.map(_.npc.pc))
}