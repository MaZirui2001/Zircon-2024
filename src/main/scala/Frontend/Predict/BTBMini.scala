import chisel3._
import chisel3.util._
import ZirconConfig.Predict.BTBMini._
import ZirconConfig.Fetch._

class BTBMiniEntry extends Bundle {
    val imm      = UInt(19.W) // 21 imm - [1:0]
    val predType = UInt(2.W)

    def apply(imm: UInt, predType: UInt): BTBMiniEntry = {
        val entry = Wire(new BTBMiniEntry)
        entry.imm      := imm
        entry.predType := predType
        // entry.valid    := valid
        entry
    }
}

class BTBMiniTagEntry extends Bundle {
    val tag      = UInt(tagWidth.W)
    val valid    = UInt(nfch.W)

    def apply(tag: UInt, valid: UInt): BTBMiniTagEntry = {
        val entry = Wire(new BTBMiniTagEntry)
        entry.tag    := tag
        entry.valid  := valid
        entry
    }
}
class BTBMiniFCIO extends Bundle {
    val pc      = Input(UInt(32.W))
    val rData   = Output(Vec(nfch, new BTBMiniEntry))
    val rValid  = Output(Vec(nfch, Bool()))
}
class BTBMiniCommitIO extends Bundle {
    val pc       = Input(UInt(32.W))
    val jumpTgt  = Input(UInt(32.W))
    val predType = Input(UInt(2.W))
    val jumpEn   = Input(Bool())
}
class BTBMiniIO extends Bundle {
    val fc  = new BTBMiniFCIO
    val gs  = Flipped(new GShareBTBMiniIO)
    val cmt = new BTBMiniCommitIO
}

class BTBMini extends Module {
    val io  = IO(new BTBMiniIO)
    val btb = VecInit.fill(way)(
        Module(new AsyncRegRam(Vec(nfch, new BTBMiniEntry), sizePerBank, 1, 1)).io
    )
    val btbTag = VecInit.fill(way)(
        Module(new AsyncRegRam(new BTBMiniTagEntry, sizePerBank, 1, 2)).io
    )
    val pht = RegInit(VecInit.fill(way)(
        VecInit.fill(sizePerBank)(VecInit.fill(nfch)(3.U(3.W)))
    ))

    /* Read */
    def bank(rIdx: UInt) = rIdx(bankWidth-1, 0)
    def idx(rIdx: UInt)  = rIdx(addrWidth-1, bankWidth)
    def tag(rIdx: UInt)  = rIdx(totalWidth-1, addrWidth)

    // random
    val rand = RegInit(0.U(2.W))
    rand := rand ^ 1.U

    /* write */
    // stage 1 : Read and judge if the target is in the BTB
    val cmtRAddr    = io.cmt.pc >> 2
    val cmtRIdx     = idx(cmtRAddr)
    btbTag.zipWithIndex.foreach{case (b, i) => b.raddr(1) := cmtRIdx}
    val cmtRTag    = btbTag.zipWithIndex.map{ case (b, i) => Mux(b.wen(0).orR && b.waddr(0) === cmtRIdx, b.wdata(0).tag, b.rdata(1).tag)}
    val cmtRValid  = VecInit(btbTag.zipWithIndex.map{ case (b, i) => Mux(b.wen(0).orR && b.waddr(0) === cmtRIdx, b.wdata(0).valid, b.rdata(1).valid)})
    // if one of the target is the hit, then the target is in the BTB
    val cmtRHit    = VecInit.tabulate(way){i => 
        cmtRValid(i).orR && tag(cmtRAddr) === cmtRTag(i)
    }
    //  Register the input
    val cmtJumpTgt  = ShiftRegister(io.cmt.jumpTgt >> 2, 1, 0.U, true.B)
    val cmtPC       = ShiftRegister(io.cmt.pc >> 2, 1, 0.U, true.B)
    val cmtPredType = ShiftRegister(io.cmt.predType, 1, 0.U, true.B)
    val cmtJumpEn   = ShiftRegister(io.cmt.jumpEn, 1, false.B, true.B)
    val cmtWHit     = ShiftRegister(cmtRHit, 1, VecInit.fill(way)(false.B), true.B)
    val cmtWValid   = ShiftRegister(cmtRValid, 1, VecInit.fill(way)(0.U(nfch.W)), true.B)

    // stage 2: write to the BTB
    // only save the imm
    val cmtWImm     = BLevelPAdder32(cmtJumpTgt, ~cmtPC, 1.U).io.res
    btb.zipWithIndex.foreach{case (b, i) => 
        b.waddr(0) := idx(cmtPC)
        b.wdata(0) := VecInit.fill(nfch)((new BTBMiniEntry)(cmtWImm.take(19), cmtPredType))
        b.wen(0)   := Mux(cmtPredType === 0.U, 0.U, Mux(Mux(cmtWHit.reduce(_ || _), cmtWHit(i), rand === i.U), UIntToOH(bank(cmtPC)), 0.U))
    }
    btbTag.zipWithIndex.foreach{case (b, i) => 
        b.waddr(0) := idx(cmtPC)
        b.wdata(0) := (new BTBMiniTagEntry)(tag(cmtPC), Mux1H(PriorityEncoderOH(cmtWHit), cmtWValid) | UIntToOH(bank(cmtPC)))
        b.wen(0)   := Mux(cmtPredType === 0.U, false.B, Mux(cmtWHit.reduce(_ || _), cmtWHit(i), rand === i.U))
    }
    pht.zipWithIndex.foreach{ case(p, i) =>
        when(cmtWHit.reduce(_ || _)){
            // if the target is in the BTB
            when(cmtWHit(i)){
                p(idx(cmtPC)).zipWithIndex.foreach{ case (p, j) =>
                    when(j.U === bank(cmtPC) && cmtPredType =/= 0.U){
                        // step 1: update the aimed pht
                        p := Mux(cmtJumpEn, p + (p =/= 7.U), p - (p =/= 0.U))
                    }.elsewhen(j.U < bank(cmtPC) && cmtPredType =/= 0.U){
                        // step 2: substract the other pht in front of the aimed pht with the same bank
                        p := Mux(cmtJumpEn, p - (p =/= 0.U), p)
                    }
                }
            }
        }.otherwise{
            // if the target is not in the BTB
            when(cmtPredType =/= 0.U){
                p(idx(cmtPC))(bank(cmtPC)) := Mux(rand === i.U, Mux(cmtJumpEn, 5.U, 2.U), p(idx(cmtPC))(bank(cmtPC)))
            }
        }
    }

    /* read */
    val rIdx = io.fc.pc >> 2
    btb.foreach{ _.raddr(0) := idx(rIdx) }
    btbTag.foreach{ _.raddr(0) := idx(rIdx) }
    val rHit    = VecInit(btbTag.map{ btag => btag.rdata(0).valid.orR && tag(rIdx) === btag.rdata(0).tag })
    val rData   = Mux1H(PriorityEncoderOH(rHit), btb.map(_.rdata(0)))
    // rValid: the target is in the BTB
    val rValid  = Mux1H(PriorityEncoderOH(rHit), VecInit(btbTag.map{ btag => btag.rdata(0).valid}))

    // shift: to cope with the pc that is not aligned to 16 bytes
    io.fc.rData.zipWithIndex.foreach{ case(r, i) => 
        // only when the gshare predicts a jump AND the target is in the block AND the target is valid, then use the BTB prediction   
        r.imm := (VecInit(rData.map{ case r => Mux(io.gs.jumpEnPredict(i) && rHit.reduce(_||_) && rValid(i), r.imm, 1.U) }).asUInt >> (19*i)).asTypeOf(Vec(nfch, UInt(19.W)))(bank(rIdx))
        r.predType := (VecInit(rData.map{ case r => r.predType }).asUInt >> (2*i)).asTypeOf(Vec(nfch, UInt(2.W)))(bank(rIdx))
    }
    io.fc.rValid.zipWithIndex.foreach{ case(r, i) => 
        r := (rValid >> i).asTypeOf(Vec(nfch, Bool()))(bank(rIdx))
    }
    io.gs.isBr := io.fc.rData.zip(io.fc.rValid).map{ case (r, v) => r.predType =/= 0.U && v }
    // phtData: the pht data of the target
    val phtData = VecInit(pht(PriorityEncoder(rHit))(idx(rIdx)).map{ case c => c(2)})
    // jumpCandidate: the jump candidate of the target, MUST be a one-hot vector
    io.gs.jumpCandidate := PriorityEncoderOH(phtData.asUInt >> bank(rIdx)).asBools

}
