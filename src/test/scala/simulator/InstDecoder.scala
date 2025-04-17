import spire.math.UInt
import spire.math.SafeLong
class InstDecoder(rf: LogicRegFile, mem: Memory, fetch: Fetch, ir: InstRecorder) {
    private def Bits(bits: UInt, hi: Int, lo: Int): UInt = {
        (bits >> lo) & ~(UInt(-1) << (hi - lo + 1))
    }
    private def signExtend(imm: UInt, width: Int): UInt = {
        if (Bits(imm, width - 1, width - 1) == UInt(1)) {
            imm | (UInt(-1) << width)
        } else {
            imm
        }
    }
    private def zeroExtend(imm: UInt, width: Int): UInt = {
        imm
    }
    private def executeRType(instruction: UInt, funct3: UInt, funct7: UInt): Unit = {
        val rd      = Bits(instruction, 11, 7)
        val rs1     = Bits(instruction, 19, 15)
        val rs2     = Bits(instruction, 24, 20)
        val opcode  = Bits(instruction, 6, 0)

        val value1 = rf.read(rs1)
        val value2 = rf.read(rs2)
        val result = opcode.toInt match {
            case 0x33 => {
                funct7.toInt match {
                    case 0x00 => {
                        ir.addALUInsts(1)
                        funct3.toInt match{
                            case 0x0 => value1 + value2             // add
                            case 0x1 => value1 << value2.toInt     // sll
                            case 0x2 => if(value1.toInt < value2.toInt) UInt(1) else UInt(0) // slt
                            case 0x3 => if(value1 < value2) UInt(1) else UInt(0) // sltu
                            case 0x4 => value1 ^ value2            // xor
                            case 0x5 => value1 >> value2.toInt     // srl
                            case 0x6 => value1 | value2            // or
                            case 0x7 => value1 & value2            // and
                            // case _ => throw new Exception("Invalid funct3 of funct7 = 0 in R-type")
                        }
                    }
                    case 0x20 => {
                        ir.addALUInsts(1)
                        funct3.toInt match{
                            case 0x0 => value1 - value2            // sub
                            case 0x5 => UInt(value1.toInt >> value2.toInt)  // sra
                            // case _ => throw new Exception("Invalid funct3 of funct7 = 0x20 in R-type")
                        }
                    }
                    case 0x01 => {
                        funct3.toInt match{
                            case 0x0 => {ir.addMulInsts(1); UInt(value1.toInt * value2.toInt)}                                      // mul
                            case 0x1 => {ir.addMulInsts(1); UInt(((BigInt(value1.toInt) * BigInt(value2.toInt)) >> 32).toLong)}    // mulh
                            case 0x2 => {ir.addMulInsts(1); UInt(((BigInt(value1.toInt) * value2.toLong) >> 32).toLong)}          // mulhsu
                            case 0x3 => {ir.addMulInsts(1); UInt(((value1.toLong * value2.toLong) >> 32).toLong)}                // mulhu
                            case 0x4 => {ir.addDivInsts(1); (if (value2 == UInt(0)) UInt(-1) else UInt(value1.toInt / value2.toInt))} // div
                            case 0x5 => {ir.addDivInsts(1); (if (value2 == UInt(0)) UInt(-1) else value1 / value2)} // divu
                            case 0x6 => {ir.addDivInsts(1); (if (value2 == UInt(0)) value1 else UInt(value1.toInt % value2.toInt))} // rem
                            case 0x7 => {ir.addDivInsts(1); (if (value2 == UInt(0)) value1 else value1 % value2)} // remu
                            // case _ => throw new Exception("Invalid funct3 of funct7 = 0x01 in R-type")
                        }
                    }
                }
            }
        }
        
        rf.write(rd, result)
        fetch.setPC(fetch.getPC() + UInt(4))
    }
    private def executeIType(instruction: UInt, funct3: UInt): Unit = {
        val rd      = Bits(instruction, 11, 7)
        val rs1     = Bits(instruction, 19, 15)
        val imm     = signExtend(Bits(instruction, 31, 20), 12)
        val opcode  = Bits(instruction, 6, 0)
        val pc      = fetch.getPC()
        val value1  = rf.read(rs1)
        val result  = opcode.toInt match {
            case 0x13 => {
                ir.addALUInsts(1)
                fetch.setPC(pc + UInt(4))
                funct3.toInt match{
                    case 0x0 => value1 + imm // addi
                    case 0x1 => value1 << imm.toInt // slli
                    case 0x2 => if(value1.toInt < imm.toInt) UInt(1) else UInt(0) // slti
                    case 0x3 => if(value1 < imm) UInt(1) else UInt(0) // sltiu
                    case 0x4 => value1 ^ imm // xori
                    case 0x5 => if((instruction.toInt & 0x40000000) == 0) value1 >> (imm.toInt & 0x1F) else UInt(value1.toInt >> (imm.toInt & 0x1F)) // srli & srai
                    case 0x6 => value1 | imm // ori
                    case 0x7 => value1 & imm // andi
                    // case _ => throw new Exception("Invalid funct3 of I-type")
                }
            }
            case 0x03 => {
                ir.addLoadInsts(1)
                fetch.setPC(pc + UInt(4))
                mem.read(value1 + imm, funct3.toInt)
            }
            case 0x67 => {
                ir.addBranchInsts(1)
                fetch.setPC(value1 + imm)
                pc + UInt(4)
            }
        }
        rf.write(rd, result)
        
    }
    private def executeBType(instruction: UInt, funct3: UInt): Unit = {
        val rs1     = Bits(instruction, 19, 15)
        val rs2     = Bits(instruction, 24, 20)
        val imm     = signExtend(Bits(instruction, 31, 31) << 12 | (Bits(instruction, 7, 7) << 11) | (Bits(instruction, 30, 25) << 5) | (Bits(instruction, 11, 8) << 1), 13)
        val opcode  = Bits(instruction, 6, 0)
        val pc      = fetch.getPC()

        val value1  = rf.read(rs1)
        val value2  = rf.read(rs2)
        val npc     = (opcode.toInt match {
            case 0x63 => {
                ir.addBranchInsts(1)
                funct3.toInt match {
                    case 0x0 => if (value1 == value2) pc + imm else pc + UInt(4)
                    case 0x1 => if (value1 != value2) pc + imm else pc + UInt(4)
                    case 0x4 => if (value1.toInt < value2.toInt) pc + imm else pc + UInt(4)
                    case 0x5 => if (value1.toInt >= value2.toInt) pc + imm else pc + UInt(4)
                    case 0x6 => if (value1 < value2) pc + imm else pc + UInt(4)
                    case 0x7 => if (value1 >= value2) pc + imm else pc + UInt(4)
                }
            }
        })
        fetch.setPC(npc)
    }
    private def executeSType(instruction: UInt, funct3: UInt): Unit = {
        val rs1     = Bits(instruction, 19, 15)
        val rs2     = Bits(instruction, 24, 20)
        val imm     = signExtend(Bits(instruction, 31, 25) << 5 | Bits(instruction, 11, 7), 12)
        val value1  = rf.read(rs1)
        val value2  = rf.read(rs2)
        val opcode  = Bits(instruction, 6, 0)
        opcode.toInt match {
            case 0x23 => { mem.write(value1 + imm, value2, funct3.toInt) }
        }
        ir.addStoreInsts(1)
        fetch.setPC(fetch.getPC() + UInt(4))
    }
    private def executeUType(instruction: UInt): Unit = {
        val rd      = Bits(instruction, 11, 7)
        val imm     = Bits(instruction, 31, 12) << 12
        val opcode  = Bits(instruction, 6, 0)
        val pc      = fetch.getPC()
        val result  = opcode.toInt match {
            case 0x37 => { imm }
            case 0x17 => { pc + imm }
        }
        ir.addALUInsts(1)
        rf.write(rd, result)
        fetch.setPC(pc + UInt(4))
    }
    private def executeJType(instruction: UInt): Unit = {
        val rd      = Bits(instruction, 11, 7)
        val rs1     = Bits(instruction, 19, 15)
        val imm     = signExtend(Bits(instruction, 31, 31) << 20 | (Bits(instruction, 19, 12) << 12) | (Bits(instruction, 20, 20) << 11) | (Bits(instruction, 30, 21) << 1), 21)
        val opcode  = Bits(instruction, 6, 0)
        val pc      = fetch.getPC()
        val value1  = rf.read(rs1)
        val result  = opcode.toInt match {
            case 0x6F => { 
                ir.addBranchInsts(1)
                fetch.setPC(pc + imm)
                pc + UInt(4) 
            }
        }
        rf.write(rd, result)
    }
    def decodeAndExecute(instruction: UInt): Unit = {
        val opcode = Bits(instruction, 6, 0)
        val funct3 = Bits(instruction, 14, 12)
        val funct7 = Bits(instruction, 31, 25)
        opcode.toInt match {
            case 0x37 => { executeUType(instruction) }
            case 0x17 => { executeUType(instruction) }
            case 0x6F => { executeJType(instruction) }
            case 0x67 => { executeIType(instruction, funct3) }
            case 0x63 => { executeBType(instruction, funct3) }
            case 0x03 => { executeIType(instruction, funct3) }
            case 0x23 => { executeSType(instruction, funct3) }
            case 0x13 => { executeIType(instruction, funct3) }
            case 0x33 => { executeRType(instruction, funct3, funct7) }
        }
    }
}