// import chisel3._
// import chiseltest._
// import org.scalatest.flatspec.AnyFlatSpec
// import scala.util._
// import ALUBROp._

// object ALUDut{
//     def ALU(src1: Int, src2: Int, op: Int): Int = {
//         // 用case语句在软件层面模拟一个ALU
//         op match{
//             case ADD => src1 + src2
//             case SUB => src1 - src2
//             case AND => src1 & src2
//             case OR => src1 | src2
//             case XOR => src1 ^ src2
//             case NOR => ~(src1 | src2)
//             case SLL => src1 << (src2 & 0x1F)
//             case SRL => src1 >>> (src2 & 0x1F)
//             case SRA => src1 >> (src2 & 0x1F)
//             case SLT => if(src1 < src2) 1 else 0
//             case SLTU => if((src1.toLong & 0xFFFFFFFFL) < (src2.toLong & 0xFFFFFFFFL)) 1 else 0
//             case LUI => src2
//         }
//     }
// }

// class ALUTest extends AnyFlatSpec with ChiselScalatestTester{
//     behavior of "ALU"
//     it should "pass" in {
//         test(new ALU)
//         // .withAnnotations(Seq(WriteVcdAnnotation))
//         { c =>
//             val values = ALUBROp.all
//             for (i <- 0 until 100) {
//                 val a = Random.nextLong(0xFFFFFFFFL)
//                 val b = Random.nextLong(0xFFFFFFFFL)
//                 val op = values((Random.nextInt(ALUBROp.all.length)))
//                 c.io.src1.poke(a.U)
//                 c.io.src2.poke(b.U)
//                 c.io.op.poke(op)
//                 c.clock.step(1)
//                 // 十六进制输出结果
//                 // println(i)
//                 val res = c.io.res.expect( ((ALUDut.ALU(a.toInt, b.toInt, op).toLong) & 0xFFFFFFFFL).U, // 十六进制：
//                     s"src1: 0x${a.toHexString}, src2: 0x${b.toHexString}, op: ${op}")

//             }
//         }
//     }
// }