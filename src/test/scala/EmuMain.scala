import chisel3._
import chiseltest._
import chiseltest.simulator.VerilatorFlags
import org.scalatest.flatspec.AnyFlatSpec
import scala.util.control.Breaks._
import chiseltest.internal.CachingAnnotation

class EmuMain extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "EmuMain"
    println("开始编译")
    it should "pass" in {
        test(new CPU)
        .withAnnotations(Seq(
            CachingAnnotation,
            VerilatorBackendAnnotation, 
            WriteVcdAnnotation, 
            VerilatorFlags(Seq(
                "-j", "16", 
                "--no-MMD", "--cc", "--exe"
            ))))
        { c =>
            c.clock.setTimeout(10000)
            println("开始仿真")
            val emu = new Emulator()
            val imgPath = Option(System.getenv("IMG"))
            imgPath match {
                case Some(path) => emu.mem_init(path)
                case None => 
                    println("没有提供镜像文件路径，使用默认镜像")
                    emu.mem_init(null)
            }
            breakable {
                while(true){
                    val end = emu.step(c)
                    if(end == 0){
                        println(Console.GREEN + "程序正常退出" + Console.RESET)
                        break()
                    } else if (end == -1){
                        println(Console.RED + "程序异常退出" + Console.RESET)
                        break()
                    } else if (end == -2){
                        println(Console.YELLOW + "Difftest失败" + Console.RESET)
                        break()
                    }
                }
            }
            println("指令环缓冲区:")
            val iring = emu.iring.toArray
            for (i <- 0 until iring.length) {
                // 十六进制补充前导0到32位
                println(f"${iring(i)._1.toInt.toHexString.reverse.padTo(8, '0').reverse.mkString}: ${iring(i)._2.toInt.toHexString.reverse.padTo(8, '0').reverse.mkString}")
            }
        }

    }
}


