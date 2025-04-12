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
            c.clock.setTimeout(0)
            println("开始仿真")
            val emu = new Emulator()
            val imgPath = Option(System.getenv("IMG"))
            imgPath match {
                case Some(path) => emu.memInit(path)
                case None => 
                    println("没有提供镜像文件路径，使用默认镜像")
                    emu.memInit(null)
            }
            breakable {
                while(true){
                    val end = emu.step(c)
                    if(end == 0){
                        emu.printIRing()
                        println(Console.GREEN + "程序正常退出" + Console.RESET)
                        break()
                    } else if (end == -1){
                        emu.printIRing()
                        println(Console.RED + "程序异常退出" + Console.RESET)
                        throw new Exception("程序异常退出")
                    } else if (end == -2){
                        emu.printIRing()
                        println(Console.YELLOW + "Difftest失败" + Console.RESET)
                        throw new Exception("Difftest失败")
                    } else if (end == -3){
                        emu.printIRing()
                        println(Console.YELLOW + "CPU过久没有提交指令" + Console.RESET)
                        throw new Exception("CPU过久没有提交指令")
                    }
                }
            }
        }

    }
}


