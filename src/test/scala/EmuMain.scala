import chisel3._
import chiseltest._
import chiseltest.simulator.VerilatorFlags
import org.scalatest.flatspec.AnyFlatSpec
import scala.util.control.Breaks._
import chiseltest.internal.CachingAnnotation
import firrtl2.options.TargetDirAnnotation
import java.nio.file.{Paths, Path}
import chiseltest.simulator.PlusArgsAnnotation


class EmuMain extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "EmuMain"
    println("开始编译")
    it should "pass" in {
        val testDir = Option(System.getenv("TEST_DIR")).getOrElse("test_run_dir")
        val pwd = System.getProperty("user.dir")
        val relativeTestDir = Paths.get(pwd).relativize(Paths.get(testDir)).toString
        val testDirPath = Paths.get(testDir)
        if (!java.nio.file.Files.exists(testDirPath)) {
            java.nio.file.Files.createDirectories(testDirPath)
        }
        val isSim = Option(System.getenv("BUILD_MODE")).getOrElse("SYNC") != "SYNC"
        println(s"isSim: $isSim")
        test(new CPU(isSim))
        .withAnnotations(Seq(
            CachingAnnotation,
            VerilatorBackendAnnotation, 
            // WriteFstAnnotation,
            TargetDirAnnotation(relativeTestDir),
            VerilatorFlags(Seq(
                "-j", "16", 
                "--no-MMD", "--cc", "--exe",
                "--threads", "2",
            ))))
        { c =>
            c.clock.setTimeout(0)
            println("开始仿真")
            val emu = new Emulator()
            // 解析IMG参数
            val imgPath = Option(System.getenv("IMG"))
            imgPath match {
                case Some(path) => emu.memInit(path)
                case None => 
                    println("没有提供镜像文件路径，使用默认镜像")
                    emu.memInit(null)
            }
            // 通过IMG参数解析文件名（文件名是.前，最后一个/后）
            val imgName = imgPath.getOrElse("default").split("/").last.split("\\.").head
            emu.statisticInit(testDir, imgName)

            breakable {
                while(true){
                    val end = emu.step(c, -1)
                    if(end == 0){
                        emu.printStatistic()
                        println(Console.GREEN + "程序正常退出" + Console.RESET)
                        break()
                    } else if (end == -1){
                        emu.printStatistic()
                        println(Console.RED + "程序异常退出" + Console.RESET)
                        throw new Exception("程序异常退出")
                    } else if (end == -2){
                        emu.printStatistic()
                        println(Console.YELLOW + "Difftest失败" + Console.RESET)
                        throw new Exception("Difftest失败")
                    } else if (end == -3){
                        emu.printStatistic()
                        println(Console.YELLOW + "CPU过久没有提交指令" + Console.RESET)
                        throw new Exception("CPU过久没有提交指令")
                    }
                }
            }
        }
    }
}


