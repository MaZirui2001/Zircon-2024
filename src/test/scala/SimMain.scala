import chisel3._
import chiseltest._
import chiseltest.simulator.VerilatorFlags
import org.scalatest.flatspec.AnyFlatSpec
import scala.util.control.Breaks._


class SimMain extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "SimMain"
    it should "pass" in {
        val sim = new Simulator()
        // 这个文件的路径将会通过系统属性IMG_PATH获取，默认使用示例路径
        val IMG_PATH = System.getProperty("IMG")
        sim.mem_init(IMG_PATH)
        breakable {
            while (true) {
                val end = sim.step()
                if (end == 0) {
                    println(Console.GREEN + "程序正常退出" + Console.RESET)
                    break()
                } else if (end == -1) {
                    // 红色字
                    println(Console.RED + "程序异常退出" + Console.RESET)
                    break()
                } else {
                    // println("程序继续执行中...")
                }
            }
        }
        println("指令环缓冲区:")
        val iring = sim.iring_dump()
        for (i <- 0 until iring.length) {
            // 十六进制补充前导0到32位
            println(f"${iring(i)._1.toInt.toHexString.reverse.padTo(8, '0').reverse.mkString}: ${iring(i)._2.toInt.toHexString.reverse.padTo(8, '0').reverse.mkString}")
        }
    }
}