import chisel3._
import circt.stage.ChiselStage
import chisel3.stage.ChiselOption
object Main extends App {
    var firtoolOptions = Array(
        "-disable-all-randomization", 
        "-strip-debug-info",
        "-strip-fir-debug-info",
        "-O=release",
        "--ignore-read-enable-mem",
        "--lower-memories",
        "--lowering-options=noAlwaysComb, disallowPackedArrays, disallowLocalVariables, explicitBitcast, disallowMuxInlining, disallowExpressionInliningInPorts",
        "-o=verilog/",
        "-split-verilog",
)
    val isSim = Option(System.getenv("BUILD_MODE")).getOrElse("SYNC") != "SYNC"
    println(s"isSim: $isSim")
    ChiselStage.emitSystemVerilogFile(
        new CPU(isSim),
        Array("-td", "build/"),
        firtoolOpts = firtoolOptions,
    )
}