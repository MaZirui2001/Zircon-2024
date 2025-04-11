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
        "--lowering-options=disallowLocalVariables, disallowPackedArrays, explicitBitcast, disallowMuxInlining, disallowExpressionInliningInPorts",
        "-o=verilog/",
        "-split-verilog",
                                )
    ChiselStage.emitSystemVerilogFile(
        new CPU,
        Array("-td", "build/"),
        firtoolOpts = firtoolOptions,
    )
}