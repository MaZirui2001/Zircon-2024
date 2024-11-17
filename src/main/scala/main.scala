import chisel3._
import circt.stage.ChiselStage
import chisel3.stage.ChiselOption
import Adder._
import Multiply._
object FIFOMain extends App {
    var firtool_options = Array("-disable-all-randomization", 
                                "-strip-debug-info",
                                "-strip-fir-debug-info",
                                "-O=release",
                                "--ignore-read-enable-mem",
                                "--lower-memories",
                                "--lowering-options=disallowLocalVariables, explicitBitcast, disallowMuxInlining, disallowExpressionInliningInPorts",
                                "-o=verilog/",
                                "-split-verilog",
                                )
    ChiselStage.emitSystemVerilogFile(
        new L2Cache_Test,
        Array("-td", "build/"),
        firtoolOpts = firtool_options,
    )
}