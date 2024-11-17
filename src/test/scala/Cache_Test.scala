import chisel3._
import chisel3.util._
import scala.collection.mutable



case class Memory_Item(data: UInt, last_write: Array[Int])
case class AXI_Read_Config(araddr: UInt, arlen: UInt, arsize: UInt, arburst: UInt, state: Int)
case class AXI_Read_Item(arready: Bool, rdata: UInt, rvalid: Bool, rlast: Bool)
case class AXI_Write_Config(awaddr: UInt, awlen: UInt, awsize: UInt, awburst: UInt, wstrb: UInt, wlast: Bool, state: Int)
case class AXI_Write_Item(awready: Bool, wready: Bool, bvalid: Bool, bresp: UInt)



class AXI_Memory(depth: Int){
    private val mem: mutable.Map[UInt, Memory_Item] = mutable.Map()
    var readConfig: Option[AXI_Read_Config] = None
    var writeConfig: Option[AXI_Write_Config] = None
    def read(
        araddr: UInt, 
        arlen: UInt, 
        arsize: UInt, 
        arburst: UInt,
        arvalid: Bool,
        rready: Bool,
    ): AXI_Read_Item = {
    }
}