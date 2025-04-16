import spire.math.UInt
import chiseltest._

class Device {
    private val baseAddr = UInt(0xa0000000)
    private val uartAddr = baseAddr + UInt(0x00003f8)


    def read(addr: UInt): UInt = {
        UInt(0)
    }

    def uart_write(data: Byte): Unit = {
        // printf("%c", data)
        print(data.toChar)
        // Console.flush()
    }

    def write(addr: UInt, data: Byte): Unit = {
        if (addr === uartAddr) {
            uart_write(data)
        }
    }

}