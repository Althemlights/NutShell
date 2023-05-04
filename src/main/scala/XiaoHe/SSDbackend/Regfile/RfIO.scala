package SSDbackend
import chisel3._

class RfIO extends Bundle {
  val wen = Input(Vec(2, Bool()))
  val raddr = Input(Vec(4, UInt(5.W)))
  val waddr = Input(Vec(2, UInt(5.W)))
  val rdata = Output(Vec(4, UInt(64.W)))
  val wdata = Input(Vec(2, UInt(64.W)))
}
