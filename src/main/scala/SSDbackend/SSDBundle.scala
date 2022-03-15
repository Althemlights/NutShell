package SSDbackend

import chisel3._
import nutcore._

class StallFlushIO extends Bundle{
  //stall point : (0,2) -> i0pipestall, i1pipestall, memsstall
  val stall = Output(Vec(3,Bool()))
  val flush = Output(Vec(10,Bool()))
}
class decodePkt extends  NutCoreBundle{
  val rd = Output(UInt(5.W))
  val rdvalid = Output(Bool())
  val alu = Output(Bool())
  val mul = Output(Bool())
  val div = Output(Bool())
  val load = Output(Bool())
  val store = Output(Bool())
  val subalu = Output(Bool())

}
trait hasBypassConst{
  def E0BypaaPort = 12  // 0->9: alu1,alu0,e21,e20,e31,e30,mem3,mdu3,subalu1,subalu0,e51,e50
  def E2BypaaPort = 2  // 0->1: e51,e50
  def E3BypaaPort = 5 // 0->8 : e30,e40,e41,e50,e51
}
class BypassCtl extends Bundle with hasBypassConst {
  val rs1bypasse0 = Output(Vec(E0BypaaPort,Bool()))
  val rs2bypasse0 = Output(Vec(E0BypaaPort,Bool()))
  val rs1bypasse2 = Output(Vec(E2BypaaPort,Bool()))
  val rs2bypasse2 = Output(Vec(E2BypaaPort,Bool()))
  val rs1bypasse3 = Output(Vec(E3BypaaPort,Bool()))
  val rs2bypasse3 = Output(Vec(E3BypaaPort,Bool()))
}
class BypassPkt extends Bundle {
  val decodePkt = new decodePkt
  val BypassCtl = new BypassCtl
}
class FuPkt extends NutCoreBundle {
  val rs1 = Output(UInt(64.W))
  val rs2 = Output(UInt(64.W))
  val rd = Output(UInt(64.W))
  val fuOpType = Output(UInt(7.W))
  val offset = Output(UInt(64.W))
  //for difftest
  val instr = Output(UInt(64.W))
  //for redirect
  val pc = Output(UInt(VAddrBits.W))
  val pnpc = Output(UInt(VAddrBits.W))
  val brIdx = Output(UInt(4.W))
  val isRVC = Output(Bool())  //not use
  val isBranch = Output(Bool()) // not use
}

