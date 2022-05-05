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
  val muldiv = Output(Bool())
  val load = Output(Bool())
  val store = Output(Bool())
  val subalu = Output(Bool())
  val branch = Output(Bool())

}
trait hasBypassConst{
  def E0BypassPort = 12  // 0->9: alu1,alu0,e21,e20,e31,e30,mem3,mdu3,subalu1,subalu0,e51,e50
  def E2BypassPort = 2  // 0->1: e51,e50
  def E3BypassPort = 5 // 0->8 : e30,e40,e41,e50,e51
}
class BypassCtl extends Bundle with hasBypassConst {
  val rs1bypasse0 = Output(Vec(E0BypassPort,Bool()))
  val rs2bypasse0 = Output(Vec(E0BypassPort,Bool()))
  val rs1bypasse2 = Output(Vec(E2BypassPort,Bool()))
  val rs2bypasse2 = Output(Vec(E2BypassPort,Bool()))
  val rs1bypasse3 = Output(Vec(E3BypassPort,Bool()))
  val rs2bypasse3 = Output(Vec(E3BypassPort,Bool()))
}
class BypassPkt extends Bundle {
  val decodePkt = new decodePkt
  val BypassCtl = new BypassCtl
}
class rsrdPkt extends Bundle{
  val rs1Valid = Output(Bool())
  val rs1Pc = Output(Bool())
  val rs2Valid = Output(Bool())
  val rs2Imm = Output(Bool())
  val rdValid = Output(Bool())
  val rs1 = Output(UInt(5.W))
  val rs2 = Output(UInt(5.W))
  val rd = Output(UInt(5.W))
}
class FuPkt extends NutCoreBundle {
  val rs1 = Output(UInt(64.W))
  val rs2 = Output(UInt(64.W))
  val rd = Output(UInt(64.W))
  val fuOpType = Output(UInt(7.W))
  val offset = Output(UInt(64.W))
  val bpuUpdateReq = new BPUUpdateReq
  val alu2pmu = new ALU2PMUIO
  //for difftest
  val instr = Output(UInt(64.W))
  //for redirect
  val pc = Output(UInt(VAddrBits.W))
  val pnpc = Output(UInt(VAddrBits.W))
  val brIdx = Output(UInt(4.W))
  val isRVC = Output(Bool())  //not use
  val isBranch = Output(Bool()) // not use
  val debugInfo = new rsrdPkt
  val csrInst = Output(Bool())
}

