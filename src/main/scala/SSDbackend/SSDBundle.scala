package SSDbackend

import chisel3._
import nutcore._

class StallFlushIO extends Bundle{
  //stall point : (0,2) -> i0pipestall, i1pipestall, memsstall
  val stall = Output(Vec(3,Bool()))
  val flush = Output(Vec(10,Bool())) //10 + 2 (2 is for regfile invalid write)
  val invalid = Output(Vec(12,Bool()))
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
  val csr = Output(Bool())
  val skip = Output(Bool())
}
trait hasBypassConst{
  def E0BypassPort = 12  // 0->9: alu1,alu0,e21,e20,e31,e30,mem3,mdu3,subalu1,subalu0,e51,e50
  def E2BypassPort = 2   // 0->1: e51,e50
  def E3BypassPort = 5   // 0->8 : e30,e40,e41,e50,e51
  def E1StoreBypassPort = 4
  def E2StoreBypassPort = 6
}
class BypassCtl extends Bundle with hasBypassConst {
  val rs1bypasse0 = Output(Vec(E0BypassPort,Bool()))
  val rs2bypasse0 = Output(Vec(E0BypassPort,Bool()))
  val rs1bypasse2 = Output(Vec(E2BypassPort,Bool()))
  val rs2bypasse2 = Output(Vec(E2BypassPort,Bool()))
  val rs1bypasse3 = Output(Vec(E3BypassPort,Bool()))
  val rs2bypasse3 = Output(Vec(E3BypassPort,Bool()))
}
class LSUPipeBypassCtrl extends Bundle with hasBypassConst{
  val lsBypassCtrlE1 = Output(Vec(E1StoreBypassPort,Bool()))
  val storeBypassCtrlE2 = Output(Vec(E2StoreBypassPort,Bool()))
}
class LSUBypassCtrl extends Bundle with hasBypassConst{
  val lsBypassCtrli0E1 = Output(Vec(E1StoreBypassPort,Bool()))
  val lsBypassCtrli1E1 = Output(Vec(E1StoreBypassPort,Bool()))
  val storeBypassCtrlE2 = Output(Vec(E2StoreBypassPort,Bool()))
}
class StorePipeBypassPort extends  Bundle with hasBypassConst{
  val lsBypassPortE1 = Output(Vec(E1StoreBypassPort,UInt(64.W)))
  val storeBypassPortE2 = Output(Vec(E2StoreBypassPort,UInt(64.W)))
}
class BypassPkt extends Bundle {
  val decodePkt = new decodePkt
  val BypassCtl = new BypassCtl
  val lsuCtrl = new LSUPipeBypassCtrl
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
  val redirect = new RedirectIO
  //for difftest
  val instr = Output(UInt(32.W))
  //for redirect
  val pc = Output(UInt(VAddrBits.W))
  val pnpc = Output(UInt(VAddrBits.W))
  val brIdx = Output(UInt(4.W))
  val isRVC = Output(Bool())  //not use
  val isBranch = Output(Bool()) // not use
  val debugInfo = new rsrdPkt
  val csrInst = Output(Bool())
  //for SubALU
  val isSubALU = Output(Bool())
  //for MMIO
  val isMMIO = Output(Bool())
  //for ghr update
  val ghr = Output(UInt(GhrLength.W))
  val btbIsBranch = Output(Bool())  //for update ghr
  //for ghr commit
  val branchTaken = Output(Bool())
  //for difftest
  val CSRregfile = new CSRregfile
  val ArchEvent = new ArchEvent
}
class CSRregfile extends NutCoreBundle {
  val priviledgeMode      =  Output(UInt(XLEN.W))
  val mstatus      =  Output(UInt(XLEN.W))
  val sstatus      =  Output(UInt(XLEN.W))
  val mepc      =  Output(UInt(XLEN.W))
  val sepc      =  Output(UInt(XLEN.W))
  val mtval      =  Output(UInt(XLEN.W))
  val stval      =  Output(UInt(XLEN.W))
  val mtvec      =  Output(UInt(XLEN.W))
  val stvec      =  Output(UInt(XLEN.W))
  val mcause      =  Output(UInt(XLEN.W))
  val scause      =  Output(UInt(XLEN.W))
  val satp      =  Output(UInt(XLEN.W))
  val mip      =  Output(UInt(XLEN.W))
  val mie      =  Output(UInt(XLEN.W))
  val mscratch      =  Output(UInt(XLEN.W))
  val sscratch      =  Output(UInt(XLEN.W))
  val mideleg      =  Output(UInt(XLEN.W))
  val medeleg      =  Output(UInt(XLEN.W))
}

class ArchEvent extends NutCoreBundle {
  val intrNO =        Output(UInt(32.W))
  val cause =         Output(UInt(32.W))
  val exceptionPC =   Output(UInt(64.W))
  val exceptionInst = Output(UInt(32.W))
}

