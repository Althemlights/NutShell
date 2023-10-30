/**************************************************************************************
 * Copyright (c) 2020 Institute of Computing Technology, CAS
 * Copyright (c) 2020 University of Chinese Academy of Sciences
 *
 * NutShell is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 *             http://license.coscl.org.cn/MulanPSL2
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND, EITHER
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT, MERCHANTABILITY OR
 * FIT FOR A PARTICULAR PURPOSE.
 *
 * See the Mulan PSL v2 for more details.
 ***************************************************************************************/

package XiaoHe

import chisel3._
import chisel3.util._

class CtrlSignalIO extends NutCoreBundle {
  val src1Type = Output(SrcType())
  val src2Type = Output(SrcType())
  val fuType = Output(FuType())
  val fuOpType = Output(FuOpType())
  val rfSrc1 = Output(UInt(5.W))
  val rfSrc2 = Output(UInt(5.W))
  val rfWen = Output(Bool())
  val rfDest = Output(UInt(5.W))
  val isNutCoreTrap = Output(Bool())
  val isSrc1Forward = Output(Bool())
  val isSrc2Forward = Output(Bool())
  val noSpecExec = Output(Bool())  // This inst can not be speculated
  val isBlocked = Output(Bool())   // This inst requires pipeline to be blocked

  //new signal
  val rs1Valid = Output(Bool())
  val rs2Valid = Output(Bool())
  val rdValid = Output(Bool())
}

class DataSrcIO extends NutCoreBundle {
  val src1 = Output(UInt(XLEN.W))
  val src2 = Output(UInt(XLEN.W))
  val imm  = Output(UInt(XLEN.W))
}

class RedirectIO extends NutCoreBundle {
  val target = Output(UInt(VAddrBits.W))
  val rtype = Output(UInt(1.W)) // 1: branch mispredict: only need to flush frontend  0: others: flush the whole pipeline
  val valid = Output(Bool())
  val btbIsBranch = Output(UInt(4.W))
  //for debug
  val pc = Output(UInt(VAddrBits.W))
}

class RedirectIO_nooo extends NutCoreBundle {
  val target = Output(UInt(VAddrBits.W))
  val rtype = Output(UInt(1.W)) // 1: branch mispredict: only need to flush frontend  0: others: flush the whole pipeline
  val valid = Output(Bool())
}


class BypassIO extends  Bundle{
  val isALU = Output(Bool())
  val isMDU = Output(Bool())
  val isLoad = Output(Bool())
  val isStore = Output(Bool())
}

class CtrlFlowIO extends NutCoreBundle {
  val instr = Output(UInt(64.W))
  val pc = Output(UInt(VAddrBits.W))
  val pnpc = Output(UInt(VAddrBits.W))
  val redirect = new RedirectIO
  val exceptionVec = Output(Vec(16, Bool()))
  val intrVec = Output(Vec(12, Bool()))
  val brIdx = Output(UInt(4.W))
  val isRVC = Output(Bool())
  val crossPageIPFFix = Output(Bool())
  val runahead_checkpoint_id = Output(UInt(64.W))
  val isBranch = Output(Bool())
  val triggeredFire = Output(new TriggerCf)

  //sfb add
  val sfb = Output(Bool())
}

class DecodeIO extends NutCoreBundle {
  val cf = new CtrlFlowIO
  val ctrl = new CtrlSignalIO
  val data = new DataSrcIO
}

class WriteBackIO extends NutCoreBundle {
  val rfWen = Output(Bool())
  val rfDest = Output(UInt(5.W))
  val rfData = Output(UInt(XLEN.W))
}

class CommitIO extends NutCoreBundle {
  val decode = new DecodeIO
  val isMMIO = Output(Bool())
  val intrNO = Output(UInt(XLEN.W))
  val commits = Output(Vec(FuType.num, UInt(XLEN.W)))
}



class FunctionUnitIO extends NutCoreBundle {
  val in = Flipped(Decoupled(new Bundle {
    val src1 = Output(UInt(XLEN.W))
    val src2 = Output(UInt(XLEN.W))
    val func = Output(FuOpType())
  }))
  val out = Decoupled(Output(UInt(XLEN.W)))
}

class ForwardIO extends NutCoreBundle {
  val valid = Output(Bool())
  val wb = new WriteBackIO
  val fuType = Output(FuType())
}

class MMUIO extends NutCoreBundle {
  // val ptev = Output(Bool())
  // val pteu = Output(Bool())
  // val ptex = Output(Bool())
  // val valid = Output(Bool())
  // val isStore = Output(Bool())

  val priviledgeMode = Input(UInt(2.W))
  val status_sum = Input(Bool())
  val status_mxr = Input(Bool())

  val loadPF = Output(Bool())
  val storePF = Output(Bool())
  val addr = Output(UInt(VAddrBits.W))

  def isPF() = loadPF || storePF
}

class MemMMUIO extends NutCoreBundle {
  val imem = new MMUIO
  val dmem = new MMUIO
}

class TLBExuIO extends NutCoreBundle {
  val satp = Output(UInt(XLEN.W))
  val sfence = new Bundle {
    val valid = Output(Bool())
    val asid  = Output(UInt(9.W))
    val vaddr = Output(UInt(XLEN.W))
  }

  def access(valid: Bool, src1: UInt, src2: UInt, func: UInt, satp: UInt) = {//func no use here for just sfence.vma only
    this.sfence.valid := valid
    this.sfence.vaddr := src1
    this.sfence.asid  := src2(8,0)
    this.satp := satp
  }
}

class PredictPkt extends NutCoreBundle {
  val pc = Output(UInt(VAddrBits.W)) // real PC will be regenerated in IBF
  val pnpc = Output(UInt(VAddrBits.W))
  val brIdx = Output(UInt(4.W))
  val instValid = Output(UInt(4.W))
  //above will be used as user bits in icache
  val icachePF = Output(Bool())
  val btbIsBranch = Output(UInt(4.W))

  //sfb add
  val sfb = Output(UInt(4.W))
}

// Micro OP
// XS bundle
// these 3 bundles help distribute trigger control signals from CSR
// to Frontend, Load and Store.
class FrontendTdataDistributeIO extends NutCoreBundle {
  val tUpdate = ValidIO(new Bundle {
    val addr = Output(UInt(log2Up(TriggerNum).W))
    val tdata = new MatchTriggerIO
  })
  val tEnableVec: Vec[Bool] = Output(Vec(TriggerNum, Bool()))
}

class MemTdataDistributeIO extends NutCoreBundle {
  val tUpdate = ValidIO(new Bundle {
    val addr = Output(UInt(log2Up(TriggerNum).W))
    val tdata = new MatchTriggerIO
  })
  val tEnableVec: Vec[Bool] = Output(Vec(TriggerNum, Bool()))
}

class MatchTriggerIO extends NutCoreBundle {
  val matchType = Output(UInt(2.W))
  val select = Output(Bool())
  val timing = Output(Bool())
  val action = Output(Bool())
  val chain = Output(Bool())
  val execute = Output(Bool())
  val store = Output(Bool())
  val load = Output(Bool())
  val tdata2 = Output(UInt(64.W))
}

class CustomCSRCtrlIO extends NutCoreBundle {
  // TODO: move it to a new bundle, since single step is not a custom control signal
  //val singlestep = Output(Bool())
  val frontend_trigger = new FrontendTdataDistributeIO()
  val mem_trigger = new MemTdataDistributeIO()
}

class TriggerCf extends NutCoreBundle {
  // frontend
  val frontendHit       = Vec(TriggerNum, Bool()) // en && hit
  val frontendTiming    = Vec(TriggerNum, Bool()) // en && timing
  val frontendChain     = Vec(TriggerNum, Bool()) // en && chain
  val frontendCanFire   = Vec(TriggerNum, Bool())
  // backend
  val backendHit        = Vec(TriggerNum, Bool())
  val backendCanFire    = Vec(TriggerNum, Bool())

  // Two situations not allowed:
  // 1. load data comparison
  // 2. store chaining with store
  // def getHitFrontend = frontendHit.reduce(_ || _)
  // def getHitBackend = backendHit.reduce(_ || _)
  // def hit = getHitFrontend || getHitBackend
  def getFrontendCanFire = frontendCanFire.reduce(_ || _)
  def getBackendCanFire = backendCanFire.reduce(_ || _)
  def canFire = getFrontendCanFire || getBackendCanFire
  def clear(): Unit = {
    frontendHit.foreach(_ := false.B)
    frontendCanFire.foreach(_ := false.B)
    backendHit.foreach(_ := false.B)
    backendCanFire.foreach(_ := false.B)
    frontendTiming.foreach(_ := false.B)
    frontendChain.foreach(_ := false.B)
  }
}
