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

package nutcore

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import utils._
import bus.simplebus._
import chisel3.experimental.IO
import chipsalliance.rocketchip.config.Parameters

class FrontendIO(implicit p: Parameters) extends Bundle with HasNutCoreConst {
  val imem = new SimpleBusUC(userBits = ICacheUserBundleWidth, addrBits = VAddrBits)
  val out = Vec(4, Decoupled(new DecodeIO))
  val flushVec = Output(UInt(4.W))
  val redirect = Flipped(new RedirectIO)
  val bpFlush = Output(Bool())
  val ipf = Input(Bool())
}


trait HasFrontendIO {
  implicit val p: Parameters
  val io = IO(new FrontendIO)
}

class Frontend_ooo(implicit val p: Parameters) extends NutCoreModule with HasFrontendIO {
  def pipelineConnect2[T <: Data](left: DecoupledIO[T], right: DecoupledIO[T],
    isFlush: Bool, entries: Int = 4, pipe: Boolean = false) = {
    // NOTE: depend on https://github.com/chipsalliance/chisel3/pull/2245
    // right <> Queue(left,  entries = entries, pipe = pipe, flush = Some(isFlush))
    right <> FlushableQueue(left, isFlush, entries = entries, pipe = pipe)
  }

  val ifu  = Module(new IFU_ooo)
  val ibf1 = Module(new IBF)  //copy register for high fanout signal
  val ibf2 = Module(new IBF)
  val idu  = Module(new IDU)

//  pipelineConnect2(ifu.io.out, ibf1.io.in, ifu.io.flushVec(0))
//  pipelineConnect2(ifu.io.out, ibf2.io.in, ifu.io.flushVec(0))

  ifu.io.out <> ibf1.io.in
  PipelineVector2Connect(new CtrlFlowIO, ibf1.io.out(0), ibf1.io.out(1), idu.io.in(0), idu.io.in(1), ifu.io.flushVec(1), 64)
  ibf1.io.flush := ifu.io.flushVec(1)

  ifu.io.out <> ibf2.io.in
  PipelineVector2Connect(new CtrlFlowIO, ibf2.io.out(0), ibf2.io.out(1), idu.io.in(2), idu.io.in(3), ifu.io.flushVec(1), 64)
  ibf2.io.flush := ifu.io.flushVec(1)

  io.out <> idu.io.out
  io.redirect <> ifu.io.redirect
  io.flushVec <> ifu.io.flushVec
  io.bpFlush <> ifu.io.bpFlush
  io.ipf <> ifu.io.ipf
  io.imem <> ifu.io.imem

}

class Frontend_embedded(implicit val p: Parameters) extends NutCoreModule with HasFrontendIO {
  val ifu  = Module(new IFU_embedded)
  val idu  = Module(new IDU)

  PipelineConnect(ifu.io.out, idu.io.in(0), idu.io.out(0).fire, ifu.io.flushVec(0))
  idu.io.in(1) := DontCare

  io.out <> idu.io.out
  io.redirect <> ifu.io.redirect
  io.flushVec <> ifu.io.flushVec
  io.bpFlush <> ifu.io.bpFlush
  io.ipf <> ifu.io.ipf
  io.imem <> ifu.io.imem

    Debug("------------------------ FRONTEND:------------------------\n")
    Debug("flush = %b, ifu:(%d,%d), idu:(%d,%d)\n",
      ifu.io.flushVec.asUInt, ifu.io.out.valid, ifu.io.out.ready, idu.io.in(0).valid, idu.io.in(0).ready)
    Debug(ifu.io.out.valid, "IFU: pc = 0x%x, instr = 0x%x\n", ifu.io.out.bits.pc, ifu.io.out.bits.instr)
    Debug(idu.io.in(0).valid, "IDU1: pc = 0x%x, instr = 0x%x, pnpc = 0x%x\n", idu.io.in(0).bits.pc, idu.io.in(0).bits.instr, idu.io.in(0).bits.pnpc)
}

class Frontend_inorder(implicit val p: Parameters) extends NutCoreModule with HasFrontendIO {
  val ifu  = Module(new IFU_inorder)
  val ibf = Module(new NaiveRVCAlignBuffer)
  val idu  = Module(new IDU)

  def PipelineConnect2[T <: Data](left: DecoupledIO[T], right: DecoupledIO[T],
    isFlush: Bool, entries: Int = 4, pipe: Boolean = false) = {
    // NOTE: depend on https://github.com/chipsalliance/chisel3/pull/2245
    // right <> Queue(left,  entries = entries, pipe = pipe, flush = Some(isFlush))
    right <> FlushableQueue(left, isFlush, entries = entries, pipe = pipe)
  }

  PipelineConnect2(ifu.io.out, ibf.io.in, ifu.io.flushVec(0))
  PipelineConnect(ibf.io.out, idu.io.in(0), idu.io.out(0).fire, ifu.io.flushVec(1))
  idu.io.in(1) := DontCare

  ibf.io.flush := ifu.io.flushVec(1)
  io.out <> idu.io.out
  io.redirect <> ifu.io.redirect
  io.flushVec <> ifu.io.flushVec
  io.bpFlush <> ifu.io.bpFlush
  io.ipf <> ifu.io.ipf
  io.imem <> ifu.io.imem

  Debug("------------------------ FRONTEND:------------------------\n")
  Debug("flush = %b, ifu:(%d,%d), idu:(%d,%d)\n",
    ifu.io.flushVec.asUInt, ifu.io.out.valid, ifu.io.out.ready, idu.io.in(0).valid, idu.io.in(0).ready)
  Debug(ifu.io.out.valid, "IFU: pc = 0x%x, instr = 0x%x\n", ifu.io.out.bits.pc, ifu.io.out.bits.instr)
  Debug(idu.io.in(0).valid, "IDU1: pc = 0x%x, instr = 0x%x, pnpc = 0x%x\n", idu.io.in(0).bits.pc, idu.io.in(0).bits.instr, idu.io.in(0).bits.pnpc)
}