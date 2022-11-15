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

package XiaoHe.SSDfrontend

import bus.simplebus._
import chisel3._
import chisel3.experimental.IO
import chisel3.util._
import utils._
import XiaoHe._
import XiaoHe.SSDbackend._
import XiaoHe.SSDfrontend._
import nutcore.PipelineVector2Connect
class FrontendIO(implicit val p: NutCoreConfig) extends Bundle with HasNutCoreConst {
  val imem = new SimpleBusUC(userBits = ICacheUserBundleWidth, addrBits = VAddrBits)
  val out = Vec(4, Decoupled(new DecodeIO))
  val flushVec = Output(UInt(4.W))
  val redirect = Flipped(new RedirectIO)
  val bpFlush = Output(Bool())
  val ipf = Input(Bool())
}


trait HasFrontendIO {
  implicit val p: NutCoreConfig
  val io = IO(new FrontendIO)
}

class Frontend_ooo(implicit val p: NutCoreConfig) extends NutCoreModule with HasFrontendIO {
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