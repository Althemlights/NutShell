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

import XiaoHe.SSDbackend.{SSDCoreConfig, myDebug}
import XiaoHe.isa.predecode.PreDecode
import bus.simplebus._
import chisel3._
import chisel3.util._
import top.Settings
import utils._
//import difftest._
import XiaoHe._
import XiaoHe.SSDbackend._
import XiaoHe.SSDfrontend._
trait HasResetVector {
  val resetVector = Settings.getLong("ResetVector")
}

class ICacheUserBundle extends NutCoreBundle {
  val pc = UInt(VAddrBits.W)
  val brIdx = UInt(4.W) // mark if an inst is predicted to branch
  val pnpc = UInt(VAddrBits.W)
  val instValid = UInt(4.W) // mark which part of this inst line is valid
  val btbIsBranch = UInt(4.W) // align with npc, need to delay one clock cycle
}
// Note: update ICacheUserBundleWidth when change ICacheUserBundle

class IFU_ooo extends NutCoreModule with HasResetVector {
  val io = IO(new Bundle {

    val imem = new SimpleBusUC(userBits = ICacheUserBundleWidth, addrBits = VAddrBits)
    // val pc = Input(UInt(VAddrBits.W))
    val outPredictPkt = Decoupled(new PredictPkt)
    val outInstr = Decoupled(Output(UInt(64.W)))

    val redirect = Flipped(new RedirectIO)
    val flushVec = Output(UInt(4.W))
    val bpFlush = Output(Bool())
    val ipf = Input(Bool())
  })

  // Next-line branch predictor
  val nlp = Module(new BPU_ooo)

  // pc
  val pc = RegInit(resetVector.U(VAddrBits.W))
  // val pcBrIdx = RegInit(0.U(4.W))
  val pcInstValid = RegInit("b1111".U)
  val pcUpdate = Wire(Bool())
  pcUpdate := io.redirect.valid || io.imem.req.fire()
  val snpc = Cat(pc(VAddrBits-1, 3), 0.U(3.W)) + CacheReadWidth.U  // IFU will always ask icache to fetch next instline
  // Note: we define instline as 8 Byte aligned data from icache

  // nlpxxx_latch is used for the situation when I$ is disabled
  val nlpvalidreg = RegInit(false.B)
  val nlpvalid_latch = nlpvalidreg & !io.redirect.valid
  val nlpbridx_latch = RegInit(0.U(4.W))
  val nlptarget_latch = RegInit(0.U(VAddrBits.W))

  when (nlp.io.out.valid) {
    nlpvalidreg := true.B
    nlpbridx_latch := nlp.io.brIdx.asUInt
    nlptarget_latch := nlp.io.out.target
  }

  when (io.imem.req.fire() || io.redirect.valid) {
    nlpvalidreg := false.B
    nlpbridx_latch := 0.U
    nlptarget_latch := 0.U
  }

  val bpuValid = if (Settings.get("HasIcache")) nlp.io.out.valid else nlpvalid_latch
  val bpuTarget = if (Settings.get("HasIcache")) nlp.io.out.target else nlptarget_latch
  val bpuBrIdx = if (Settings.get("HasIcache")) nlp.io.brIdx.asUInt else nlpbridx_latch

  // cross instline inst branch predict logic "crosslineJump"
  //
  // if "crosslineJump", icache will need to fetch next instline, then fetch redirect addr
  // "crosslineJump" mechanism is used to speed up such code:
  // ```
  // 000c BranchCondition (32 bit inst)
  // ```
  // in this case, full inst is needed by BRU to get the right branch result,
  // so we need to fetch the next inst line to get the higher bits of a 32bit branch inst
  // but in order to use BP result to avoid pipeline flush,
  // the next inst provided by icache should be predicted npc, instead of sequential npc
  //
  // There is another way: BPU uses "high 16 bit half" pc to index not-compressed crossline branch insts.
//  dontTouch(nlp.io.out)
  val crosslineJump = nlp.io.crosslineJump
  val s_idle :: s_crosslineJump :: Nil = Enum(2)
  val state = RegInit(s_idle)
  switch(state){
    is(s_idle){
      when(pcUpdate && crosslineJump && !io.redirect.valid){ state := s_crosslineJump }
    }
    is(s_crosslineJump){
      when(pcUpdate){ state := s_idle }
      when(io.redirect.valid){ state := s_idle }
    }
  }
  val crosslineJumpTarget = RegEnable(nlp.io.out.target, crosslineJump && pcUpdate)

  // predicted next pc
  val pnpc = Mux(crosslineJump, snpc, bpuTarget)

  // next pc
  val npc = Wire(UInt(VAddrBits.W))
  npc := Mux(io.redirect.valid, io.redirect.target, Mux(state === s_crosslineJump, crosslineJumpTarget, Mux(bpuValid, pnpc, snpc)))



  // instValid: which part of an instline contains an valid inst
  // e.g. 1100 means inst(s) in instline(63,32) is/are valid
  val npcInstValid = Wire(UInt(4.W))
  def genInstValid(pc: UInt) = LookupTree(pc(2,1), List(
    "b00".U -> "b1111".U,
    "b01".U -> "b1110".U,
    "b10".U -> "b1100".U,
    "b11".U -> "b1000".U
  ))
  npcInstValid := Mux(crosslineJump && !(state === s_crosslineJump) && !io.redirect.valid, "b0001".U,genInstValid(npc))

  // branch position index, 4 bit vector
  // e.g. brIdx 0010 means a branch is predicted/assigned at pc (offset 2)
  val brIdx = Wire(UInt(4.W))
  // predicted branch position index, 4 bit vector
  val pbrIdx = bpuBrIdx | (crosslineJump << 3)
  brIdx := Mux(io.redirect.valid, 0.U, Mux(state === s_crosslineJump, 0.U, pbrIdx))

  // BP will be disabled shortly after a redirect request
  nlp.io.in.pc.valid := Mux(crosslineJump && !pcUpdate, true.B,io.imem.req.fire() || io.redirect.valid)// only predict when Icache accepts a request
  nlp.io.in.pc.bits := Mux(crosslineJump && !pcUpdate, pc, npc)  // predict one cycle early
  nlp.io.flush := io.redirect.valid && false.B// redirect means BPU may need to be updated


  when (pcUpdate) {
    pc := npc
    pcInstValid := npcInstValid
  }


  io.flushVec := Mux(io.redirect.valid, Mux(io.redirect.rtype === 0.U, "b1111".U, "b0011".U), 0.U)
  io.bpFlush := false.B

  val icacheUserGen = Wire(new ICacheUserBundle)
  icacheUserGen.pc := pc
  icacheUserGen.pnpc := Mux(crosslineJump, nlp.io.out.target, npc)
  icacheUserGen.brIdx := brIdx & pcInstValid
  icacheUserGen.instValid := pcInstValid
  icacheUserGen.btbIsBranch := nlp.io.out.btbIsBranch

  io.imem.req.bits.apply(addr = Cat(pc(VAddrBits-1,1),0.U(1.W)), //cache will treat it as Cat(pc(63,3),0.U(3.W))
    size = "b11".U, cmd = SimpleBusCmd.read, wdata = 0.U, wmask = 0.U, user = icacheUserGen.asUInt)
  io.imem.req.valid := io.outInstr.ready
  io.imem.resp.ready := io.outInstr.ready || io.flushVec(0)

  //inst path only uses 32bit inst, get the right inst according to pc(2)

  io.outInstr.valid := io.imem.resp.valid && !io.flushVec(0)
  io.outInstr.bits := io.imem.resp.bits.rdata


  io.outPredictPkt.valid := io.imem.resp.valid && !io.flushVec(0)
  io.outPredictPkt.bits.pc := io.imem.resp.bits.user.get.asTypeOf(new ICacheUserBundle).pc
  io.outPredictPkt.bits.pnpc := io.imem.resp.bits.user.get.asTypeOf(new ICacheUserBundle).pnpc
  io.outPredictPkt.bits.brIdx := io.imem.resp.bits.user.get.asTypeOf(new ICacheUserBundle).brIdx
  io.outPredictPkt.bits.instValid := io.imem.resp.bits.user.get.asTypeOf(new ICacheUserBundle).instValid
  io.outPredictPkt.bits.btbIsBranch := io.imem.resp.bits.user.get.asTypeOf(new ICacheUserBundle).btbIsBranch
  io.outPredictPkt.bits.icachePF := io.ipf




  // Illegal branch predict check
  val FixInvalidBranchPredict = false

  def preDecodeIsBranch(x: UInt) = {
    require(x.getWidth == 16)
    val res :: Nil = ListLookup(x, List(false.B), PreDecode.branchTable)
    res
  }

}
