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

import chisel3._
import chisel3.util._
import _root_.utils._
import XiaoHe._
import XiaoHe.SSDbackend._
import XiaoHe.SSDbackend.fu._
import XiaoHe.SSDfrontend._
trait HasIBUFConst{
  // val multiIssue = true
  val instUnitWidth = 16 //bit
  val ibufBitSize = 128 //256 //bit
  val ibufSize = ibufBitSize / instUnitWidth
}

// 2-width Instruction Align Buffer
class IBF extends NutCoreModule with HasInstrType with HasIBUFConst{
  val io = IO(new Bundle {
    val inInstr = Flipped(Decoupled(UInt(64.W)))
    val inPredictPkt = Flipped(Decoupled(new PredictPkt))
    val out = Vec(2, Decoupled(new CtrlFlowIO))
    val flush = Input(Bool())
    val frontend_trigger = Flipped(new FrontendTdataDistributeIO())
  })

  //ibuf reg
  val ringInstBuffer = RegInit(VecInit(Seq.fill(ibufSize)(0.U(16.W))))
  val pcRingMeta = RegInit(VecInit(Seq.fill(ibufSize)(0.U(VAddrBits.W))))
  val npcRingMeta = RegInit(VecInit(Seq.fill(ibufSize)(0.U(VAddrBits.W))))
  val validRingMeta = RegInit(VecInit(Seq.fill(ibufSize)(false.B)))
  val btbIsBranchRingMeta = RegInit(VecInit(Seq.fill(ibufSize)(false.B)))
  val sfbRingMeta = RegInit(VecInit(Seq.fill(ibufSize)(false.B)))
  val branchRingMeta = RegInit(VecInit(Seq.fill(ibufSize)(false.B)))
  val ipfRingMeta = RegInit(VecInit(Seq.fill(ibufSize)(false.B)))
  val ringBufferHead = RegInit(0.U(log2Up(ibufSize).W))
  val ringBufferTail = RegInit(0.U(log2Up(ibufSize).W))
  val ringBufferEmpty = ringBufferHead === ringBufferTail && !validRingMeta(ringBufferHead)
  val ringBufferAllowin = (0 to 3).map(i => !validRingMeta(ringBufferHead+i.U)).foldRight(true.B)((sum,i)=>sum&i)

  //ibuf decode
  val instr = io.inInstr.bits
  val instrVec = Wire(Vec(4, UInt(16.W)))
  val isRVC = Wire(Vec(4, Bool()))
  val instValid = io.inPredictPkt.bits.instValid
  val brIdx = io.inPredictPkt.bits.brIdx // NOTE: brIdx == false.B if !instValid
  val icachePF = io.inPredictPkt.bits.icachePF
  instrVec := instr.asTypeOf(Vec(4, UInt(16.W)))
  (0 to 3).map(i => isRVC(i.U) := instrVec(i.U)(1,0) =/= "b11".U)

  // Frontend Triggers
  val frontendTrigger = Module(new FrontendTrigger)
  frontendTrigger.io.frontendTrigger  := io.frontend_trigger
  frontendTrigger.io.pc := VecInit((0 until PredictWidth).map { index =>
    io.inPredictPkt.bits.pc + (index.U * 4.U)
  })

  frontendTrigger.io.data := instr.asTypeOf(Vec(2, UInt(32.W)))
  val triggered = frontendTrigger.io.triggered
  val triggerInfoBuffer = RegInit(VecInit(Seq.fill(ibufSize)(0.U(new TriggerCf().getWidth.W))))

  //ibuf enqueue
  //if valid & ringBufferAllowin, enqueue
  val needEnqueue = Wire(Vec(4, Bool()))
  needEnqueue(0) := instValid(0)
  needEnqueue(1) := instValid(1) && !(brIdx(0) && isRVC(0))
  needEnqueue(2) := instValid(2) && !brIdx(0) && !(brIdx(1) && isRVC(1))
  needEnqueue(3) := instValid(3) && !(brIdx(0)) && !(brIdx(1)) && !(brIdx(2) && isRVC(2))

  // NOTE: needEnqueue is always of fmt "0?1?0?"
  // therefore we first shift input data, then enqueue
  // val enqueueSize = List.tabulate(4)(i => needEnqueue(i).asUInt).foldRight(0.U)((sum, i)=>sum+&i) // count(true) in needEnqueue
  val enqueueSize = needEnqueue(0).asUInt()+&needEnqueue(1).asUInt()+&needEnqueue(2).asUInt()+&needEnqueue(3).asUInt() // count(true) in needEnqueue
  val shiftSize = Mux(needEnqueue(0), 0.U, Mux(needEnqueue(1), 1.U, Mux(needEnqueue(2), 2.U, 3.U))) // count 0 in low addr in needEnqueue
  val enqueueFire = (0 to 3).map(i => enqueueSize >= (i+1).U)

  val ibufWen = io.inInstr.fire() // i.e. ringBufferAllowin && io.in.valid
  def ibufWrite(targetSlot: Int, shiftSize: UInt){
    ringInstBuffer(targetSlot.U + ringBufferHead) := instrVec(shiftSize + targetSlot.U)
    pcRingMeta(targetSlot.U + ringBufferHead) := Cat(io.inPredictPkt.bits.pc(VAddrBits-1, 3), shiftSize + targetSlot.U, 0.U(1.W))
    npcRingMeta(targetSlot.U + ringBufferHead) := io.inPredictPkt.bits.pnpc
    validRingMeta(targetSlot.U + ringBufferHead) := true.B
    branchRingMeta(targetSlot.U + ringBufferHead) := io.inPredictPkt.bits.brIdx(shiftSize + targetSlot.U)
    ipfRingMeta(targetSlot.U + ringBufferHead) := io.inPredictPkt.bits.icachePF
    btbIsBranchRingMeta(targetSlot.U + ringBufferHead) := io.inPredictPkt.bits.btbIsBranch(shiftSize + targetSlot.U)
    sfbRingMeta(targetSlot.U + ringBufferHead) := io.inPredictPkt.bits.sfb(shiftSize + targetSlot.U)
    triggerInfoBuffer(targetSlot.U + ringBufferHead) := triggered((shiftSize + targetSlot.U) >> 1).asUInt
  }

  when (ibufWen) {
    when(enqueueFire(0)){ibufWrite(0, shiftSize)}
    when(enqueueFire(1)){ibufWrite(1, shiftSize)}
    when(enqueueFire(2)){ibufWrite(2, shiftSize)}
    when(enqueueFire(3)){ibufWrite(3, shiftSize)}
    ringBufferHead := ringBufferHead + enqueueSize
  }

  io.inInstr.ready := ringBufferAllowin /*|| !io.in.valid*/// used to be !io.in.valid, do not know what's for
  io.inPredictPkt.ready := ringBufferAllowin
  //ibuf dequeue
  //there are 2 dequeue sockets

  //dequeue inst select
  val dequeueInstrVec = Wire(Vec(4, UInt(16.W)))
  val dequeueIsValid = Wire(Vec(4, Bool()))
  (0 to 3).map(i => dequeueInstrVec(i.U) := ringInstBuffer(i.U + ringBufferTail))
  (0 to 3).map(i => dequeueIsValid(i.U) := validRingMeta(i.U + ringBufferTail))


  val first4B = WireInit(false.B)
  val first2B = WireInit(false.B)
  val second4B = WireInit(false.B)
  val second2B = WireInit(false.B)

  first4B := dequeueInstrVec(0)(1,0) === "b11".U
  first2B := !first4B

  second4B := Mux(first2B, dequeueInstrVec(1)(1,0) === "b11".U,dequeueInstrVec(2)(1,0) === "b11".U)
  second2B := !second4B


  //dequeue socket 1
  io.out(0).bits := DontCare
  io.out(0).bits.redirect.valid := false.B
  io.out(0).bits.pc := pcRingMeta(ringBufferTail)
  io.out(0).bits.redirect.btbIsBranch := btbIsBranchRingMeta(ringBufferTail)
  io.out(0).bits.pnpc := npcRingMeta(ringBufferTail)
  io.out(0).bits.instr := Mux(first2B, Cat(0.U, ringInstBuffer(ringBufferTail)),Cat(ringInstBuffer(ringBufferTail+1.U), ringInstBuffer(ringBufferTail)))
  io.out(0).bits.brIdx := branchRingMeta(ringBufferTail)
  io.out(0).bits.isRVC := first2B
  io.out(0).bits.crossPageIPFFix := !ipfRingMeta(ringBufferTail) && first4B && ipfRingMeta(ringBufferTail + 1.U)

  io.out(0).bits.sfb := sfbRingMeta(ringBufferTail)


  io.out(0).valid := dequeueIsValid(0) && (first2B || dequeueIsValid(1)) && !io.flush
  io.out(0).bits.exceptionVec.map(_ => false.B)
  io.out(0).bits.exceptionVec(instrPageFault) := ipfRingMeta(ringBufferTail) || !first2B && ipfRingMeta(ringBufferTail + 1.U)
  io.out(0).bits.triggeredFire := triggerInfoBuffer(ringBufferTail).asTypeOf(new TriggerCf)

  val dequeueSize1 = Mux(io.out(0).fire(), Mux(first2B, 1.U, 2.U), 0.U) // socket 2 will use dequeueSize1 to get its inst

  //dequeue socket 2
  val inst2_StartIndex = ringBufferTail + dequeueSize1
  io.out(1).bits := DontCare
  io.out(1).bits.redirect.valid := false.B
  io.out(1).bits.pc := pcRingMeta(inst2_StartIndex)
  io.out(1).bits.redirect.btbIsBranch := btbIsBranchRingMeta(inst2_StartIndex)
  io.out(1).bits.pnpc := npcRingMeta(inst2_StartIndex)
  io.out(1).bits.instr := Mux(second2B,Cat(0.U, ringInstBuffer(inst2_StartIndex)),Cat(ringInstBuffer(inst2_StartIndex+1.U), ringInstBuffer(inst2_StartIndex)))
  io.out(1).bits.brIdx := branchRingMeta(inst2_StartIndex)
  io.out(1).bits.isRVC := second2B
  io.out(1).bits.crossPageIPFFix := !ipfRingMeta(inst2_StartIndex) && !second2B && ipfRingMeta(inst2_StartIndex + 1.U)

  io.out(1).bits.sfb := sfbRingMeta(inst2_StartIndex)

  if (EnableMultiIssue) {
    io.out(1).valid := io.out(0).valid && dequeueIsValid(dequeueSize1) && (second2B || dequeueIsValid(dequeueSize1 + 1.U)) && !io.flush
  } else {
    io.out(1).valid := false.B
  }
  io.out(1).bits.exceptionVec.map(_ => false.B)
  io.out(1).bits.exceptionVec(instrPageFault) := ipfRingMeta(inst2_StartIndex) || !second2B && ipfRingMeta(inst2_StartIndex + 1.U)
  io.out(1).bits.triggeredFire := triggerInfoBuffer(inst2_StartIndex).asTypeOf(new TriggerCf)
  
  val dequeueSize2 = Mux(io.out(1).fire, Mux(second2B, 1.U, 2.U), 0.U) // socket 2 will use dequeueSize1 to get its inst
  //Debug(io.out(1).fire, "dequeue2: inst %x pc %x npc %x br %x ipf %x(%x)\n", io.out(1).bits.instr, io.out(1).bits.pc, io.out(1).bits.pnpc, io.out(1).bits.brIdx, io.out(1).bits.exceptionVec(instrPageFault), io.out(1).bits.crossPageIPFFix)

  val dequeueSize = dequeueSize1 +& dequeueSize2

  //dequeue control
  val dequeueFire = dequeueSize > 0.U
  when (dequeueFire) {
    when(dequeueSize >= 1.U){validRingMeta(0.U + ringBufferTail) := false.B}
    when(dequeueSize >= 2.U){validRingMeta(1.U + ringBufferTail) := false.B}
    when(dequeueSize >= 3.U){validRingMeta(2.U + ringBufferTail) := false.B}
    when(dequeueSize >= 4.U){validRingMeta(3.U + ringBufferTail) := false.B}
    ringBufferTail := ringBufferTail + dequeueSize;
    //Debug("ibuf dequeue %x*16 bits\n", dequeueSize)
  }

  //flush control
  when (io.flush) {
    ringBufferHead := 0.U
    ringBufferTail := 0.U
    List.tabulate(ibufSize)(i => validRingMeta(i) := 0.U) // set valid to 0
  }
}


class FrontendTrigger extends NutCoreModule with SdtrigExt {
  
  val io = IO(new Bundle(){
    val frontendTrigger = Input(new FrontendTdataDistributeIO)              // trigger message
    val triggered     = Output(Vec(PredictWidth, new TriggerCf))            // trigger fire message
    val pc            = Input(Vec(PredictWidth, UInt(VAddrBits.W)))
    val data          = Input(Vec(PredictWidth, UInt(32.W)))
  })

  val data          = io.data

  val rawInsts = VecInit((0 until PredictWidth).map(i => data(i)))

  val tdata = RegInit(VecInit(Seq.fill(TriggerNum)(0.U.asTypeOf(new MatchTriggerIO))))
  when(io.frontendTrigger.tUpdate.valid) {
    tdata(io.frontendTrigger.tUpdate.bits.addr) := io.frontendTrigger.tUpdate.bits.tdata
  }
//  io.triggered.foreach{ i => i := 0.U.asTypeOf(new TriggerCf)}
  val triggerEnableVec = RegInit(VecInit(Seq.fill(TriggerNum)(false.B))) // From CSR, controlled by priv mode, etc.
  triggerEnableVec := io.frontendTrigger.tEnableVec
  Debug(triggerEnableVec.asUInt.orR, "Debug Mode: At least one frontend trigger is enabled\n")

  val triggerTimingVec = VecInit(tdata.map(_.timing))
  val triggerChainVec = VecInit(tdata.map(_.chain))

  //for (i <- 0 until TriggerNum) { PrintTriggerInfo(triggerEnableVec(i), tdata(i)) }

  for (i <- 0 until PredictWidth) {
    val currentPC = io.pc(i)
    val inst = WireInit(rawInsts(i))
    val triggerHitVec = Wire(Vec(TriggerNum, Bool()))
    val triggerCanFireVec = Wire(Vec(TriggerNum, Bool()))

    for (j <- 0 until TriggerNum) {
      triggerHitVec(j) := Mux(
        tdata(j).select,
        TriggerCmp(inst, tdata(j).tdata2, tdata(j).matchType, triggerEnableVec(j)),
        TriggerCmp(currentPC, tdata(j).tdata2, tdata(j).matchType, triggerEnableVec(j))
      )
    }

    TriggerCheckCanFire(TriggerNum, triggerCanFireVec, triggerHitVec, triggerTimingVec, triggerChainVec)

    // only hit, no matter fire or not
    io.triggered(i).frontendHit := triggerHitVec
    // can fire, exception will be handled at rob enq
    io.triggered(i).frontendCanFire := triggerCanFireVec
    io.triggered(i).frontendTiming  := triggerTimingVec.zip(triggerEnableVec).map{ case(timing, en) => timing && en}
    io.triggered(i).frontendChain  := triggerChainVec.zip(triggerEnableVec).map{ case(chain, en) => chain && en}
    Debug(io.triggered(i).getFrontendCanFire, p"Debug Mode: Predecode Inst No. ${i} has trigger fire vec ${io.triggered(i).frontendCanFire}\n")
  }
  io.triggered.foreach(_.backendCanFire := VecInit(Seq.fill(TriggerNum)(false.B)))
  io.triggered.foreach(_.backendHit := VecInit(Seq.fill(TriggerNum)(false.B)))
}