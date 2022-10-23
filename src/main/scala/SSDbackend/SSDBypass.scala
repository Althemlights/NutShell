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

package SSDbackend

import chisel3._
import _root_.utils.PipelineConnect
import chisel3.util._
import chisel3.{Flipped, Module, _}
import nutcore._

class DecodeIO2BypassPkt extends Module {
  val io = IO(new Bundle {
    val in = Vec(2, Flipped(Decoupled(new DecodeIO)))
    val BypassPktTable = Input(Vec(10, new BypassPkt))
    val BypassPktValid = Input(Vec(10, Bool()))
    val issueStall = Output(Vec(2, Bool()))
    val memStall = Input(Bool())
    val mduStall = Input(Bool())
    val out0 = Decoupled(new BypassPkt)
    val out1 = Decoupled(new BypassPkt)
    val pmuio = new PMUIO0

  })

  def hitStageCheck(hitVec:Vec[Bool], begin:Int, end:Int):Bool = {
    val out = Wire(Bool())
    if(begin == end)
      out := hitVec(begin)
    else
      out := hitVec.asUInt()(end,begin).orR()
    out
  }
  //生成 BypassPkt， 以及issue stall 信号
  val i0decodePkt = Wire(new decodePkt)
  val i1decodePkt = Wire(new decodePkt)
  val lsuCtrli0 = Wire(new LSUPipeBypassCtrl)
  val lsuCtrli1 = Wire(new LSUPipeBypassCtrl)
  val i0BypassCtlPkt = io.out0.bits.BypassCtl
  val i1BypassCtlPkt = io.out1.bits.BypassCtl
  DecodeIO2decodePkt(io.in(0).bits, i0decodePkt)
  DecodeIO2decodePkt(io.in(1).bits, i1decodePkt)
  io.out0.bits.decodePkt := i0decodePkt
  io.out1.bits.decodePkt := i1decodePkt
  io.out0.valid <> io.in(0).valid
  io.out1.valid <> io.in(1).valid
  io.in(0).ready <> io.out0.ready
  io.in(1).ready <> io.out1.ready


  //pipeline0 : 1,3,5,7,9
  //pipeline1 : 0,2,4,6,8
  //i0 i1 hit
  val i0rs1valid = Wire(Bool())
  val i0rs2valid = Wire(Bool())
  val i1rs1valid = Wire(Bool())
  val i1rs2valid = Wire(Bool())


  i0rs1valid := io.in(0).bits.ctrl.src1Type === SrcType.reg && io.in(0).bits.ctrl.rfSrc1 =/= 0.U
  i0rs2valid := io.in(0).bits.ctrl.src2Type === SrcType.reg && io.in(0).bits.ctrl.rfSrc2 =/= 0.U
  i1rs1valid := io.in(1).bits.ctrl.src1Type === SrcType.reg && io.in(1).bits.ctrl.rfSrc1 =/= 0.U
  i1rs2valid := io.in(1).bits.ctrl.src2Type === SrcType.reg && io.in(1).bits.ctrl.rfSrc2 =/= 0.U

  val i0rs1validTmp = io.in(0).bits.ctrl.rs1Valid
  val i0rs2validTmp = io.in(0).bits.ctrl.rs2Valid
  val i1rs1validTmp = io.in(1).bits.ctrl.rs1Valid
  val i1rs2validTmp = io.in(1).bits.ctrl.rs2Valid

  dontTouch(i0rs1validTmp)
  dontTouch(i0rs2validTmp)
  dontTouch(i1rs1validTmp)
  dontTouch(i1rs2validTmp)

  //hit stage
  val i0rs1hitStage = WireInit(10.U(4.W))
  val i0rs2hitStage = WireInit(10.U(4.W))
  val i1rs1hitStage = WireInit(10.U(4.W))
  val i1rs2hitStage = WireInit(10.U(4.W))
  dontTouch(i0rs1hitStage)
  dontTouch(i0rs2hitStage)
  dontTouch(i1rs1hitStage)
  dontTouch(i1rs2hitStage)


  val i0rs1HitSel = VecInit(Seq.fill(11)(false.B))
  val i0rs2HitSel = VecInit(Seq.fill(11)(false.B))
  val i1rs1HitSel = VecInit(Seq.fill(11)(false.B))
  val i1rs2HitSel = VecInit(Seq.fill(11)(false.B))
  val stageSeq = Seq(0.U,1.U,2.U,3.U,4.U,5.U,6.U,7.U,8.U,9.U,10.U)//10 is set for the default value of PriorityMux

  i0rs1hitStage := PriorityMux(i0rs1HitSel,stageSeq)
  i0rs2hitStage := PriorityMux(i0rs2HitSel,stageSeq)
  i1rs1hitStage := PriorityMux(i1rs1HitSel,stageSeq)
  i1rs2hitStage := PriorityMux(i1rs2HitSel,stageSeq)

  for (i <- 0 to 10) {
    if (i <= 9) {
      i0rs1HitSel(i) := io.in(0).bits.ctrl.rfSrc1 === io.BypassPktTable(i).decodePkt.rd && io.BypassPktValid(i) && i0rs1valid
      i0rs2HitSel(i) := io.in(0).bits.ctrl.rfSrc2 === io.BypassPktTable(i).decodePkt.rd && io.BypassPktValid(i) && i0rs2valid
      i1rs1HitSel(i) := io.in(1).bits.ctrl.rfSrc1 === io.BypassPktTable(i).decodePkt.rd && io.BypassPktValid(i) && i1rs1valid
      i1rs2HitSel(i) := io.in(1).bits.ctrl.rfSrc2 === io.BypassPktTable(i).decodePkt.rd && io.BypassPktValid(i) && i1rs2valid
//      i0rs1HitSel(i) := io.in(0).bits.ctrl.rfSrc1 === io.BypassPktTable(i).decodePkt.rd && io.BypassPktTable(i).decodePkt.rdvalid && io.BypassPktValid(i) && i0rs1valid
//      i0rs2HitSel(i) := io.in(0).bits.ctrl.rfSrc2 === io.BypassPktTable(i).decodePkt.rd && io.BypassPktTable(i).decodePkt.rdvalid && io.BypassPktValid(i) && i0rs2valid
//      i1rs1HitSel(i) := io.in(1).bits.ctrl.rfSrc1 === io.BypassPktTable(i).decodePkt.rd && io.BypassPktTable(i).decodePkt.rdvalid && io.BypassPktValid(i) && i1rs1valid
//      i1rs2HitSel(i) := io.in(1).bits.ctrl.rfSrc2 === io.BypassPktTable(i).decodePkt.rd && io.BypassPktTable(i).decodePkt.rdvalid && io.BypassPktValid(i) && i1rs2valid
    }
    else{
      i0rs1HitSel(10) := true.B
      i0rs2HitSel(10) := true.B
      i1rs1HitSel(10) := true.B
      i1rs2HitSel(10) := true.B

    }
  }
    

    //merge decodePkt.subalu
    val i0Hiti1Rs1 = WireInit(false.B)
    val i0Hiti1Rs2 = WireInit(false.B)
    io.out0.bits.decodePkt.subalu :=
      (i0decodePkt.alu &&  i0rs1hitStage >= 0.U && i0rs1hitStage <= 3.U && (io.BypassPktTable(i0rs1hitStage).decodePkt.muldiv || io.BypassPktTable(i0rs1hitStage).decodePkt.load)
      || i0decodePkt.alu && i0rs2hitStage >= 0.U && i0rs2hitStage <= 3.U && (io.BypassPktTable(i0rs2hitStage).decodePkt.muldiv || io.BypassPktTable(i0rs2hitStage).decodePkt.load)
      || i0decodePkt.alu && i0rs1hitStage >= 0.U && i0rs1hitStage <= 5.U && io.BypassPktTable(i0rs1hitStage).decodePkt.subalu
      || i0decodePkt.alu && i0rs2hitStage >= 0.U && i0rs2hitStage <= 5.U && io.BypassPktTable(i0rs2hitStage).decodePkt.subalu
      ||i0Hiti1Rs1 || i0Hiti1Rs2
      )

    io.out1.bits.decodePkt.subalu :=
      (i1decodePkt.alu && i1rs1hitStage >= 0.U && i1rs1hitStage <= 3.U && (io.BypassPktTable(i1rs1hitStage).decodePkt.muldiv || io.BypassPktTable(i1rs1hitStage).decodePkt.load)
      || i1decodePkt.alu && i1rs2hitStage >= 0.U && i1rs2hitStage <= 3.U && (io.BypassPktTable(i1rs2hitStage).decodePkt.muldiv || io.BypassPktTable(i1rs2hitStage).decodePkt.load)
      || i1decodePkt.alu && i1rs1hitStage >= 0.U && i1rs1hitStage <= 5.U && io.BypassPktTable(i1rs1hitStage).decodePkt.subalu
      || i1decodePkt.alu && i1rs2hitStage >= 0.U && i1rs2hitStage <= 5.U && io.BypassPktTable(i1rs2hitStage).decodePkt.subalu
      )

    //issue stall

  val FuType = VecInit(Seq.fill(10)(0.U.asTypeOf(new decodePkt)))
  for (i <- 0 to 9) FuType(i) := io.BypassPktTable(i).decodePkt
  val Valid = VecInit(Seq.fill(10)(false.B))
  for (i <- 0 to 9) Valid(i) := io.BypassPktValid(i)

  val instInPipe =  Valid(0) || Valid(1) || Valid(2) || Valid(3) || Valid(4) ||
    Valid(5) || Valid(6) || Valid(7) || Valid(8) || Valid(9)

  io.issueStall(0) := (io.in(0).bits.ctrl.rfSrc1 === i1decodePkt.rd && i0rs1valid ||
    io.in(0).bits.ctrl.rfSrc2 === i1decodePkt.rd && i0rs2valid) && i1decodePkt.rdvalid && i1decodePkt.alu && io.out1.bits.decodePkt.subalu ||
    (i0decodePkt.load || i0decodePkt.store) &&  (i1decodePkt.load || i1decodePkt.store) || (i0decodePkt.csr && i1decodePkt.csr) ||
    (i1decodePkt.csr && i0decodePkt.branch) ||
    i0decodePkt.csr ||
    i0decodePkt.muldiv &&
      (i0Hiti1Rs1 || i0Hiti1Rs2 ||
        (i0rs1hitStage >= 4.U && i0rs1hitStage <= 5.U) && FuType(i0rs1hitStage).subalu ||
        (i0rs2hitStage >= 4.U && i0rs2hitStage <= 5.U) && FuType(i0rs2hitStage).subalu ||
        (i0rs1hitStage >= 0.U && i0rs1hitStage <= 3.U) && (FuType(i0rs1hitStage).muldiv || FuType(i0rs1hitStage).subalu || FuType(i0rs1hitStage).load) ||
        (i0rs2hitStage >= 0.U && i0rs2hitStage <= 3.U) && (FuType(i0rs2hitStage).muldiv || FuType(i0rs2hitStage).subalu || FuType(i0rs2hitStage).load)) ||
    i0decodePkt.load &&
      (i0Hiti1Rs1 ||
        (i0rs1hitStage >= 0.U && i0rs1hitStage <= 3.U) && FuType(i0rs1hitStage).subalu ||
        (i0rs1hitStage >= 0.U && i0rs1hitStage <= 1.U) && (FuType(i0rs1hitStage).muldiv || FuType(i0rs1hitStage).load || FuType(i0rs1hitStage).csr)) ||
  i0decodePkt.store && ( i0Hiti1Rs1 || i0Hiti1Rs2 ||    //the condition when store instruction does not meet the launch request
      i0rs1hitStage === 0.U && (FuType(0).subalu || FuType(0).load || FuType(0).muldiv) ||
      i0rs1hitStage === 1.U && (FuType(1).subalu || FuType(1).load || FuType(1).muldiv) ||
      i0rs2hitStage === 0.U && FuType(0).subalu ||
      i0rs2hitStage === 1.U && FuType(1).subalu
      ) ||
    io.issueStall(1)


  io.issueStall(1) :=
    i1decodePkt.csr && instInPipe ||
     (i1decodePkt.muldiv) &&
      ((i1rs1hitStage >= 4.U && i1rs1hitStage <= 5.U) && FuType(i1rs1hitStage).subalu ||
        (i1rs2hitStage >= 4.U && i1rs2hitStage <= 5.U) && FuType(i1rs2hitStage).subalu ||
        (i1rs1hitStage >= 0.U && i1rs1hitStage <= 3.U) && (FuType(i1rs1hitStage).muldiv || FuType(i1rs1hitStage).subalu || FuType(i1rs1hitStage).load) ||
        (i1rs2hitStage >= 0.U && i1rs2hitStage <= 3.U) && (FuType(i1rs2hitStage).muldiv || FuType(i1rs2hitStage).subalu || FuType(i1rs2hitStage).load)) ||
     i1decodePkt.load &&
       ((i1rs1hitStage >= 0.U && i1rs1hitStage <= 3.U) && FuType(i1rs1hitStage).subalu ||
        (i1rs1hitStage >= 0.U && i1rs1hitStage <= 1.U) && (FuType(i1rs1hitStage).muldiv || FuType(i1rs1hitStage).load) ||
         (i1rs1hitStage >= 0.U && i1rs1hitStage <= 1.U) && (FuType(i1rs1hitStage).csr || FuType(i1rs1hitStage).load)) ||
    i1decodePkt.store && (
        i1rs1hitStage === 0.U && (FuType(0).subalu || FuType(0).load || FuType(0).muldiv) ||
        i1rs1hitStage === 1.U && (FuType(1).subalu || FuType(1).load || FuType(1).muldiv) ||
        i1rs1hitStage === 2.U && FuType(2).subalu ||
        i1rs1hitStage === 3.U && FuType(3).subalu ||
        i1rs2hitStage === 0.U && FuType(0).subalu ||
        i1rs2hitStage === 1.U && FuType(1).subalu
        )


  //Signal to PMU
  //Normal Bound
  dontTouch(io.pmuio)
  io.pmuio.normali0 := io.in(0).fire()
  io.pmuio.normali1 := io.in(1).fire()
  //Wrong Prediction Bound

  //Frontend Bound
  io.pmuio.frontendi0 := !io.in(0).valid
  io.pmuio.frontendi1 := !io.in(1).valid
  //Backend Bound (issue stall)
  io.pmuio.laterStageStalli0 := (io.memStall || io.mduStall) && io.in(0).valid
  io.pmuio.laterStageStalli1 := (io.memStall || io.mduStall) && io.in(1).valid


  io.pmuio.loadRsNotreadyi1 := io.in(1).valid && i1decodePkt.load && io.issueStall(1) && !io.pmuio.laterStageStalli1
  //loadRsNotreadyi10 <- subalu, loadRsNotreadyi11 <- mul/div, loadRsNotreadyi12 <- load
  io.pmuio.load_sub_rs1_i1 := io.in(1).valid && i1decodePkt.load && io.issueStall(1) && !io.pmuio.laterStageStalli1 && FuType(i1rs1hitStage).subalu
  io.pmuio.load_md_rs1_i1 := io.in(1).valid && i1decodePkt.load && io.issueStall(1) && !io.pmuio.laterStageStalli1 && FuType(i1rs1hitStage).muldiv
  io.pmuio.load_load_rs1_i1 := io.in(1).valid && i1decodePkt.load && io.issueStall(1) && !io.pmuio.laterStageStalli1 && FuType(i1rs1hitStage).load
  io.pmuio.load_sub_rs2_i1 := io.in(1).valid && i1decodePkt.load && io.issueStall(1) && !io.pmuio.laterStageStalli1 && FuType(i1rs2hitStage).subalu
  io.pmuio.load_md_rs2_i1 := io.in(1).valid && i1decodePkt.load && io.issueStall(1) && !io.pmuio.laterStageStalli1 && FuType(i1rs2hitStage).muldiv
  io.pmuio.load_load_rs2_i1 := io.in(1).valid && i1decodePkt.load && io.issueStall(1) && !io.pmuio.laterStageStalli1 && FuType(i1rs2hitStage).load

  io.pmuio.storeRsNotreadyi1 := io.in(1).valid && i1decodePkt.store && io.issueStall(1) && !io.pmuio.laterStageStalli1
  io.pmuio.mulRsNotreadyi1 := i1decodePkt.muldiv && !MDUOpType.isDiv(io.in(1).bits.ctrl.fuOpType) && io.issueStall(1) && !io.pmuio.laterStageStalli1
  io.pmuio.divRsNotreadyi1 := i1decodePkt.muldiv && MDUOpType.isDiv(io.in(1).bits.ctrl.fuOpType) && io.issueStall(1) && !io.pmuio.laterStageStalli1

  io.pmuio.i1Stalli0 := io.in(0).valid  && !io.pmuio.laterStageStalli0 && io.issueStall(1)
  io.pmuio.bothLsui0 := io.in(0).valid  && !io.pmuio.laterStageStalli0 && !io.pmuio.i1Stalli0 &&
    (i0decodePkt.load || i0decodePkt.store) && (i1decodePkt.load || i1decodePkt.store)
  io.pmuio.bothBrui0 := io.in(0).valid  && !io.pmuio.laterStageStalli0 && !io.pmuio.i1Stalli0 && i1decodePkt.branch && i0decodePkt.branch
  io.pmuio.LsuBri0 := false.B
  io.pmuio.hitSubalui0 := io.in(0).valid && !io.pmuio.laterStageStalli0 && (io.in(0).bits.ctrl.rfSrc1 === i1decodePkt.rd && i0rs1valid ||
    io.in(0).bits.ctrl.rfSrc2 === i1decodePkt.rd && i0rs2valid) && i1decodePkt.rdvalid && i1decodePkt.alu && io.out1.bits.decodePkt.subalu && !io.pmuio.i1Stalli0 &&
    !io.pmuio.bothLsui0 && !io.pmuio.LsuBri0 && !io.pmuio.bothBrui0

  val ruleStall = io.pmuio.i1Stalli0 || io.pmuio.bothLsui0 || io.pmuio.LsuBri0 || io.pmuio.hitSubalui0
  io.pmuio.loadRsNotreadyi0 := io.in(0).valid && i0decodePkt.load && io.issueStall(0) && !io.pmuio.laterStageStalli0 && !ruleStall
  io.pmuio.storeRsNotreadyi0 := io.in(0).valid && i0decodePkt.store && io.issueStall(0) && !io.pmuio.laterStageStalli0 && !ruleStall
  io.pmuio.mulRsNotreadyi0 := i0decodePkt.muldiv && !MDUOpType.isDiv(io.in(0).bits.ctrl.fuOpType) && io.in(0).valid && io.issueStall(0) && !io.pmuio.laterStageStalli0 && !ruleStall
  io.pmuio.divRsNotreadyi0 := i0decodePkt.muldiv && MDUOpType.isDiv(io.in(0).bits.ctrl.fuOpType) && io.in(0).valid && io.issueStall(0) && !io.pmuio.laterStageStalli0 && !ruleStall




    //BypassPkt out0
  i0Hiti1Rs1 := io.in(0).bits.ctrl.rfSrc1 === i1decodePkt.rd && i0rs1valid && i1decodePkt.rdvalid //&& (i1decodePkt.muldiv || i1decodePkt.load || i1decodePkt.alu && !io.out1.bits.decodePkt.subalu)
  i0Hiti1Rs2 := io.in(0).bits.ctrl.rfSrc2 === i1decodePkt.rd && i0rs2valid && i1decodePkt.rdvalid //&& (i1decodePkt.muldiv || i1decodePkt.load || i1decodePkt.alu && !io.out1.bits.decodePkt.subalu)

  io.out0.bits.BypassCtl.rs1bypasse0 := VecInit(
//       i0rs1hitStage === 0.U && FuType(0).alu && !FuType(0).subalu && !i0Hiti1Rs1,
       i0rs1hitStage === 0.U && (FuType(0).alu && !FuType(0).subalu || FuType(0).csr) && !i0Hiti1Rs1,
//       i0rs1hitStage === 1.U && FuType(1).alu && !FuType(1).subalu && !i0Hiti1Rs1,
       i0rs1hitStage === 1.U && (FuType(1).alu && !FuType(1).subalu || FuType(1).csr) && !i0Hiti1Rs1,
       i0rs1hitStage === 2.U && (FuType(2).alu && !FuType(2).subalu || FuType(2).csr) && !i0Hiti1Rs1,
       i0rs1hitStage === 3.U && (FuType(3).alu && !FuType(3).subalu || FuType(3).csr) && !i0Hiti1Rs1,
       i0rs1hitStage === 4.U && (FuType(4).alu && !FuType(4).subalu || FuType(4).csr) && !i0Hiti1Rs1,
       i0rs1hitStage === 5.U && (FuType(5).alu && !FuType(5).subalu || FuType(5).csr) && !i0Hiti1Rs1,
      (i0rs1hitStage === 4.U && FuType(4).load || i0rs1hitStage === 5.U && FuType(5).load) && !i0Hiti1Rs1,
      (i0rs1hitStage === 4.U && FuType(4).muldiv  || i0rs1hitStage === 5.U && FuType(5).muldiv) && !i0Hiti1Rs1,
       i0rs1hitStage === 6.U && !i0Hiti1Rs1,
       i0rs1hitStage === 7.U && !i0Hiti1Rs1,
       i0rs1hitStage === 8.U && !i0Hiti1Rs1,
       i0rs1hitStage === 9.U && !i0Hiti1Rs1
    )
  io.out0.bits.BypassCtl.rs2bypasse0 := VecInit(
//        i0rs2hitStage === 0.U && FuType(0).alu && !FuType(0).subalu && !i0Hiti1Rs2,
//        i0rs2hitStage === 1.U && FuType(1).alu && !FuType(1).subalu && !i0Hiti1Rs2,
        i0rs2hitStage === 0.U && (FuType(0).alu && !FuType(0).subalu || FuType(0).csr) && !i0Hiti1Rs2,
        i0rs2hitStage === 1.U && (FuType(1).alu && !FuType(1).subalu || FuType(1).csr) && !i0Hiti1Rs2,
        i0rs2hitStage === 2.U && (FuType(2).alu && !FuType(2).subalu || FuType(2).csr) && !i0Hiti1Rs2,
        i0rs2hitStage === 3.U && (FuType(3).alu && !FuType(3).subalu || FuType(3).csr) && !i0Hiti1Rs2,
        i0rs2hitStage === 4.U && (FuType(4).alu && !FuType(4).subalu || FuType(4).csr) && !i0Hiti1Rs2,
        i0rs2hitStage === 5.U && (FuType(5).alu && !FuType(5).subalu || FuType(5).csr) && !i0Hiti1Rs2,
       (i0rs2hitStage === 4.U && FuType(4).load || i0rs2hitStage === 5.U && FuType(5).load) && !i0Hiti1Rs2,
       (i0rs2hitStage === 4.U && FuType(4).muldiv  || i0rs2hitStage === 5.U && FuType(5).muldiv) && !i0Hiti1Rs2,
        i0rs2hitStage === 6.U && !i0Hiti1Rs2,
        i0rs2hitStage === 7.U && !i0Hiti1Rs2,
        i0rs2hitStage === 8.U && !i0Hiti1Rs2,
        i0rs2hitStage === 9.U && !i0Hiti1Rs2
    )
    io.out0.bits.BypassCtl.rs1bypasse2 := VecInit(
      i0rs1hitStage === 4.U && FuType(4).subalu && !i0Hiti1Rs1,
      i0rs1hitStage === 5.U && FuType(5).subalu && !i0Hiti1Rs1
    )
    io.out0.bits.BypassCtl.rs2bypasse2 := Seq(
      i0rs2hitStage === 4.U && FuType(4).subalu && !i0Hiti1Rs2,
      i0rs2hitStage === 5.U && FuType(5).subalu && !i0Hiti1Rs2
    )

    io.out0.bits.BypassCtl.rs1bypasse3 := VecInit(
      i0Hiti1Rs1,
      i0rs1hitStage === 0.U && (FuType(0).subalu || FuType(0).load || FuType(0).muldiv) && !i0Hiti1Rs1,
      i0rs1hitStage === 1.U && (FuType(1).subalu || FuType(1).load || FuType(1).muldiv) && !i0Hiti1Rs1,
      i0rs1hitStage === 2.U && (FuType(2).subalu || FuType(2).load || FuType(2).muldiv) && !i0Hiti1Rs1,
      i0rs1hitStage === 3.U && (FuType(3).subalu || FuType(3).load || FuType(3).muldiv) && !i0Hiti1Rs1
    )


    io.out0.bits.BypassCtl.rs2bypasse3 := VecInit(
      i0Hiti1Rs2,
      i0rs2hitStage === 0.U && (FuType(0).subalu || FuType(0).load || FuType(0).muldiv) && !i0Hiti1Rs2,
      i0rs2hitStage === 1.U && (FuType(1).subalu || FuType(1).load || FuType(1).muldiv) && !i0Hiti1Rs2,
      i0rs2hitStage === 2.U && (FuType(2).subalu || FuType(2).load || FuType(2).muldiv) && !i0Hiti1Rs2,
      i0rs2hitStage === 3.U && (FuType(3).subalu || FuType(3).load || FuType(3).muldiv) && !i0Hiti1Rs2
    )
    //BypassPkt out1
    io.out1.bits.BypassCtl.rs1bypasse0 := VecInit(
       i1rs1hitStage === 0.U && (FuType(0).alu && !FuType(0).subalu || FuType(0).csr),
       i1rs1hitStage === 1.U && (FuType(1).alu && !FuType(1).subalu || FuType(1).csr),
       i1rs1hitStage === 2.U && (FuType(2).alu && !FuType(2).subalu || FuType(2).csr),
       i1rs1hitStage === 3.U && (FuType(3).alu && !FuType(3).subalu || FuType(3).csr),
       i1rs1hitStage === 4.U && (FuType(4).alu && !FuType(4).subalu || FuType(4).csr),
       i1rs1hitStage === 5.U && (FuType(5).alu && !FuType(5).subalu || FuType(5).csr),
      (i1rs1hitStage === 4.U && FuType(4).load || i1rs1hitStage === 5.U && FuType(5).load),
      (i1rs1hitStage === 4.U && FuType(4).muldiv  || i1rs1hitStage === 5.U && FuType(5).muldiv),
       i1rs1hitStage === 6.U,
       i1rs1hitStage === 7.U,
       i1rs1hitStage === 8.U,
       i1rs1hitStage === 9.U
    )
    io.out1.bits.BypassCtl.rs2bypasse0 := VecInit(
       i1rs2hitStage === 0.U && (FuType(0).alu && !FuType(0).subalu || FuType(0).csr),
       i1rs2hitStage === 1.U && (FuType(1).alu && !FuType(1).subalu || FuType(1).csr),
       i1rs2hitStage === 2.U && (FuType(2).alu && !FuType(2).subalu || FuType(2).csr),
       i1rs2hitStage === 3.U && (FuType(3).alu && !FuType(3).subalu || FuType(3).csr),
       i1rs2hitStage === 4.U && (FuType(4).alu && !FuType(4).subalu || FuType(4).csr),
       i1rs2hitStage === 5.U && (FuType(5).alu && !FuType(5).subalu || FuType(5).csr),
      (i1rs2hitStage === 4.U && FuType(4).load || i1rs2hitStage === 5.U && FuType(5).load),
      (i1rs2hitStage === 4.U && FuType(4).muldiv || i1rs2hitStage === 5.U && FuType(5).muldiv),
       i1rs2hitStage === 6.U,
       i1rs2hitStage === 7.U,
       i1rs2hitStage === 8.U,
       i1rs2hitStage === 9.U
    )
    io.out1.bits.BypassCtl.rs1bypasse2 := VecInit(
      i1rs1hitStage === 4.U && FuType(4).subalu,
      i1rs1hitStage === 5.U && FuType(5).subalu
    )
    io.out1.bits.BypassCtl.rs2bypasse2 := VecInit(
      i1rs2hitStage === 4.U && FuType(4).subalu,
      i1rs2hitStage === 5.U && FuType(5).subalu
    )
  io.out1.bits.BypassCtl.rs1bypasse3 := VecInit(
    false.B,
    i1rs1hitStage === 0.U && (FuType(0).subalu || FuType(0).load || FuType(0).muldiv),
    i1rs1hitStage === 1.U && (FuType(1).subalu || FuType(1).load || FuType(1).muldiv),
    i1rs1hitStage === 2.U && (FuType(2).subalu || FuType(2).load || FuType(2).muldiv),
    i1rs1hitStage === 3.U && (FuType(3).subalu || FuType(3).load || FuType(3).muldiv)
  )
  io.out1.bits.BypassCtl.rs2bypasse3 := VecInit(
    false.B,
    i1rs2hitStage === 0.U && (FuType(0).subalu || FuType(0).load || FuType(0).muldiv),
    i1rs2hitStage === 1.U && (FuType(1).subalu || FuType(1).load || FuType(1).muldiv),
    i1rs2hitStage === 2.U && (FuType(2).subalu || FuType(2).load || FuType(2).muldiv),
    i1rs2hitStage === 3.U && (FuType(3).subalu || FuType(3).load || FuType(3).muldiv)
  )

  // store pipeline bypaas ctrl
  lsuCtrli0.lsBypassCtrlE1 := VecInit(
    i0rs1hitStage === 2.U && (FuType(2).load || FuType(2).muldiv) && (i0decodePkt.store || i0decodePkt.load),
    i0rs1hitStage === 3.U && (FuType(3).load || FuType(3).muldiv) && (i0decodePkt.store || i0decodePkt.load),
    i0rs1hitStage === 4.U && FuType(4).subalu && (i0decodePkt.store || i0decodePkt.load),
    i0rs1hitStage === 5.U && FuType(5).subalu && (i0decodePkt.store || i0decodePkt.load)
  )
  lsuCtrli0.storeBypassCtrlE2 := VecInit(
    i0rs2hitStage === 0.U && (FuType(0).load || FuType(0).muldiv) && i0decodePkt.store,
    i0rs2hitStage === 1.U && (FuType(1).load || FuType(1).muldiv) && i0decodePkt.store,
    i0rs2hitStage === 2.U && (FuType(2).subalu || FuType(2).load || FuType(2).muldiv) && i0decodePkt.store,
    i0rs2hitStage === 3.U && (FuType(3).subalu || FuType(3).load || FuType(3).muldiv) && i0decodePkt.store,
    i0rs2hitStage === 4.U && (FuType(4).subalu) && i0decodePkt.store,
    i0rs2hitStage === 5.U && (FuType(5).subalu) && i0decodePkt.store
  )
  lsuCtrli1.lsBypassCtrlE1 := VecInit(
    i1rs1hitStage === 2.U && (FuType(2).load || FuType(2).muldiv) && (i1decodePkt.store || i1decodePkt.load),
    i1rs1hitStage === 3.U && (FuType(3).load || FuType(3).muldiv) && (i1decodePkt.store || i1decodePkt.load),
    i1rs1hitStage === 4.U && FuType(4).subalu && (i1decodePkt.store || i1decodePkt.load),
    i1rs1hitStage === 5.U && FuType(5).subalu && (i1decodePkt.store || i1decodePkt.load)
  )
  lsuCtrli1.storeBypassCtrlE2 := VecInit(
    i1rs2hitStage === 0.U && (FuType(0).load || FuType(0).muldiv) && i1decodePkt.store,
    i1rs2hitStage === 1.U && (FuType(1).load || FuType(1).muldiv) && i1decodePkt.store,
    i1rs2hitStage === 2.U && (FuType(2).subalu || FuType(2).load || FuType(2).muldiv) && i1decodePkt.store,
    i1rs2hitStage === 3.U && (FuType(3).subalu || FuType(3).load || FuType(3).muldiv) && i1decodePkt.store,
    i1rs2hitStage === 4.U && (FuType(4).subalu) && i1decodePkt.store,
    i1rs2hitStage === 5.U && (FuType(5).subalu) && i1decodePkt.store
  )
  dontTouch(lsuCtrli0)
  dontTouch(lsuCtrli1)
  io.out0.bits.lsuCtrl := lsuCtrli0
  io.out1.bits.lsuCtrl := lsuCtrli1

  //debug
/*  val cond = Wire(Bool())
  cond := io.in(0).bits.cf.pc === "h8000008c".U
  myDebug(cond,"rs1e3BypassCtl is %b\n",io.out0.bits.BypassCtl.rs1bypasse3.asUInt)
  myDebug(cond,"rs1hitstage is %x\n",i0rs1hitStage)
  myDebug(cond,"rs2hitstage is %x\n",i0rs2hitStage)
  myDebug(cond,"hitrd is%x, rs1 is %x,rs2 is %x\n",io.BypassPktTable(i0rs1hitStage).decodePkt.rd,io.in(0).bits.ctrl.rfSrc1,io.in(0).bits.ctrl.rfSrc2)
  myDebug(cond,"i0hiti1:%b\n",(io.in(1).bits.ctrl.rfSrc1 === i1decodePkt.rd).asUInt)*/
  //myDebug(cond,"hitsel is %b\n",i0rs1HitSel.asUInt)
}

object DecodeIO2decodePkt {
  def apply(in:DecodeIO,out:decodePkt){
    out.rd := Mux(in.ctrl.rdValid,in.ctrl.rfDest,0.U)
    out.rdvalid <> in.ctrl.rdValid
    out.alu := in.ctrl.fuType === FuType.alu || in.ctrl.fuType === FuType.bru
    //subalu 会在其他模块覆盖掉
    out.muldiv := in.ctrl.fuType === FuType.mdu
    out.load := LSUOpType.isLoad(in.ctrl.fuOpType) && in.ctrl.fuType === FuType.lsu
    out.store := LSUOpType.isStore(in.ctrl.fuOpType) && in.ctrl.fuType === FuType.lsu
    out.subalu := false.B
    out.branch := ALUOpType.isBru(in.ctrl.fuOpType) && in.ctrl.fuType === FuType.alu
    out.csr    := in.ctrl.fuType === FuType.csr
    out.skip   := in.cf.instr =/= 0x7b.U
  }
}

class PipeCtl extends Module{
  val io = IO(new Bundle{
    val i0pipeStall = Input(Bool())
    val i1pipeStall = Input(Bool())
    val memStall = Input(Bool())
    val flush = Input(Vec(4,Bool()))  //flushPoint(0,3) -> alu0,alu1,sub_alu0,sub_alu1
  })
  val pipeCtl = IO(new StallFlushIO)
  dontTouch(io.flush)

  // stall vec
  pipeCtl.stall(0) := io.i1pipeStall
  pipeCtl.stall(1) := io.i0pipeStall
  pipeCtl.stall(2) := io.memStall

  //flush/invalid vec
  val allStageFlushVec = VecInit(Seq.fill(10)(false.B))
  val allStageInvalidVec = VecInit(Seq.fill(12)(false.B))
  //pipeline0 : 1,3,5,7,9
  //pipeline1 : 0,2,4,6,8
  val alu0InvalidList = List(0,1,2,3)
  val alu1InvalidList = List(0,1,2,3,4)
  val subalu0InvalidList = List(0,1,2,3,4,5,6,7,8,9)
  val subalu1InvalidList = List(0,1,2,3,4,5,6,7,8,9,10)

  val alu0FlushList = List(0,1)
  val alu1FlushList = List(0,1,2)
  val subalu0FlushList = List(0,1,2,3,4,5,6,7)
  val subalu1FlushList = List(0,1,2,3,4,5,6,7,8)


  alu0InvalidList.foreach{ case i => when(io.flush(0) === true.B){allStageInvalidVec(i) := io.flush(0)}}
  alu1InvalidList.foreach{ case i => when(io.flush(1) === true.B){allStageInvalidVec(i) := io.flush(1)}}
  subalu0InvalidList.foreach{ case i => when(io.flush(2) === true.B){allStageInvalidVec(i) := io.flush(2)}}
  subalu1InvalidList.foreach{ case i => when(io.flush(3) === true.B){allStageInvalidVec(i) := io.flush(3)}}

  alu0FlushList.foreach{ case i => when(io.flush(0) === true.B){allStageFlushVec(i) := io.flush(0)}}
  alu1FlushList.foreach{ case i => when(io.flush(1) === true.B){allStageFlushVec(i) := io.flush(1)}}
  subalu0FlushList.foreach{ case i => when(io.flush(2) === true.B){allStageFlushVec(i) := io.flush(2)}}
  subalu1FlushList.foreach{ case i => when(io.flush(3) === true.B){allStageFlushVec(i) := io.flush(3)}}

  pipeCtl.invalid := allStageInvalidVec
  pipeCtl.flush := allStageFlushVec


}
class Bypass extends Module{
  val io = IO(new Bundle {
    val in = Vec(2,Flipped(Decoupled(new DecodeIO)))
    val memStall = Input(Bool())
    val mduStall = Input(Bool())
    val flush = Input(Vec(4,Bool()))
    val issueStall = Output(Vec(2,Bool()))
    val pipeFlush = Output(Vec(10,Bool()))
    val pipeInvalid = Output(Vec(12,Bool()))
    val decodeBypassPkt = Vec(2, Decoupled(new BypassPkt))
    val BypassPkt = Vec(10, new BypassPkt)
    val BypassPktValid = Output(Vec(10,Bool()))
    val pmuio = new PMUIO0
    val LSUBypassCtrl = new LSUBypassCtrl
  })

  val PipelineCtl = Module(new PipeCtl)
  val DecodeIO2BypassPkt = Module(new DecodeIO2BypassPkt)

  //PktPipeline
  val pipeIn = Wire(Vec(10,Decoupled(new BypassPkt)))
  val pipeOut = Wire(Vec(10,Decoupled(new BypassPkt)))
  val pipeFire = VecInit(Seq.fill(10)(false.B))
  //PktPipeline in ,out & fire
  pipeIn(0) := DecodeIO2BypassPkt.io.out0
  pipeIn(1) := DecodeIO2BypassPkt.io.out1

  for (i <- 2 to 9) {
    pipeIn(i).bits := pipeOut(i - 2).bits
    pipeOut(i - 2).ready := pipeIn(i).ready
    pipeIn(i).valid := pipeOut(i - 2).valid
  }

  for (i <- 0 to 9) {
    if (i == 8 || i == 9) pipeFire(i) := true.B
    else pipeFire(i) := pipeOut(i).valid && pipeIn(i + 2).ready
  }

  //ready & valid
  pipeOut(8).ready := true.B
  pipeOut(9).ready := true.B

  val BypassPkt = Wire(Vec(10, new BypassPkt))
  val BypassPktValid = Wire(Vec(10,Bool()))
  for(i <- 0 to 9) BypassPkt(i) := pipeOut(i).bits
  for(i <- 0 to 9) BypassPktValid(i) := pipeOut(i).valid && !io.pipeInvalid(i+2)

  PipelineCtl.io.i0pipeStall <>  DecodeIO2BypassPkt.io.issueStall(0)
  PipelineCtl.io.i1pipeStall <>  DecodeIO2BypassPkt.io.issueStall(1)
  PipelineCtl.io.flush <> io.flush
  PipelineCtl.io.memStall <> io.memStall
  DecodeIO2BypassPkt.io.BypassPktTable := BypassPkt
  DecodeIO2BypassPkt.io.BypassPktValid := BypassPktValid
  io.BypassPkt := BypassPkt
  io.BypassPktValid := BypassPktValid
  DecodeIO2BypassPkt.io.in(0) <> io.in(1)
  DecodeIO2BypassPkt.io.in(1) <> io.in(0)
  DecodeIO2BypassPkt.io.memStall <> io.memStall
  DecodeIO2BypassPkt.io.mduStall <> io.mduStall
  DecodeIO2BypassPkt.io.pmuio <> io.pmuio


  io.issueStall := DecodeIO2BypassPkt.io.issueStall
  io.decodeBypassPkt <> Seq(DecodeIO2BypassPkt.io.out0,DecodeIO2BypassPkt.io.out1)
  io.pipeFlush := PipelineCtl.pipeCtl.flush
  io.pipeInvalid := PipelineCtl.pipeCtl.invalid

  //LSU pipeline bypass ctrl
  io.LSUBypassCtrl.lsBypassCtrli0E1 := Mux(pipeOut(0).valid && (BypassPkt(0).decodePkt.store || BypassPkt(0).decodePkt.load),pipeOut(0).bits.lsuCtrl.lsBypassCtrlE1,
    0.U.asTypeOf(new LSUPipeBypassCtrl).lsBypassCtrlE1)
  io.LSUBypassCtrl.lsBypassCtrli1E1 := Mux(pipeOut(1).valid && (BypassPkt(1).decodePkt.store || BypassPkt(1).decodePkt.load),pipeOut(1).bits.lsuCtrl.lsBypassCtrlE1,
      0.U.asTypeOf(new LSUPipeBypassCtrl).lsBypassCtrlE1)
  io.LSUBypassCtrl.storeBypassCtrlE2 := Mux(pipeOut(3).valid && BypassPkt(3).decodePkt.store,pipeOut(3).bits.lsuCtrl.storeBypassCtrlE2,
    Mux(pipeOut(2).valid && BypassPkt(2).decodePkt.store,pipeOut(2).bits.lsuCtrl.storeBypassCtrlE2,
      0.U.asTypeOf(new LSUPipeBypassCtrl).storeBypassCtrlE2))

  //pipeline connect
  //stall stage
  val pipeStage0 = Module(new stallPointConnect(new BypassPkt)).suggestName("pipeStage0")
  val pipeStage1 = Module(new stallPointConnect(new BypassPkt)).suggestName("pipeStage1")
  val pipeStage6 = Module(new stallPointConnect(new BypassPkt)).suggestName("pipeStage6")
  val pipeStage7 = Module(new stallPointConnect(new BypassPkt)).suggestName("pipeStage7")

  val stallStageList = List(pipeStage0,pipeStage1,pipeStage6,pipeStage7)
  val stallList = List(0,1,6,7)
  (stallStageList zip stallList).foreach{case (a,b) =>
    a.io.left <> pipeIn(b)
    a.io.right <> pipeOut(b)
    a.io.rightOutFire <> pipeFire(b)
    a.io.isFlush <> PipelineCtl.pipeCtl.flush(b)
    a.io.inValid <> PipelineCtl.pipeCtl.invalid(b)
  }
  pipeStage0.io.isStall := DecodeIO2BypassPkt.io.issueStall(0)
  pipeStage1.io.isStall := DecodeIO2BypassPkt.io.issueStall(1)
  pipeStage6.io.isStall := io.memStall || io.mduStall
  pipeStage7.io.isStall := io.memStall || io.mduStall
//  pipeStage2.io.isStall := io.lsuS2Stall
//  pipeStage3.io.isStall := io.lsuS2Stall
//  pipeStage4.io.isStall := io.lsuS3Stall
//  pipeStage5.io.isStall := io.lsuS3Stall
//  pipeStage6.io.isStall := io.memStall
//  pipeStage7.io.isStall := io.memStall

  //normal stage
  val pipeStage2 = Module(new normalPipeConnect(new BypassPkt)).suggestName("pipeStage2")
  val pipeStage3 = Module(new normalPipeConnect(new BypassPkt)).suggestName("pipeStage3")
  val pipeStage4 = Module(new normalPipeConnect(new BypassPkt)).suggestName("pipeStage4")
  val pipeStage5 = Module(new normalPipeConnect(new BypassPkt)).suggestName("pipeStage5")
  val pipeStage8 = Module(new normalPipeConnect(new BypassPkt)).suggestName("pipeStage8")
  val pipeStage9 = Module(new normalPipeConnect(new BypassPkt)).suggestName("pipeStage9")

  val normalStageList = List(pipeStage2,pipeStage3,pipeStage4,pipeStage5,pipeStage8,pipeStage9)
  val noralList = List(2,3,4,5,8,9)

  (normalStageList zip noralList).foreach{case (a,b) =>
    a.io.left <> pipeIn(b)
    a.io.right <> pipeOut(b)
    a.io.rightOutFire <> pipeFire(b)
    a.io.isFlush <> PipelineCtl.pipeCtl.flush(b)
    a.io.inValid <> PipelineCtl.pipeCtl.invalid(b)
  }


}




