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

package XiaoHe.SSDbackend.backend

import XiaoHe.SSDbackend._
import XiaoHe.SSDbackend.fu.{ALUOpType, LSUOpType, MDUOpType, SSDMOU}
import XiaoHe._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import chisel3.{Flipped, Module, _}
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

//  dontTouch(i0rs1validTmp)
//  dontTouch(i0rs2validTmp)
//  dontTouch(i1rs1validTmp)
//  dontTouch(i1rs2validTmp)

  //hit stage
  val i0rs1hitStage = WireInit(10.U(4.W))
  val i0rs2hitStage = WireInit(10.U(4.W))
  val i1rs1hitStage = WireInit(10.U(4.W))
  val i1rs2hitStage = WireInit(10.U(4.W))
//  dontTouch(i0rs1hitStage)
//  dontTouch(i0rs2hitStage)
//  dontTouch(i1rs1hitStage)
//  dontTouch(i1rs2hitStage)


  val i0rs1HitSel = VecInit(Seq.fill(11)(false.B))
  val i0rs2HitSel = VecInit(Seq.fill(11)(false.B))
  val i1rs1HitSel = VecInit(Seq.fill(11)(false.B))
  val i1rs2HitSel = VecInit(Seq.fill(11)(false.B))
  val i1Hiti0Rs1 = WireInit(false.B)
  val i1Hiti0Rs2 = WireInit(false.B)

  val stageSeq = Seq(0.U,1.U,2.U,3.U,4.U,5.U,6.U,7.U,8.U,9.U,10.U)//10 is set for the default value of PriorityMux

  i0rs1hitStage := PriorityMux(i0rs1HitSel,stageSeq)
  i0rs2hitStage := PriorityMux(i0rs2HitSel,stageSeq)
  i1rs1hitStage := Mux(i1Hiti0Rs1, 10.U, PriorityMux(i1rs1HitSel,stageSeq))
  i1rs2hitStage := Mux(i1Hiti0Rs2, 10.U, PriorityMux(i1rs2HitSel,stageSeq))

  // for (i <- 0 to 10) {
  //   if (i <= 9) {
  //     i0rs1HitSel(i) := io.in(0).bits.ctrl.rfSrc1 === io.BypassPktTable(i+1).decodePkt.rd && io.BypassPktValid(i+1) && i0rs1valid
  //     i0rs2HitSel(i) := io.in(0).bits.ctrl.rfSrc2 === io.BypassPktTable(i+1).decodePkt.rd && io.BypassPktValid(i+1) && i0rs2valid
  //     i1rs1HitSel(i) := io.in(1).bits.ctrl.rfSrc1 === io.BypassPktTable(i+1).decodePkt.rd && io.BypassPktValid(i+1) && i1rs1valid
  //     i1rs2HitSel(i) := io.in(1).bits.ctrl.rfSrc2 === io.BypassPktTable(i+1).decodePkt.rd && io.BypassPktValid(i+1) && i1rs2valid
  //   }
  //   else{
  //
  //   }
  // }
  i0rs1HitSel := VecInit(io.in(0).bits.ctrl.rfSrc1 === io.BypassPktTable(1).decodePkt.rd && io.BypassPktValid(1) && i0rs1valid,
  io.in(0).bits.ctrl.rfSrc1 === io.BypassPktTable(0).decodePkt.rd && io.BypassPktValid(0) && i0rs1valid,
  io.in(0).bits.ctrl.rfSrc1 === io.BypassPktTable(3).decodePkt.rd && io.BypassPktValid(3) && i0rs1valid,
  io.in(0).bits.ctrl.rfSrc1 === io.BypassPktTable(2).decodePkt.rd && io.BypassPktValid(2) && i0rs1valid,
  io.in(0).bits.ctrl.rfSrc1 === io.BypassPktTable(5).decodePkt.rd && io.BypassPktValid(5) && i0rs1valid,
  io.in(0).bits.ctrl.rfSrc1 === io.BypassPktTable(4).decodePkt.rd && io.BypassPktValid(4) && i0rs1valid,
  io.in(0).bits.ctrl.rfSrc1 === io.BypassPktTable(7).decodePkt.rd && io.BypassPktValid(7) && i0rs1valid,
  io.in(0).bits.ctrl.rfSrc1 === io.BypassPktTable(6).decodePkt.rd && io.BypassPktValid(6) && i0rs1valid,
  io.in(0).bits.ctrl.rfSrc1 === io.BypassPktTable(9).decodePkt.rd && io.BypassPktValid(9) && i0rs1valid,
  io.in(0).bits.ctrl.rfSrc1 === io.BypassPktTable(8).decodePkt.rd && io.BypassPktValid(8) && i0rs1valid,
  true.B
  )
  i0rs2HitSel := VecInit(io.in(0).bits.ctrl.rfSrc2 === io.BypassPktTable(1).decodePkt.rd && io.BypassPktValid(1) && i0rs2valid,
  io.in(0).bits.ctrl.rfSrc2 === io.BypassPktTable(0).decodePkt.rd && io.BypassPktValid(0) && i0rs2valid,
  io.in(0).bits.ctrl.rfSrc2 === io.BypassPktTable(3).decodePkt.rd && io.BypassPktValid(3) && i0rs2valid,
  io.in(0).bits.ctrl.rfSrc2 === io.BypassPktTable(2).decodePkt.rd && io.BypassPktValid(2) && i0rs2valid,
  io.in(0).bits.ctrl.rfSrc2 === io.BypassPktTable(5).decodePkt.rd && io.BypassPktValid(5) && i0rs2valid,
  io.in(0).bits.ctrl.rfSrc2 === io.BypassPktTable(4).decodePkt.rd && io.BypassPktValid(4) && i0rs2valid,
  io.in(0).bits.ctrl.rfSrc2 === io.BypassPktTable(7).decodePkt.rd && io.BypassPktValid(7) && i0rs2valid,
  io.in(0).bits.ctrl.rfSrc2 === io.BypassPktTable(6).decodePkt.rd && io.BypassPktValid(6) && i0rs2valid,
  io.in(0).bits.ctrl.rfSrc2 === io.BypassPktTable(9).decodePkt.rd && io.BypassPktValid(9) && i0rs2valid,
  io.in(0).bits.ctrl.rfSrc2 === io.BypassPktTable(8).decodePkt.rd && io.BypassPktValid(8) && i0rs2valid,
  true.B
  )
  i1rs1HitSel := VecInit(io.in(1).bits.ctrl.rfSrc1 === io.BypassPktTable(1).decodePkt.rd && io.BypassPktValid(1) && i1rs1valid,
  io.in(1).bits.ctrl.rfSrc1 === io.BypassPktTable(0).decodePkt.rd && io.BypassPktValid(0) && i1rs1valid,
  io.in(1).bits.ctrl.rfSrc1 === io.BypassPktTable(3).decodePkt.rd && io.BypassPktValid(3) && i1rs1valid,
  io.in(1).bits.ctrl.rfSrc1 === io.BypassPktTable(2).decodePkt.rd && io.BypassPktValid(2) && i1rs1valid,
  io.in(1).bits.ctrl.rfSrc1 === io.BypassPktTable(5).decodePkt.rd && io.BypassPktValid(5) && i1rs1valid,
  io.in(1).bits.ctrl.rfSrc1 === io.BypassPktTable(4).decodePkt.rd && io.BypassPktValid(4) && i1rs1valid,
  io.in(1).bits.ctrl.rfSrc1 === io.BypassPktTable(7).decodePkt.rd && io.BypassPktValid(7) && i1rs1valid,
  io.in(1).bits.ctrl.rfSrc1 === io.BypassPktTable(6).decodePkt.rd && io.BypassPktValid(6) && i1rs1valid,
  io.in(1).bits.ctrl.rfSrc1 === io.BypassPktTable(9).decodePkt.rd && io.BypassPktValid(9) && i1rs1valid,
  io.in(1).bits.ctrl.rfSrc1 === io.BypassPktTable(8).decodePkt.rd && io.BypassPktValid(8) && i1rs1valid,
  true.B
  )
  i1rs2HitSel := VecInit(io.in(1).bits.ctrl.rfSrc2 === io.BypassPktTable(1).decodePkt.rd && io.BypassPktValid(1) && i1rs2valid,
  io.in(1).bits.ctrl.rfSrc2 === io.BypassPktTable(0).decodePkt.rd && io.BypassPktValid(0) && i1rs2valid,
  io.in(1).bits.ctrl.rfSrc2 === io.BypassPktTable(3).decodePkt.rd && io.BypassPktValid(3) && i1rs2valid,
  io.in(1).bits.ctrl.rfSrc2 === io.BypassPktTable(2).decodePkt.rd && io.BypassPktValid(2) && i1rs2valid,
  io.in(1).bits.ctrl.rfSrc2 === io.BypassPktTable(5).decodePkt.rd && io.BypassPktValid(5) && i1rs2valid,
  io.in(1).bits.ctrl.rfSrc2 === io.BypassPktTable(4).decodePkt.rd && io.BypassPktValid(4) && i1rs2valid,
  io.in(1).bits.ctrl.rfSrc2 === io.BypassPktTable(7).decodePkt.rd && io.BypassPktValid(7) && i1rs2valid,
  io.in(1).bits.ctrl.rfSrc2 === io.BypassPktTable(6).decodePkt.rd && io.BypassPktValid(6) && i1rs2valid,
  io.in(1).bits.ctrl.rfSrc2 === io.BypassPktTable(9).decodePkt.rd && io.BypassPktValid(9) && i1rs2valid,
  io.in(1).bits.ctrl.rfSrc2 === io.BypassPktTable(8).decodePkt.rd && io.BypassPktValid(8) && i1rs2valid,
  true.B
  )
    

    //merge decodePkt.subalu




  io.out0.bits.decodePkt.subalu := (
    (i0decodePkt.alu && i0rs1hitStage === 0.U && (io.BypassPktTable(1).decodePkt.muldiv || io.BypassPktTable(1).decodePkt.load )) ||
    (i0decodePkt.alu && i0rs1hitStage === 1.U && (io.BypassPktTable(0).decodePkt.muldiv || io.BypassPktTable(0).decodePkt.load )) ||
    (i0decodePkt.alu && i0rs1hitStage === 2.U && (io.BypassPktTable(3).decodePkt.muldiv || io.BypassPktTable(3).decodePkt.load )) ||
    (i0decodePkt.alu && i0rs1hitStage === 3.U && (io.BypassPktTable(2).decodePkt.muldiv || io.BypassPktTable(2).decodePkt.load )) 
  ) || (
    (i0decodePkt.alu && i0rs2hitStage === 0.U && (io.BypassPktTable(1).decodePkt.muldiv || io.BypassPktTable(1).decodePkt.load )) ||
    (i0decodePkt.alu && i0rs2hitStage === 1.U && (io.BypassPktTable(0).decodePkt.muldiv || io.BypassPktTable(0).decodePkt.load )) ||
    (i0decodePkt.alu && i0rs2hitStage === 2.U && (io.BypassPktTable(3).decodePkt.muldiv || io.BypassPktTable(3).decodePkt.load )) ||
    (i0decodePkt.alu && i0rs2hitStage === 3.U && (io.BypassPktTable(2).decodePkt.muldiv || io.BypassPktTable(2).decodePkt.load )) 
  ) || (
    (i0decodePkt.alu && i0rs1hitStage === 0.U && (io.BypassPktTable(1).decodePkt.subalu )) ||
    (i0decodePkt.alu && i0rs1hitStage === 1.U && (io.BypassPktTable(0).decodePkt.subalu )) ||
    (i0decodePkt.alu && i0rs1hitStage === 2.U && (io.BypassPktTable(3).decodePkt.subalu )) ||
    (i0decodePkt.alu && i0rs1hitStage === 3.U && (io.BypassPktTable(2).decodePkt.subalu )) ||
    (i0decodePkt.alu && i0rs1hitStage === 4.U && (io.BypassPktTable(5).decodePkt.subalu )) ||
    (i0decodePkt.alu && i0rs1hitStage === 5.U && (io.BypassPktTable(4).decodePkt.subalu ))
  ) || (
    (i0decodePkt.alu && i0rs2hitStage === 0.U && (io.BypassPktTable(1).decodePkt.subalu )) ||
    (i0decodePkt.alu && i0rs2hitStage === 1.U && (io.BypassPktTable(0).decodePkt.subalu )) ||
    (i0decodePkt.alu && i0rs2hitStage === 2.U && (io.BypassPktTable(3).decodePkt.subalu )) ||
    (i0decodePkt.alu && i0rs2hitStage === 3.U && (io.BypassPktTable(2).decodePkt.subalu )) ||
    (i0decodePkt.alu && i0rs2hitStage === 4.U && (io.BypassPktTable(5).decodePkt.subalu )) ||
    (i0decodePkt.alu && i0rs2hitStage === 5.U && (io.BypassPktTable(4).decodePkt.subalu ))
  )


  io.out1.bits.decodePkt.subalu := (
    (i1decodePkt.alu && i1rs1hitStage === 0.U && (io.BypassPktTable(1).decodePkt.muldiv || io.BypassPktTable(1).decodePkt.load )) ||
    (i1decodePkt.alu && i1rs1hitStage === 1.U && (io.BypassPktTable(0).decodePkt.muldiv || io.BypassPktTable(0).decodePkt.load )) ||
    (i1decodePkt.alu && i1rs1hitStage === 2.U && (io.BypassPktTable(3).decodePkt.muldiv || io.BypassPktTable(3).decodePkt.load )) ||
    (i1decodePkt.alu && i1rs1hitStage === 3.U && (io.BypassPktTable(2).decodePkt.muldiv || io.BypassPktTable(2).decodePkt.load )) 
  ) || (
    (i1decodePkt.alu && i1rs2hitStage === 0.U && (io.BypassPktTable(1).decodePkt.muldiv || io.BypassPktTable(1).decodePkt.load )) ||
    (i1decodePkt.alu && i1rs2hitStage === 1.U && (io.BypassPktTable(0).decodePkt.muldiv || io.BypassPktTable(0).decodePkt.load )) ||
    (i1decodePkt.alu && i1rs2hitStage === 2.U && (io.BypassPktTable(3).decodePkt.muldiv || io.BypassPktTable(3).decodePkt.load )) ||
    (i1decodePkt.alu && i1rs2hitStage === 3.U && (io.BypassPktTable(2).decodePkt.muldiv || io.BypassPktTable(2).decodePkt.load ))
  ) || (
    (i1decodePkt.alu && i1rs1hitStage === 0.U && (io.BypassPktTable(1).decodePkt.subalu )) ||
    (i1decodePkt.alu && i1rs1hitStage === 1.U && (io.BypassPktTable(0).decodePkt.subalu )) ||
    (i1decodePkt.alu && i1rs1hitStage === 2.U && (io.BypassPktTable(3).decodePkt.subalu )) ||
    (i1decodePkt.alu && i1rs1hitStage === 3.U && (io.BypassPktTable(2).decodePkt.subalu )) 
  ) || (
    (i1decodePkt.alu && i1rs2hitStage === 0.U && (io.BypassPktTable(1).decodePkt.subalu )) ||
    (i1decodePkt.alu && i1rs2hitStage === 1.U && (io.BypassPktTable(0).decodePkt.subalu )) ||
    (i1decodePkt.alu && i1rs2hitStage === 2.U && (io.BypassPktTable(3).decodePkt.subalu )) ||
    (i1decodePkt.alu && i1rs2hitStage === 3.U && (io.BypassPktTable(2).decodePkt.subalu )) 
  ) || ((i1Hiti0Rs1 || i1Hiti0Rs2) && i1decodePkt.alu)

  val i0Subalu = io.out0.bits.decodePkt.subalu
  val i1Subalu = io.out1.bits.decodePkt.subalu

  val FuType = VecInit(Seq.fill(10)(0.U.asTypeOf(new decodePkt)))
  for (i <- 0 to 9) FuType(i) := io.BypassPktTable(i).decodePkt
  val Valid = VecInit(Seq.fill(10)(false.B))
  for (i <- 0 to 9) Valid(i) := io.BypassPktValid(i)

  val instInPipe =  Valid(0) || Valid(1) || Valid(2) || Valid(3) || Valid(4) ||
    Valid(5) || Valid(6) || Valid(7) || Valid(8) || Valid(9)

  val mouvalid = ((io.in(0).bits.ctrl.fuType === "b100".U) && io.in(0).valid) || ((io.in(1).bits.ctrl.fuType === "b100".U) && io.in(1).valid)
//  dontTouch(mouvalid)
  val mou = Module(new SSDMOU)
  mou.io.pipelinevalid := instInPipe
  mou.io.in.valid := i1decodePkt.mou && io.in(1).valid
  mou.io.in.bits.func := 0.U
  mou.io.out.ready := true.B
  mou.io.flush := !mou.io.in.valid
  //for mdu not ready
  val mduNotReady0 = i0decodePkt.muldiv && ((FuType(0).muldiv && Valid(0)) || (FuType(1).muldiv && Valid(1)) ||
    (FuType(2).muldiv && Valid(2)) || (FuType(3).muldiv && Valid(3)))
  val mduNotReady1 = i1decodePkt.muldiv && ((FuType(0).muldiv && Valid(0)) || (FuType(1).muldiv && Valid(1)) ||
    (FuType(2).muldiv && Valid(2)) || (FuType(3).muldiv && Valid(3)))

    val i1rs1BypassE0 = io.out1.bits.BypassCtl.rs1bypasse0
    val i1rs2BypassE0 = io.out1.bits.BypassCtl.rs2bypasse0

  val i0rs1MatchE1 = WireInit(false.B)
  val i0rs1MatchE2 = WireInit(false.B)
  val i0rs1MatchE3 = WireInit(false.B)
  val i1rs1MatchE1 = WireInit(false.B)
  val i1rs1MatchE2 = WireInit(false.B)
  val i1rs1MatchE3 = WireInit(false.B)

  val i0rs2MatchE1 = WireInit(false.B)
  val i0rs2MatchE2 = WireInit(false.B)
  val i0rs2MatchE3 = WireInit(false.B)
  val i1rs2MatchE1 = WireInit(false.B)
  val i1rs2MatchE2 = WireInit(false.B)
  val i1rs2MatchE3 = WireInit(false.B)

  i0rs1MatchE1 := i0rs1hitStage === 0.U || i0rs1hitStage === 1.U
  i0rs1MatchE2 := i0rs1hitStage === 2.U || i0rs1hitStage === 3.U
  i0rs1MatchE3 := i0rs1hitStage === 4.U || i0rs1hitStage === 5.U
  i1rs1MatchE1 := i1rs1hitStage === 0.U || i1rs1hitStage === 1.U
  i1rs1MatchE2 := i1rs1hitStage === 2.U || i1rs1hitStage === 3.U
  i1rs1MatchE3 := i1rs1hitStage === 4.U || i1rs1hitStage === 5.U


  i0rs2MatchE1 := i0rs2hitStage === 0.U || i0rs2hitStage === 1.U
  i0rs2MatchE2 := i0rs2hitStage === 2.U || i0rs2hitStage === 3.U
  i0rs2MatchE3 := i0rs2hitStage === 4.U || i0rs2hitStage === 5.U
  i1rs2MatchE1 := i1rs2hitStage === 0.U || i1rs2hitStage === 1.U
  i1rs2MatchE2 := i1rs2hitStage === 2.U || i1rs2hitStage === 3.U
  i1rs2MatchE3 := i1rs2hitStage === 4.U || i1rs2hitStage === 5.U


  val i0NotAlu = WireInit(false.B)
  val i1NotAlu = WireInit(false.B)

  i0NotAlu := !(i0decodePkt.alu || i0decodePkt.branch)
  i1NotAlu := !(i1decodePkt.alu || i1decodePkt.branch)

  val i0rs1Class = Wire(new decodePkt)
  val i0rs2Class = Wire(new decodePkt)
  val i1rs1Class = Wire(new decodePkt)
  val i1rs2Class = Wire(new decodePkt)

  i0rs1Class := Mux(i0rs1hitStage === 10.U, 0.U.asTypeOf(new decodePkt), FuType(Mux(i0rs1hitStage(0) === 0.U, i0rs1hitStage + 1.U,i0rs1hitStage - 1.U )) )
  i0rs2Class := Mux(i0rs2hitStage === 10.U, 0.U.asTypeOf(new decodePkt), FuType(Mux(i0rs2hitStage(0) === 0.U, i0rs2hitStage + 1.U,i0rs2hitStage - 1.U )) )
  i1rs1Class := Mux(i1rs1hitStage === 10.U, 0.U.asTypeOf(new decodePkt), FuType(Mux(i1rs1hitStage(0) === 0.U, i1rs1hitStage + 1.U,i1rs1hitStage - 1.U )) )
  i1rs2Class := Mux(i1rs2hitStage === 10.U, 0.U.asTypeOf(new decodePkt), FuType(Mux(i1rs2hitStage(0) === 0.U, i1rs2hitStage + 1.U,i1rs2hitStage - 1.U )) )



  val i0LoadBlock = WireInit(false.B)
  val i0MulBlock = WireInit(false.B)
  val i0SecondaryBlock = WireInit(false.B)
  val i0SecondaryStall = WireInit(false.B)
  //  assign i0_load_block_d = (i0_not_alu_eff & i0_rs1_class_d.load & i0_rs1_match_e1) |
  //                           (i0_not_alu_eff & i0_rs1_class_d.load & i0_rs1_match_e2 & ~i0_dp.load & ~i0_dp.store & ~i0_dp.mul) | // can bypass load to address of load/store
  //                           (i0_not_alu_eff & i0_rs2_class_d.load & i0_rs2_match_e1 & ~i0_dp.store) |
  //                           (i0_not_alu_eff & i0_rs2_class_d.load & i0_rs2_match_e2 & ~i0_dp.store & ~i0_dp.mul);
  i0LoadBlock := (i0NotAlu && i0rs1Class.load && (i0rs1hitStage === 0.U || i0rs1hitStage === 1.U) ) ||
                 (i0NotAlu && i0rs1Class.load && (i0rs1hitStage === 2.U || i0rs1hitStage === 3.U) && !(i0decodePkt.load) && !(i0decodePkt.store)) ||
                 (i0NotAlu && i0rs2Class.load && (i0rs2hitStage === 0.U || i0rs2hitStage === 1.U) && !i0decodePkt.store) ||
                 (i0NotAlu && i0rs2Class.load && (i0rs2hitStage === 2.U || i0rs2hitStage === 3.U) && !i0decodePkt.store) 

  //  assign i0_mul_block_d = (i0_not_alu_eff & i0_rs1_class_d.mul & i0_rs1_match_e1_e2) |
  //                        (i0_not_alu_eff & i0_rs2_class_d.mul & i0_rs2_match_e1_e2);
  i0MulBlock := (i0NotAlu && i0rs1Class.muldiv && (i0rs1MatchE1 || i0rs1MatchE2)) ||
                (i0NotAlu && i0rs2Class.muldiv && (i0rs2MatchE1 || i0rs2MatchE2))
  
  //  assign i0_secondary_block_d = ((~i0_dp.alu & i0_rs1_class_d.sec & i0_rs1_match_e1_e3) |
  //                             (~i0_dp.alu & i0_rs2_class_d.sec & i0_rs2_match_e1_e3 & ~i0_dp.store)) & ~disable_secondary;
  i0SecondaryBlock := (i0NotAlu && i0rs1Class.subalu && (i0rs1MatchE1 || i0rs1MatchE2 || i0rs1MatchE3)) && i0decodePkt.muldiv ||
                      (i0NotAlu && i0rs1Class.subalu && (i0rs1MatchE1 || i0rs1MatchE2 ) && (i0decodePkt.load || i0decodePkt.store)) ||
                      (i0NotAlu && i0rs2Class.subalu && (i0rs2MatchE1 ) && i0decodePkt.store ) ||
                      (i0NotAlu && i0rs2Class.subalu && (i0rs2MatchE1 || i0rs2MatchE2 || i0rs2MatchE3 ) && !i0decodePkt.store )
                      
  //  assign i0_secondary_stall_d = ((i0_dp.alu & i1_rs1_depend_i0_d & ~i1_dp.alu & i0_secondary_d) |
  //                                 (i0_dp.alu & i1_rs2_depend_i0_d & ~i1_dp.alu & ~i1_dp.store & i0_secondary_d)) & ~disable_secondary;
  i0SecondaryStall := (io.out0.bits.decodePkt.subalu && i1Hiti0Rs1 && i1NotAlu) |
                      (io.out0.bits.decodePkt.subalu && i1Hiti0Rs2 && i1NotAlu)
  
  io.issueStall(0) := 
    i0decodePkt.csr && instInPipe || (i0decodePkt.mou && (! mou.io.out.valid)) ||
    mduNotReady1 ||
    i0LoadBlock ||
    i0MulBlock ||
    i0SecondaryBlock ||
    i0SecondaryStall 
    // i0_dp.csr_read |
    // i0_dp.csr_write |
    // i1_dp.csr_read |
    // i1_dp.csr_write |  // optimized csr write with rd==0
    // i1_nonblock_load_stall |
    // i1_store_stall_d |
    // i1_load_block_d |    // load data not ready
    // i1_mul_block_d |     // mul data not ready
    // (i1_depend_i0_d & ~non_block_case_d & ~store_data_bypass_i0_e2_c2) |
    // i1_load2_block_d |  // back-to-back load's at decode
    // i1_mul2_block_d |
    // i1_load_stall_d |  // prior stores
    // i1_secondary_block_d |

  val i1LoadBlock = WireInit(false.B)
  val i1MulBlock = WireInit(false.B)
  val i1Dependi0 = WireInit(false.B)
  val i1Load2Block = WireInit(false.B)
  val i1Mul2Block = WireInit(false.B)
  val i1SecondaryBlock = WireInit(false.B)

// assign i1_load_block_d = (i1_not_alu_eff & i1_rs1_class_d.load & i1_rs1_match_e1) |
//                             (i1_not_alu_eff & i1_rs1_class_d.load & i1_rs1_match_e2 & ~i1_dp.load & ~i1_dp.store & ~i1_dp.mul) |
//                             (i1_not_alu_eff & i1_rs2_class_d.load & i1_rs2_match_e1 & ~i1_dp.store) |
//                             (i1_not_alu_eff & i1_rs2_class_d.load & i1_rs2_match_e2 & ~i1_dp.store & ~i1_dp.mul);
  i1LoadBlock := (i1NotAlu && i1rs1Class.load && i1rs1MatchE1) ||
                 (i1NotAlu && i1rs1Class.load && i1rs1MatchE2 && !i1decodePkt.load && !i1decodePkt.store ) ||
                 (i1NotAlu && i1rs2Class.load && i1rs2MatchE1 && !i1decodePkt.store) ||
                 (i1NotAlu && i1rs2Class.load && i1rs2MatchE2 && !i1decodePkt.store)
// assign i1_mul_block_d = (i1_not_alu_eff & i1_rs1_class_d.mul & i1_rs1_match_e1_e2) |
//                            (i1_not_alu_eff & i1_rs2_class_d.mul & i1_rs2_match_e1_e2);
  i1MulBlock := (i1NotAlu && i1rs1Class.muldiv && (i1rs1MatchE1 || i1rs1MatchE2)) ||
                (i1NotAlu && i1rs2Class.muldiv && (i1rs2MatchE1 || i1rs2MatchE2))


  BoringUtils.addSource(mduNotReady1,"mduNotReady1")
  i1Dependi0 := i1Hiti0Rs1 || i1Hiti0Rs2

  i1Load2Block := (i1decodePkt.load || i1decodePkt.store) && (i0decodePkt.load || i0decodePkt.store)
  i1Mul2Block := i1decodePkt.muldiv && i0decodePkt.muldiv
  //  assign i1_secondary_block_d = ((~i1_dp.alu & i1_rs1_class_d.sec & i1_rs1_match_e1_e3) |
  //                                 (~i1_dp.alu & i1_rs2_class_d.sec & i1_rs2_match_e1_e3 & ~i1_dp.store) & ~disable_secondary);
  i1SecondaryBlock := (i1NotAlu && i1rs1Class.subalu && (i1rs1MatchE1 || i1rs1MatchE2 || i1rs1MatchE3) && i1decodePkt.muldiv) ||
                      (i1NotAlu && i1rs1Class.subalu && (i1rs1MatchE1 || i1rs1MatchE2 ) && (i1decodePkt.load || i1decodePkt.store)) ||
                      (i1NotAlu && i1rs2Class.subalu && (i1rs2MatchE1 ) && i1decodePkt.store ) ||
                      (i1NotAlu && i1rs2Class.subalu && (i1rs2MatchE1 || i1rs2MatchE2 || i1rs2MatchE3) && !i1decodePkt.store)
  val x = i1rs2MatchE2 && (!i1rs2BypassE0(2) && !i1rs2Class.load || !i1rs2BypassE0(3) && !i1rs2Class.load) || i1rs2MatchE1 && (!i1rs2BypassE0(0) || !i1rs2BypassE0(1))
  val y = i1rs1MatchE2 && (!i1rs1BypassE0(2) && !i1rs1Class.load || !i1rs1BypassE0(3) && !i1rs1Class.load) || i1rs1MatchE1 && (!i1rs1BypassE0(0) || !i1rs1BypassE0(1))

  val fasheng = WireInit(false.B)
  fasheng :=  (i0decodePkt.alu && !i0Subalu && i1decodePkt.alu && i1Hiti0Rs1 && i1rs2hitStage =/= 10.U && !x )||
               (i0decodePkt.alu && !i0Subalu && i1decodePkt.alu && i1Hiti0Rs2 && i1rs1hitStage =/= 10.U && !y)
  val noneBlockCase = WireInit(false.B)
  noneBlockCase := (i1decodePkt.alu && i0decodePkt.load) || (i1decodePkt.alu && i0decodePkt.muldiv) || (i0decodePkt.alu && !i0Subalu && (i1decodePkt.load || i1decodePkt.store))

  io.issueStall(1) :=
    (i0decodePkt.csr && i1decodePkt.csr) ||
    (i0decodePkt.csr && i1decodePkt.branch) || (i1decodePkt.mou) ||
    i1decodePkt.csr ||
    i1LoadBlock ||
    i1MulBlock ||
    i1Mul2Block ||
    i1Load2Block ||
    i1Dependi0 && !noneBlockCase ||
    i1SecondaryBlock ||
    mduNotReady0  ||
    io.issueStall(0) ||
    i1decodePkt.alu && ( i1rs1MatchE3 && i1rs1Class.subalu || i1rs2MatchE3 && i1rs2Class.subalu)

  BoringUtils.addSource(i1LoadBlock, "i1LoadBlock")
  BoringUtils.addSource(i1MulBlock, "i1MulBlock")
  BoringUtils.addSource(i1Dependi0 && !noneBlockCase, "i1Dependi0_noneBlockCase")
  BoringUtils.addSource(i1LoadBlock, "i1SecondaryBlock")

  BoringUtils.addSource(fasheng, "whhh")

  val cond = i1Dependi0 && !noneBlockCase
  BoringUtils.addSource(cond && i1decodePkt.load && i0decodePkt.alu && !i0Subalu, "i1LoadDependi0ALu")
  BoringUtils.addSource(i1Hiti0Rs2 && i1decodePkt.store && i0decodePkt.alu && !i0Subalu, "i1rs2StoreDependi0ALu")
  BoringUtils.addSource(cond && i1decodePkt.alu && i0decodePkt.alu && !i0Subalu, "i1AluDependi0ALu")
  // BoringUtils.addSource(cond && i1decodePkt.load && i0decodePkt.alu, "i1LoadDependi0ALu")


  BoringUtils.addSource(io.issueStall(0), "issueStalli0Cycle")
  BoringUtils.addSource(io.issueStall(0)&(!RegNext(io.issueStall(0))), "issueStalli0Cnt")
  BoringUtils.addSource(io.issueStall(1), "issueStalli1Cycle")
  BoringUtils.addSource(io.issueStall(1)&(!RegNext(io.issueStall(1))), "issueStalli1Cnt")

  BoringUtils.addSource(mduNotReady0, "mduNotReady0")
  BoringUtils.addSource(i1Load2Block, "i0i1LSBlock")
  BoringUtils.addSource(i0decodePkt.load && i1decodePkt.load && i1Hiti0Rs1, "i0i1LoadBlockLoadtouse")
  BoringUtils.addSource(i0decodePkt.store && i1decodePkt.store, "i0i1StoreBlock")
  BoringUtils.addSource((i0decodePkt.load && i1decodePkt.store) || (i0decodePkt.store && i1decodePkt.load), "i0i1AlternaingBlock")
  

  mou.io.flush := !(io.issueStall(1))
//  BoringUtils.addSource((!io.issueStall(1)), "issueStall_flush")
  //Signal to PMU
  //Normal Bound
//  dontTouch(io.pmuio)
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
  i1Hiti0Rs1 := io.in(1).bits.ctrl.rfSrc1 === i0decodePkt.rd && i1rs1valid && i0decodePkt.rdvalid
  i1Hiti0Rs2 := io.in(1).bits.ctrl.rfSrc2 === i0decodePkt.rd && i1rs2valid && i0decodePkt.rdvalid

  val i1Hiti0Rs1NotSec = WireInit(false.B)
  val i1Hiti0Rs2NotSec = WireInit(false.B)


  io.out0.bits.BypassCtl.rs1bypasse0 := VecInit(
      i0rs1hitStage === 0.U && (FuType(1).alu && !FuType(1).subalu || FuType(1).csr),
      i0rs1hitStage === 1.U && (FuType(0).alu && !FuType(0).subalu || FuType(0).csr),
      i0rs1hitStage === 2.U && (FuType(3).alu && !FuType(3).subalu || FuType(3).csr),
      i0rs1hitStage === 3.U && (FuType(2).alu && !FuType(2).subalu || FuType(2).csr),
      i0rs1hitStage === 4.U && (FuType(5).alu && !FuType(5).subalu || FuType(5).csr || FuType(5).load || FuType(5).muldiv),
      i0rs1hitStage === 5.U && (FuType(4).alu && !FuType(4).subalu || FuType(4).csr || FuType(4).load || FuType(4).muldiv),
      i0rs1hitStage === 6.U,
      i0rs1hitStage === 7.U,
      i0rs1hitStage === 8.U,
      i0rs1hitStage === 9.U
    )

  io.out0.bits.BypassCtl.rs2bypasse0 := VecInit(
      i0rs2hitStage === 0.U && (FuType(1).alu && !FuType(1).subalu || FuType(1).csr),
      i0rs2hitStage === 1.U && (FuType(0).alu && !FuType(0).subalu || FuType(0).csr),
      i0rs2hitStage === 2.U && (FuType(3).alu && !FuType(3).subalu || FuType(3).csr),
      i0rs2hitStage === 3.U && (FuType(2).alu && !FuType(2).subalu || FuType(2).csr),
      i0rs2hitStage === 4.U && (FuType(5).alu && !FuType(5).subalu || FuType(5).csr || FuType(5).load || FuType(5).muldiv),
      i0rs2hitStage === 5.U && (FuType(4).alu && !FuType(4).subalu || FuType(4).csr || FuType(4).load || FuType(4).muldiv),
      i0rs2hitStage === 6.U,
      i0rs2hitStage === 7.U,
      i0rs2hitStage === 8.U,
      i0rs2hitStage === 9.U
    )
  io.out0.bits.BypassCtl.rs1bypasse2 := VecInit(
      i0rs1hitStage === 4.U && FuType(5).subalu,
      i0rs1hitStage === 5.U && FuType(4).subalu
    )
  io.out0.bits.BypassCtl.rs2bypasse2 := Seq(
      i0rs2hitStage === 4.U && FuType(5).subalu,
      i0rs2hitStage === 5.U && FuType(4).subalu
    )
  io.out0.bits.BypassCtl.rs1bypasse3 := VecInit(
      false.B,
      i0rs1hitStage === 0.U && (FuType(1).subalu || FuType(1).load || FuType(1).muldiv),
      i0rs1hitStage === 1.U && (FuType(0).subalu || FuType(0).load || FuType(0).muldiv),
      i0rs1hitStage === 2.U && (FuType(3).subalu || FuType(3).load || FuType(3).muldiv),
      i0rs1hitStage === 3.U && (FuType(2).subalu || FuType(2).load || FuType(2).muldiv)
    )
  io.out0.bits.BypassCtl.rs2bypasse3 := VecInit(
      false.B,
      i0rs2hitStage === 0.U && (FuType(1).subalu || FuType(1).load || FuType(1).muldiv),
      i0rs2hitStage === 1.U && (FuType(0).subalu || FuType(0).load || FuType(0).muldiv),
      i0rs2hitStage === 2.U && (FuType(3).subalu || FuType(3).load || FuType(3).muldiv),
      i0rs2hitStage === 3.U && (FuType(2).subalu || FuType(2).load || FuType(2).muldiv)
    )


  io.out1.bits.BypassCtl.rs1bypasse0 := VecInit(
      i1rs1hitStage === 0.U && (FuType(1).alu && !FuType(1).subalu || FuType(1).csr) && !i1Hiti0Rs1,
      i1rs1hitStage === 1.U && (FuType(0).alu && !FuType(0).subalu || FuType(0).csr) && !i1Hiti0Rs1,
      i1rs1hitStage === 2.U && (FuType(3).alu && !FuType(3).subalu || FuType(3).csr) && !i1Hiti0Rs1,
      i1rs1hitStage === 3.U && (FuType(2).alu && !FuType(2).subalu || FuType(2).csr) && !i1Hiti0Rs1,
      i1rs1hitStage === 4.U && (FuType(5).alu && !FuType(5).subalu || FuType(5).csr || FuType(5).load || FuType(5).muldiv) && !i1Hiti0Rs1,
      i1rs1hitStage === 5.U && (FuType(4).alu && !FuType(4).subalu || FuType(4).csr || FuType(4).load || FuType(4).muldiv) && !i1Hiti0Rs1,
      i1rs1hitStage === 6.U && !i1Hiti0Rs1,
      i1rs1hitStage === 7.U && !i1Hiti0Rs1,
      i1rs1hitStage === 8.U && !i1Hiti0Rs1,
      i1rs1hitStage === 9.U && !i1Hiti0Rs1
    )
  io.out1.bits.BypassCtl.rs2bypasse0 := VecInit(
      i1rs2hitStage === 0.U && (FuType(1).alu && !FuType(1).subalu || FuType(1).csr) && !i1Hiti0Rs2,
      i1rs2hitStage === 1.U && (FuType(0).alu && !FuType(0).subalu || FuType(0).csr) && !i1Hiti0Rs2,
      i1rs2hitStage === 2.U && (FuType(3).alu && !FuType(3).subalu || FuType(3).csr) && !i1Hiti0Rs2,
      i1rs2hitStage === 3.U && (FuType(2).alu && !FuType(2).subalu || FuType(2).csr) && !i1Hiti0Rs2,
      i1rs2hitStage === 4.U && (FuType(5).alu && !FuType(5).subalu || FuType(5).csr || FuType(5).load || FuType(5).muldiv) && !i1Hiti0Rs2,
      i1rs2hitStage === 5.U && (FuType(4).alu && !FuType(4).subalu || FuType(4).csr || FuType(4).load || FuType(4).muldiv) && !i1Hiti0Rs2,
      i1rs2hitStage === 6.U && !i1Hiti0Rs2,
      i1rs2hitStage === 7.U && !i1Hiti0Rs2,
      i1rs2hitStage === 8.U && !i1Hiti0Rs2,
      i1rs2hitStage === 9.U && !i1Hiti0Rs2
    )

    
    io.out1.bits.BypassCtl.rs1bypasse2 := VecInit(
      i1rs1hitStage === 4.U && FuType(5).subalu && !i1Hiti0Rs1,
      i1rs1hitStage === 5.U && FuType(4).subalu && !i1Hiti0Rs1
    )
    io.out1.bits.BypassCtl.rs2bypasse2 := Seq(
      i1rs2hitStage === 4.U && FuType(5).subalu && !i1Hiti0Rs2,
      i1rs2hitStage === 5.U && FuType(4).subalu && !i1Hiti0Rs2
    )
  i1Hiti0Rs1NotSec := i1Hiti0Rs1 && !i0decodePkt.subalu
  i1Hiti0Rs2NotSec := i1Hiti0Rs2 && !i0decodePkt.subalu
    io.out1.bits.BypassCtl.rs1bypasse3 := VecInit(
      i1Hiti0Rs1NotSec,
      i1rs1hitStage === 0.U && (FuType(1).subalu || FuType(1).load || FuType(1).muldiv) && !i1Hiti0Rs1,
      i1rs1hitStage === 1.U && (FuType(0).subalu || FuType(0).load || FuType(0).muldiv) && !i1Hiti0Rs1,
      i1rs1hitStage === 2.U && (FuType(3).subalu || FuType(3).load || FuType(3).muldiv) && !i1Hiti0Rs1,
      i1rs1hitStage === 3.U && (FuType(2).subalu || FuType(2).load || FuType(2).muldiv) && !i1Hiti0Rs1
    )


    io.out1.bits.BypassCtl.rs2bypasse3 := VecInit(
      i1Hiti0Rs2NotSec,
      i1rs2hitStage === 0.U && (FuType(1).subalu || FuType(1).load || FuType(1).muldiv) && !i1Hiti0Rs2,
      i1rs2hitStage === 1.U && (FuType(0).subalu || FuType(0).load || FuType(0).muldiv) && !i1Hiti0Rs2,
      i1rs2hitStage === 2.U && (FuType(3).subalu || FuType(3).load || FuType(3).muldiv) && !i1Hiti0Rs2,
      i1rs2hitStage === 3.U && (FuType(2).subalu || FuType(2).load || FuType(2).muldiv) && !i1Hiti0Rs2
    )

  // store pipeline bypaas ctrl
  lsuCtrli0.lsBypassCtrlE1 := VecInit(
    false.B,
    i0rs1hitStage === 2.U && (FuType(3).load || FuType(3).muldiv) && (i0decodePkt.store || i0decodePkt.load),
    i0rs1hitStage === 3.U && (FuType(2).load || FuType(2).muldiv) && (i0decodePkt.store || i0decodePkt.load),
    i0rs1hitStage === 4.U && FuType(5).subalu && (i0decodePkt.store || i0decodePkt.load),
    i0rs1hitStage === 5.U && FuType(4).subalu && (i0decodePkt.store || i0decodePkt.load)
  )
  lsuCtrli0.storeBypassCtrlE2 := VecInit(
    false.B,
    i0rs2hitStage === 0.U && (FuType(1).load || FuType(1).muldiv) && i0decodePkt.store,
    i0rs2hitStage === 1.U && (FuType(0).load || FuType(0).muldiv) && i0decodePkt.store,
    i0rs2hitStage === 2.U && (FuType(3).subalu || FuType(3).load || FuType(3).muldiv) && i0decodePkt.store,
    i0rs2hitStage === 3.U && (FuType(2).subalu || FuType(2).load || FuType(2).muldiv) && i0decodePkt.store,
    i0rs2hitStage === 4.U && (FuType(5).subalu) && i0decodePkt.store,
    i0rs2hitStage === 5.U && (FuType(4).subalu) && i0decodePkt.store
  )
  lsuCtrli1.lsBypassCtrlE1 := VecInit(
    i1Hiti0Rs1 && i0decodePkt.alu && !i0Subalu && (i1decodePkt.store || i1decodePkt.load),
    i1rs1hitStage === 2.U && (FuType(3).load || FuType(3).muldiv) && (i1decodePkt.store || i1decodePkt.load),
    i1rs1hitStage === 3.U && (FuType(2).load || FuType(2).muldiv) && (i1decodePkt.store || i1decodePkt.load),
    i1rs1hitStage === 4.U && FuType(5).subalu && (i1decodePkt.store || i1decodePkt.load),
    i1rs1hitStage === 5.U && FuType(4).subalu && (i1decodePkt.store || i1decodePkt.load)
  )
  lsuCtrli1.storeBypassCtrlE2 := VecInit(
    i1Hiti0Rs2 && i0decodePkt.alu && !i0Subalu && (i1decodePkt.store),
    i1rs2hitStage === 0.U && (FuType(1).load || FuType(1).muldiv) && i1decodePkt.store,
    i1rs2hitStage === 1.U && (FuType(0).load || FuType(0).muldiv) && i1decodePkt.store,
    i1rs2hitStage === 2.U && (FuType(3).subalu || FuType(3).load || FuType(3).muldiv) && i1decodePkt.store,
    i1rs2hitStage === 3.U && (FuType(2).subalu || FuType(2).load || FuType(2).muldiv) && i1decodePkt.store,
    i1rs2hitStage === 4.U && (FuType(5).subalu) && i1decodePkt.store,
    i1rs2hitStage === 5.U && (FuType(4).subalu) && i1decodePkt.store
  )
//  dontTouch(lsuCtrli0)
//  dontTouch(lsuCtrli1)
  io.out0.bits.lsuCtrl := lsuCtrli0
  io.out1.bits.lsuCtrl := lsuCtrli1

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
    out.mou    := in.ctrl.fuType === FuType.mou
  }
}

class PipeCtl extends Module{
  val io = IO(new Bundle{
    val i0pipeStall = Input(Bool())
    val i1pipeStall = Input(Bool())
    val memStall = Input(Bool())
    val flush = Input(Vec(4,Bool()))  //flushPoint(0,3) -> alu0,alu1,sub_alu0,sub_alu1
    val pipeCtl = new StallFlushIO
  })
//  val pipeCtl = IO(new StallFlushIO)
//  dontTouch(io.flush)

  // stall vec
  io.pipeCtl.stall(0) := io.i1pipeStall
  io.pipeCtl.stall(1) := io.i0pipeStall
  io.pipeCtl.stall(2) := io.memStall

  //flush/invalid vec
  val allStageFlushVec = VecInit(Seq.fill(10)(false.B))
  val allStageInvalidVec = VecInit(Seq.fill(12)(false.B))
  //pipeline0 
  //pipeline1 
  /*
  ***
  jpz note: the modification below is very critical,because the issue channels were renamed!!!!
  ***
  */
  val alu0InvalidList = List(0,1,2,3,5)
  val alu1InvalidList = List(0,1,2,3)
  val subalu0InvalidList = List(0,1,2,3,4,5,6,7,8,9,11)
  val subalu1InvalidList = List(0,1,2,3,4,5,6,7,8,9)

  val alu0FlushList = List(0,1,3)
  val alu1FlushList = List(0,1)
  val subalu0FlushList = List(0,1,2,3,4,5,6,7,9)
  val subalu1FlushList = List(0,1,2,3,4,5,6,7)


  alu0InvalidList.foreach{ case i => when(io.flush(0) === true.B){allStageInvalidVec(i) := io.flush(0)}}
  alu1InvalidList.foreach{ case i => when(io.flush(1) === true.B){allStageInvalidVec(i) := io.flush(1)}}
  subalu0InvalidList.foreach{ case i => when(io.flush(2) === true.B){allStageInvalidVec(i) := io.flush(2)}}
  subalu1InvalidList.foreach{ case i => when(io.flush(3) === true.B){allStageInvalidVec(i) := io.flush(3)}}

  alu0FlushList.foreach{ case i => when(io.flush(0) === true.B){allStageFlushVec(i) := io.flush(0)}}
  alu1FlushList.foreach{ case i => when(io.flush(1) === true.B){allStageFlushVec(i) := io.flush(1)}}
  subalu0FlushList.foreach{ case i => when(io.flush(2) === true.B){allStageFlushVec(i) := io.flush(2)}}
  subalu1FlushList.foreach{ case i => when(io.flush(3) === true.B){allStageFlushVec(i) := io.flush(3)}}

  io.pipeCtl.invalid := allStageInvalidVec
  io.pipeCtl.flush := allStageFlushVec


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
  DecodeIO2BypassPkt.io.in(0) <> io.in(0)
  DecodeIO2BypassPkt.io.in(1) <> io.in(1)
  DecodeIO2BypassPkt.io.memStall <> io.memStall
  DecodeIO2BypassPkt.io.mduStall <> io.mduStall
  DecodeIO2BypassPkt.io.pmuio <> io.pmuio


  io.issueStall := DecodeIO2BypassPkt.io.issueStall
  io.decodeBypassPkt <> Seq(DecodeIO2BypassPkt.io.out0,DecodeIO2BypassPkt.io.out1)
  io.pipeFlush := PipelineCtl.io.pipeCtl.flush
  io.pipeInvalid := PipelineCtl.io.pipeCtl.invalid

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
    a.io.isFlush <> PipelineCtl.io.pipeCtl.flush(b)
    a.io.inValid <> PipelineCtl.io.pipeCtl.invalid(b)
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
    a.io.isFlush <> PipelineCtl.io.pipeCtl.flush(b)
    a.io.inValid <> PipelineCtl.io.pipeCtl.invalid(b)
  }


}




