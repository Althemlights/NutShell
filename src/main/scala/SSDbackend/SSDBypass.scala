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

//第一版先就将这部分都塞到一块
class DecodeIO2BypassPkt extends Module {
  val io = IO(new Bundle {
    val in = Vec(2, Flipped(Decoupled(new DecodeIO)))
    val BypassPktTable = Input(Vec(10, new BypassPkt))
    val BypassPktValid = Input(Vec(10, Bool()))
    val issueStall = Output(Vec(2, Bool()))
    val out0 = Decoupled(new BypassPkt)
    val out1 = Decoupled(new BypassPkt)
  })
  //生成 BypassPkt， 以及issue stall 信号
  val i0decodePkt = Wire(new decodePkt)
  val i1decodePkt = Wire(new decodePkt)
  DecodeIO2decodePkt(io.in(0).bits, i0decodePkt)
  DecodeIO2decodePkt(io.in(1).bits, i1decodePkt)
  io.out0.bits.decodePkt := i0decodePkt
  io.out1.bits.decodePkt := i1decodePkt //先简单连一下，之后再覆盖或补全其他输出信号
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
      i0rs1HitSel(i) := io.in(0).bits.ctrl.rfSrc1 === io.BypassPktTable(i).decodePkt.rd && io.BypassPktTable(i).decodePkt.rdvalid && io.BypassPktValid(i) && i0rs1valid
      i0rs2HitSel(i) := io.in(0).bits.ctrl.rfSrc2 === io.BypassPktTable(i).decodePkt.rd && io.BypassPktTable(i).decodePkt.rdvalid && io.BypassPktValid(i) && i0rs2valid
      i1rs1HitSel(i) := io.in(1).bits.ctrl.rfSrc1 === io.BypassPktTable(i).decodePkt.rd && io.BypassPktTable(i).decodePkt.rdvalid && io.BypassPktValid(i) && i1rs1valid
      i1rs2HitSel(i) := io.in(1).bits.ctrl.rfSrc2 === io.BypassPktTable(i).decodePkt.rd && io.BypassPktTable(i).decodePkt.rdvalid && io.BypassPktValid(i) && i1rs2valid
    }
    else{
      i0rs1HitSel(10) := true.B
      i0rs2HitSel(10) := true.B
      i1rs1HitSel(10) := true.B
      i1rs2HitSel(10) := true.B

    }
  }
    

    //merge decodePkt.subalu

    io.out0.bits.decodePkt.subalu :=
      (i0decodePkt.alu && i0rs1hitStage >= 0.U && i0rs1hitStage <= 3.U && (io.BypassPktTable(i0rs1hitStage).decodePkt.mul || io.BypassPktTable(i0rs1hitStage).decodePkt.load)
      || i0decodePkt.alu && i0rs2hitStage >= 0.U && i0rs2hitStage <= 3.U && (io.BypassPktTable(i0rs2hitStage).decodePkt.mul || io.BypassPktTable(i0rs2hitStage).decodePkt.load)
      || i0decodePkt.alu && i0rs1hitStage >= 0.U && i0rs1hitStage <= 5.U && io.BypassPktTable(i0rs1hitStage).decodePkt.subalu
      || i0decodePkt.alu && i0rs2hitStage >= 0.U && i0rs2hitStage <= 5.U && io.BypassPktTable(i0rs2hitStage).decodePkt.subalu
      || i0decodePkt.alu && i1decodePkt.alu && !io.out1.bits.decodePkt.subalu &&
        (io.in(0).bits.ctrl.rfSrc1 === i1decodePkt.rd && i0rs1valid || io.in(0).bits.ctrl.rfSrc2 === i1decodePkt.rd && i0rs2valid) && i1decodePkt.rdvalid
      )

    io.out1.bits.decodePkt.subalu :=
      (i1decodePkt.alu && i1rs1hitStage >= 0.U && i1rs1hitStage <= 3.U && (io.BypassPktTable(i1rs1hitStage).decodePkt.mul || io.BypassPktTable(i1rs1hitStage).decodePkt.load)
      || i1decodePkt.alu && i1rs2hitStage >= 0.U && i1rs2hitStage <= 3.U && (io.BypassPktTable(i1rs2hitStage).decodePkt.mul || io.BypassPktTable(i1rs2hitStage).decodePkt.load)
      || i1decodePkt.alu && i1rs1hitStage >= 0.U && i1rs1hitStage <= 5.U && io.BypassPktTable(i1rs1hitStage).decodePkt.subalu
      || i1decodePkt.alu && i1rs2hitStage >= 0.U && i1rs2hitStage <= 5.U && io.BypassPktTable(i1rs2hitStage).decodePkt.subalu
      )

    //issue stall
  io.issueStall(0) := (io.in(0).bits.ctrl.rfSrc1 === i1decodePkt.rd && i0rs1valid || io.in(0).bits.ctrl.rfSrc2 === i1decodePkt.rd && i0rs2valid) &&
    i1decodePkt.rdvalid && i1decodePkt.alu && io.out1.bits.decodePkt.subalu
  io.issueStall(1) := false.B
  dontTouch(io.issueStall)

  val cond = Wire(Bool())
  cond := io.in(0).bits.cf.pc === "h8000011c".U
  myDebug(cond,"issue_0 is %b\n",io.issueStall(0).asUInt)

    //BypassCtl
    val FuType = VecInit(Seq.fill(10)(0.U.asTypeOf(new decodePkt)))
    for (i <- 0 to 9) FuType(i) := io.BypassPktTable(i).decodePkt
    val Valid = VecInit(Seq.fill(10)(false.B))
    for (i <- 0 to 9) Valid(i) := io.BypassPktValid(i)

    //BypassPkt out0
  val i0Hiti1Rs1 = Wire(Bool())
  val i0Hiti1Rs2 = Wire(Bool())
  i0Hiti1Rs1 := io.in(0).bits.ctrl.rfSrc1 === i1decodePkt.rd && i0rs1valid && i1decodePkt.rdvalid && (i1decodePkt.mul || i1decodePkt.load || i1decodePkt.alu && !io.out1.bits.decodePkt.subalu)
  i0Hiti1Rs2 := io.in(0).bits.ctrl.rfSrc2 === i1decodePkt.rd && i0rs2valid && i1decodePkt.rdvalid && (i1decodePkt.mul || i1decodePkt.load || i1decodePkt.alu && !io.out1.bits.decodePkt.subalu)

  io.out0.bits.BypassCtl.rs1bypasse0 := VecInit(
      i0rs1valid && Valid(0) && i0rs1hitStage === 0.U && FuType(0).alu && !FuType(0).subalu && !i0Hiti1Rs1,
      i0rs1valid && Valid(1) && i0rs1hitStage === 1.U && FuType(1).alu && !FuType(1).subalu && !i0Hiti1Rs1,
      i0rs1valid && Valid(2) && i0rs1hitStage === 2.U && FuType(2).alu && !FuType(2).subalu && !i0Hiti1Rs1,
      i0rs1valid && Valid(3) && i0rs1hitStage === 3.U && FuType(3).alu && !FuType(3).subalu && !i0Hiti1Rs1,
      i0rs1valid && Valid(4) && i0rs1hitStage === 4.U && FuType(4).alu && !FuType(4).subalu && !i0Hiti1Rs1,
      i0rs1valid && Valid(5) && i0rs1hitStage === 5.U && FuType(5).alu && !FuType(5).subalu && !i0Hiti1Rs1,
      i0rs1valid && Valid(4) && i0rs1hitStage === 4.U && FuType(4).load || Valid(5) && i0rs1hitStage === 5.U && FuType(5).load && !i0Hiti1Rs1,
      i0rs1valid && Valid(4) && i0rs1hitStage === 4.U && FuType(4).mul || Valid(5) && i0rs1hitStage === 5.U && FuType(5).mul && !i0Hiti1Rs1,
      i0rs1valid && Valid(6) && i0rs1hitStage === 6.U && !i0Hiti1Rs1,
      i0rs1valid && Valid(7) && i0rs1hitStage === 7.U && !i0Hiti1Rs1,
      i0rs1valid && Valid(8) && i0rs1hitStage === 8.U && !i0Hiti1Rs1,
      i0rs1valid && Valid(9) && i0rs1hitStage === 9.U && !i0Hiti1Rs1
    )
    io.out0.bits.BypassCtl.rs2bypasse0 := VecInit(
      i0rs2valid && Valid(0) && i0rs2hitStage === 0.U && FuType(0).alu && !FuType(0).subalu && !i0Hiti1Rs2,
      i0rs2valid && Valid(1) && i0rs2hitStage === 1.U && FuType(1).alu && !FuType(1).subalu && !i0Hiti1Rs2,
      i0rs2valid && Valid(2) && i0rs2hitStage === 2.U && FuType(2).alu && !FuType(2).subalu && !i0Hiti1Rs2,
      i0rs2valid && Valid(3) && i0rs2hitStage === 3.U && FuType(3).alu && !FuType(3).subalu && !i0Hiti1Rs2,
      i0rs2valid && Valid(4) && i0rs2hitStage === 4.U && FuType(4).alu && !FuType(4).subalu && !i0Hiti1Rs2,
      i0rs2valid && Valid(5) && i0rs2hitStage === 5.U && FuType(5).alu && !FuType(5).subalu && !i0Hiti1Rs2,
      i0rs2valid && Valid(4) && i0rs2hitStage === 4.U && FuType(4).load || Valid(5) && i0rs1hitStage === 5.U && FuType(5).load && !i0Hiti1Rs2,
      i0rs2valid && Valid(4) && i0rs2hitStage === 4.U && FuType(4).mul || Valid(5) && i0rs1hitStage === 5.U && FuType(5).mul && !i0Hiti1Rs2,
      i0rs2valid && Valid(6) && i0rs2hitStage === 6.U && !i0Hiti1Rs2,
      i0rs2valid && Valid(7) && i0rs2hitStage === 7.U && !i0Hiti1Rs2,
      i0rs2valid && Valid(8) && i0rs2hitStage === 8.U && !i0Hiti1Rs2,
      i0rs2valid && Valid(9) && i0rs2hitStage === 9.U && !i0Hiti1Rs2
    )
    io.out0.bits.BypassCtl.rs1bypasse2 := VecInit(
      i0rs1valid && Valid(4) && i0rs1hitStage === 4.U && FuType(4).alu && FuType(4).subalu && !i0Hiti1Rs1,
      i0rs1valid && Valid(5) && i0rs1hitStage === 5.U && FuType(5).alu && FuType(5).subalu && !i0Hiti1Rs1
    )
    io.out0.bits.BypassCtl.rs2bypasse2 := Seq(
      i0rs2valid && Valid(4) && i0rs2hitStage === 4.U && FuType(4).alu && FuType(4).subalu && !i0Hiti1Rs2,
      i0rs2valid && Valid(5) && i0rs2hitStage === 5.U && FuType(5).alu && FuType(5).subalu && !i0Hiti1Rs2
    )

    io.out0.bits.BypassCtl.rs1bypasse3 := VecInit(
      i0rs1valid && i0Hiti1Rs1,
      i0rs1valid && Valid(0) && i0rs1hitStage === 0.U && FuType(0).alu && FuType(0).subalu
        || Valid(0) && i0rs1hitStage === 0.U && FuType(0).load
        || Valid(0) && i0rs1hitStage === 0.U && FuType(0).mul,
      i0rs1valid && Valid(1) && i0rs1hitStage === 1.U && FuType(1).load
        || Valid(1) && i0rs1hitStage === 1.U && FuType(1).alu && FuType(1).subalu
        || Valid(1) && i0rs1hitStage === 1.U && FuType(1).mul,
      i0rs1valid && Valid(2) && i0rs1hitStage === 2.U && FuType(2).alu && FuType(2).subalu
        || Valid(2) && i0rs1hitStage === 2.U && FuType(2).load
        || Valid(2) && i0rs1hitStage === 2.U && FuType(2).mul,
      i0rs1valid && Valid(3) && i0rs1hitStage === 3.U && FuType(3).alu && FuType(3).subalu
        || Valid(3) && i0rs1hitStage === 3.U && FuType(3).load
        || Valid(3) && i0rs1hitStage === 3.U && FuType(3).mul
    )


    io.out0.bits.BypassCtl.rs2bypasse3 := VecInit(
      i0rs2valid && i0Hiti1Rs2,
      i0rs2valid && Valid(0) && i0rs2hitStage === 0.U && FuType(0).alu && FuType(0).subalu
        || Valid(0) && i0rs2hitStage === 0.U && FuType(0).load
        || Valid(0) && i0rs2hitStage === 0.U && FuType(0).mul,
      i0rs2valid && Valid(1) && i0rs2hitStage === 1.U && FuType(1).load
        || Valid(1) && i0rs2hitStage === 1.U && FuType(1).alu && FuType(1).subalu
        || Valid(1) && i0rs2hitStage === 1.U && FuType(1).mul,
      i0rs2valid && Valid(2) && i0rs2hitStage === 2.U && FuType(2).alu && FuType(2).subalu
        || Valid(2) && i0rs2hitStage === 2.U && FuType(2).load
        || Valid(2) && i0rs2hitStage === 2.U && FuType(2).mul,
      i0rs2valid && Valid(3) && i0rs2hitStage === 3.U && FuType(3).alu && FuType(3).subalu
        || Valid(3) && i0rs2hitStage === 3.U && FuType(3).load
        || Valid(3) && i0rs2hitStage === 3.U && FuType(3).mul
    )
    //BypassPkt out1
    io.out1.bits.BypassCtl.rs1bypasse0 := VecInit(
      i1rs1valid && Valid(0) && i1rs1hitStage === 0.U && FuType(0).alu && !FuType(0).subalu,
      i1rs1valid && Valid(1) && i1rs1hitStage === 1.U && FuType(1).alu && !FuType(1).subalu,
      i1rs1valid && Valid(2) && i1rs1hitStage === 2.U && FuType(2).alu && !FuType(2).subalu,
      i1rs1valid && Valid(3) && i1rs1hitStage === 3.U && FuType(3).alu && !FuType(3).subalu,
      i1rs1valid && Valid(4) && i1rs1hitStage === 4.U && FuType(4).alu && !FuType(4).subalu,
      i1rs1valid && Valid(5) && i1rs1hitStage === 5.U && FuType(5).alu && !FuType(5).subalu,
      i1rs1valid && Valid(4) && i1rs1hitStage === 4.U && FuType(4).load || Valid(5) && i1rs1hitStage === 5.U && FuType(5).load,
      i1rs1valid && Valid(4) && i1rs1hitStage === 4.U && FuType(4).mul || Valid(5) && i1rs1hitStage === 5.U && FuType(5).mul,
      i1rs1valid && Valid(6) && i1rs1hitStage === 6.U,
      i1rs1valid && Valid(7) && i1rs1hitStage === 7.U,
      i1rs1valid && Valid(8) && i1rs1hitStage === 8.U,
      i1rs1valid && Valid(9) && i1rs1hitStage === 9.U
    )
    io.out1.bits.BypassCtl.rs2bypasse0 := VecInit(
      i1rs2valid && Valid(0) && i1rs2hitStage === 0.U && FuType(0).alu && !FuType(0).subalu,
      i1rs2valid && Valid(1) && i1rs2hitStage === 1.U && FuType(1).alu && !FuType(1).subalu,
      i1rs2valid && Valid(2) && i1rs2hitStage === 2.U && FuType(2).alu && !FuType(2).subalu,
      i1rs2valid && Valid(3) && i1rs2hitStage === 3.U && FuType(3).alu && !FuType(3).subalu,
      i1rs2valid && Valid(4) && i1rs2hitStage === 4.U && FuType(4).alu && !FuType(4).subalu,
      i1rs2valid && Valid(5) && i1rs2hitStage === 5.U && FuType(5).alu && !FuType(5).subalu,
      i1rs2valid && Valid(4) && i1rs2hitStage === 4.U && FuType(4).load || Valid(5) && i1rs1hitStage === 5.U && FuType(5).load,
      i1rs2valid && Valid(4) && i1rs2hitStage === 4.U && FuType(4).mul || Valid(5) && i1rs1hitStage === 5.U && FuType(5).mul,
      i1rs2valid && Valid(6) && i1rs2hitStage === 6.U,
      i1rs2valid && Valid(7) && i1rs2hitStage === 7.U,
      i1rs2valid && Valid(8) && i1rs2hitStage === 8.U,
      i1rs2valid && Valid(9) && i1rs2hitStage === 9.U
    )
    io.out1.bits.BypassCtl.rs1bypasse2 := VecInit(
      i1rs1valid && Valid(4) && i1rs1hitStage === 4.U && FuType(4).alu && FuType(4).subalu,
      i1rs1valid && Valid(5) && i1rs1hitStage === 5.U && FuType(5).alu && FuType(5).subalu
    )
    io.out1.bits.BypassCtl.rs2bypasse2 := VecInit(
      i1rs2valid && Valid(4) && i1rs2hitStage === 4.U && FuType(4).alu && FuType(4).subalu,
      i1rs2valid && Valid(5) && i1rs2hitStage === 5.U && FuType(5).alu && FuType(5).subalu
    )
  io.out1.bits.BypassCtl.rs1bypasse3 := VecInit(
    false.B,
    i1rs1valid && Valid(0) && i1rs1hitStage === 0.U && FuType(0).alu && FuType(0).subalu
      || Valid(0) && i1rs1hitStage === 0.U && FuType(0).load
      || Valid(0) && i1rs1hitStage === 0.U && FuType(0).mul,
    i1rs1valid && Valid(1) && i1rs1hitStage === 1.U && FuType(1).load
      || Valid(1) && i1rs1hitStage === 1.U && FuType(1).alu && FuType(1).subalu
      || Valid(1) && i1rs1hitStage === 1.U && FuType(1).mul,
    i1rs1valid && Valid(2) && i1rs1hitStage === 2.U && FuType(2).alu && FuType(2).subalu
      || Valid(2) && i1rs1hitStage === 2.U && FuType(2).load
      || Valid(2) && i1rs1hitStage === 2.U && FuType(2).mul,
    i1rs1valid && Valid(3) && i1rs1hitStage === 3.U && FuType(3).alu && FuType(3).subalu
      || Valid(3) && i1rs1hitStage === 3.U && FuType(3).load
      || Valid(3) && i1rs1hitStage === 3.U && FuType(3).mul
  )
  io.out1.bits.BypassCtl.rs2bypasse3 := VecInit(
    false.B,
    i1rs2valid && Valid(0) && i1rs2hitStage === 0.U && FuType(0).alu && FuType(0).subalu
      || Valid(0) && i1rs2hitStage === 0.U && FuType(0).load
      || Valid(0) && i1rs2hitStage === 0.U && FuType(0).mul,
    i1rs2valid && Valid(1) && i1rs2hitStage === 1.U && FuType(1).load
      || Valid(1) && i1rs2hitStage === 1.U && FuType(1).alu && FuType(1).subalu
      || Valid(1) && i1rs2hitStage === 1.U && FuType(1).mul,
    i1rs2valid && Valid(2) && i1rs2hitStage === 2.U && FuType(2).alu && FuType(2).subalu
      || Valid(2) && i1rs2hitStage === 2.U && FuType(2).load
      || Valid(2) && i1rs2hitStage === 2.U && FuType(2).mul,
    i1rs2valid && Valid(3) && i1rs2hitStage === 3.U && FuType(3).alu && FuType(3).subalu
      || Valid(3) && i1rs2hitStage === 3.U && FuType(3).load
      || Valid(3) && i1rs2hitStage === 3.U && FuType(3).mul
  )
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
    out.rd <> in.ctrl.rfDest
    out.rdvalid <> in.ctrl.rfWen
    out.alu := in.ctrl.fuType === FuType.alu || in.ctrl.fuType === FuType.bru
    //下面的就先置零，subalu 会在其他模块覆盖掉
    out.mul := false.B
    out.div := false.B
    out.load := false.B
    out.store := false.B
    out.subalu := false.B
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

  //flush vec
  val allStageFlushVec = VecInit(Seq.fill(10)(false.B))

  //pipeline0 : 1,3,5,7,9
  //pipeline1 : 0,2,4,6,8
  val alu0FLushList = List(0,1)
  val alu1FLushList = List(0,1,2)
  val subalu0FLushList = List(0,1,2,3,4,5,6,7)
  val subalu1FLushList = List(0,1,2,3,4,5,6,7,8)


  alu0FLushList.foreach{ case i => when(io.flush(0) === true.B){allStageFlushVec(i) := io.flush(0)}}
  alu1FLushList.foreach{ case i => when(io.flush(1) === true.B){allStageFlushVec(i) := io.flush(1)}}
  subalu0FLushList.foreach{ case i => when(io.flush(2) === true.B){allStageFlushVec(i) := io.flush(2)}}
  subalu1FLushList.foreach{ case i => when(io.flush(3) === true.B){allStageFlushVec(i) := io.flush(3)}}

  pipeCtl.flush := allStageFlushVec


}
class Bypass extends Module{
  val io = IO(new Bundle {
    val in = Vec(2,Flipped(Decoupled(new DecodeIO)))
    val memStall = Input(Bool())
    val flush = Input(Vec(4,Bool()))
    val issueStall = Output(Vec(2,Bool()))
    val pipeFlush = Output(Vec(10,Bool()))
    val decodeBypassPkt = Vec(2, Decoupled(new BypassPkt))
    val BypassPkt = Vec(10, new BypassPkt)
    val BypassPktValid = Output(Vec(10,Bool()))
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
  for(i <- 0 to 9) BypassPktValid(i) := pipeOut(i).valid

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


  io.issueStall := DecodeIO2BypassPkt.io.issueStall
  io.decodeBypassPkt <> Seq(DecodeIO2BypassPkt.io.out0,DecodeIO2BypassPkt.io.out1)
  io.pipeFlush := PipelineCtl.pipeCtl.flush

  //pipeline connect
  //stall stage
  val pipeStage0 = Module(new stallPointConnect(new BypassPkt))
  val pipeStage1 = Module(new stallPointConnect(new BypassPkt))
  val pipeStage6 = Module(new stallPointConnect(new BypassPkt))
  val pipeStage7 = Module(new stallPointConnect(new BypassPkt))

  val stallStageList = List(pipeStage0,pipeStage1,pipeStage6,pipeStage7)
  val stallList = List(0,1,6,7)
  (stallStageList zip stallList).foreach{case (a,b) =>
    a.io.left <> pipeIn(b)
    a.io.right <> pipeOut(b)
    a.io.rightOutFire <> pipeFire(b)
    a.io.isFlush <> PipelineCtl.pipeCtl.flush(b)
  }
  pipeStage0.io.isStall := DecodeIO2BypassPkt.io.issueStall(0)
  pipeStage1.io.isStall := DecodeIO2BypassPkt.io.issueStall(1)
  pipeStage6.io.isStall := io.memStall
  pipeStage7.io.isStall := io.memStall

  //normal stage
  val pipeStage2 = Module(new normalPipeConnect(new BypassPkt))
  val pipeStage3 = Module(new normalPipeConnect(new BypassPkt))
  val pipeStage4 = Module(new normalPipeConnect(new BypassPkt))
  val pipeStage5 = Module(new normalPipeConnect(new BypassPkt))
  val pipeStage8 = Module(new normalPipeConnect(new BypassPkt))
  val pipeStage9 = Module(new normalPipeConnect(new BypassPkt))

  val normalStageList = List(pipeStage2,pipeStage3,pipeStage4,pipeStage5,pipeStage8,pipeStage9)
  val noralList = List(2,3,4,5,8,9)

  (normalStageList zip noralList).foreach{case (a,b) =>
    a.io.left <> pipeIn(b)
    a.io.right <> pipeOut(b)
    a.io.rightOutFire <> pipeFire(b)
    a.io.isFlush <> PipelineCtl.pipeCtl.flush(b)
  }


}




