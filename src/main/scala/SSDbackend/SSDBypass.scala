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
  val i1hiti0 = Wire(Bool())

  i0rs1valid := io.in(0).bits.ctrl.src1Type === "b0".U
  i0rs2valid := io.in(0).bits.ctrl.src2Type === "b0".U
  i1rs1valid := io.in(1).bits.ctrl.src1Type === "b0".U
  i1rs2valid := io.in(1).bits.ctrl.src2Type === "b0".U
  i1hiti0 := (io.in(1).bits.ctrl.rfSrc1 === io.in(0).bits.ctrl.rfDest && i1rs1valid && i0decodePkt.rdvalid
    || io.in(1).bits.ctrl.rfSrc2 === io.in(0).bits.ctrl.rfDest && i1rs2valid && i0decodePkt.rdvalid)
  //hit stage
  val i0rs1hitStage = 10.U(4.W)
  val i0rs2hitStage = 10.U(4.W)
  val i1rs1hitStage = 10.U(4.W)
  val i1rs2hitStage = 10.U(4.W)

  for (i <- 9 to 0) {
    when(io.in(0).bits.ctrl.rfSrc1 === io.BypassPktTable(i).decodePkt.rd && io.BypassPktTable(i).decodePkt.rdvalid && io.BypassPktValid(i)) {
      i0rs1hitStage := i.U
    }
    when(io.in(0).bits.ctrl.rfSrc2 === io.BypassPktTable(i).decodePkt.rd && io.BypassPktTable(i).decodePkt.rdvalid && io.BypassPktValid(i)) {
      i0rs2hitStage := i.U
    }
    when(io.in(0).bits.ctrl.rfSrc1 === io.BypassPktTable(i).decodePkt.rd && io.BypassPktTable(i).decodePkt.rdvalid && io.BypassPktValid(i)) {
      i1rs1hitStage := i.U
    }
    when(io.in(0).bits.ctrl.rfSrc2 === io.BypassPktTable(i).decodePkt.rd && io.BypassPktTable(i).decodePkt.rdvalid && io.BypassPktValid(i)) {
      i1rs2hitStage := i.U
    }
  }
    

    //merge decodePkt.subalu

    io.out0.bits.decodePkt.subalu := (i0decodePkt.alu && i0rs1hitStage >= 0.U && i0rs1hitStage <= 3.U && (io.BypassPktTable(i0rs1hitStage).decodePkt.mul || io.BypassPktTable(i0rs1hitStage).decodePkt.load)
      || i0decodePkt.alu && i0rs2hitStage >= 0.U && i0rs2hitStage <= 3.U && (io.BypassPktTable(i0rs2hitStage).decodePkt.mul || io.BypassPktTable(i0rs2hitStage).decodePkt.load)
      || i0decodePkt.alu && i0rs1hitStage >= 0.U && i0rs1hitStage <= 5.U && io.BypassPktTable(i0rs1hitStage).decodePkt.subalu
      || i0decodePkt.alu && i0rs2hitStage >= 0.U && i0rs2hitStage <= 5.U && io.BypassPktTable(i0rs2hitStage).decodePkt.subalu
      )

    io.out1.bits.decodePkt.subalu := (i1decodePkt.alu && i1rs1hitStage >= 1.U && i1rs1hitStage <= 3.U && (io.BypassPktTable(i1rs1hitStage).decodePkt.mul || io.BypassPktTable(i1rs1hitStage).decodePkt.load)
      || i1decodePkt.alu && i1rs2hitStage >= 1.U && i1rs2hitStage <= 3.U && (io.BypassPktTable(i1rs2hitStage).decodePkt.mul || io.BypassPktTable(i1rs2hitStage).decodePkt.load)
      || i1decodePkt.alu && i1rs1hitStage >= 1.U && i1rs1hitStage <= 5.U && io.BypassPktTable(i1rs1hitStage).decodePkt.subalu
      || i1decodePkt.alu && i1rs2hitStage >= 1.U && i1rs2hitStage <= 5.U && io.BypassPktTable(i1rs2hitStage).decodePkt.subalu
      || i1hiti0 && (i0decodePkt.mul || i0decodePkt.load) && i1decodePkt.alu
      )

    //issue stall
    //这部分暂时先只考虑整数指令需要stall的情况（额，整数指令好像这一步不会stall)
    io.issueStall := VecInit(Seq(false.B,false.B))

    //BypassCtl
    val FuType = VecInit(Seq.fill(10)(0.U.asTypeOf(new decodePkt)))
    for (i <- 0 to 0) FuType(i) := io.BypassPktTable(i).decodePkt
    val Valid = VecInit(Seq.fill(10)(false.B))
    for (i <- 0 to 0) Valid(i) := io.BypassPktValid(i)
    //BypassPkt out0
    io.out0.bits.BypassCtl.rs1bypasse0 := Seq(
      Valid(0) && i0rs1hitStage === 0.U && FuType(0).alu && !FuType(0).subalu,
      Valid(1) && i0rs1hitStage === 1.U && FuType(1).alu && !FuType(1).subalu,
      Valid(2) && i0rs1hitStage === 2.U && FuType(2).alu && !FuType(2).subalu,
      Valid(3) && i0rs1hitStage === 3.U && FuType(3).alu && !FuType(3).subalu,
      Valid(4) && i0rs1hitStage === 4.U && FuType(4).alu && !FuType(4).subalu,
      Valid(5) && i0rs1hitStage === 5.U && FuType(5).alu && !FuType(5).subalu,
      Valid(4) && i0rs1hitStage === 4.U && FuType(4).load || Valid(5) && i0rs1hitStage === 5.U && FuType(5).load,
      Valid(4) && i0rs1hitStage === 4.U && FuType(4).mul || Valid(5) && i0rs1hitStage === 5.U && FuType(5).mul,
      Valid(6) && i0rs1hitStage === 6.U,
      Valid(7) && i0rs1hitStage === 7.U,
      Valid(8) && i0rs1hitStage === 8.U,
      Valid(9) && i0rs1hitStage === 9.U
    )
    io.out0.bits.BypassCtl.rs2bypasse0 := Seq(
      Valid(0) && i0rs2hitStage === 0.U && FuType(0).alu && !FuType(0).subalu,
      Valid(1) && i0rs2hitStage === 1.U && FuType(1).alu && !FuType(1).subalu,
      Valid(2) && i0rs2hitStage === 2.U && FuType(2).alu && !FuType(2).subalu,
      Valid(3) && i0rs2hitStage === 3.U && FuType(3).alu && !FuType(3).subalu,
      Valid(4) && i0rs2hitStage === 4.U && FuType(4).alu && !FuType(4).subalu,
      Valid(5) && i0rs2hitStage === 5.U && FuType(5).alu && !FuType(5).subalu,
      Valid(4) && i0rs2hitStage === 4.U && FuType(4).load || Valid(5) && i0rs1hitStage === 5.U && FuType(5).load,
      Valid(4) && i0rs2hitStage === 4.U && FuType(4).mul || Valid(5) && i0rs1hitStage === 5.U && FuType(5).mul,
      Valid(6) && i0rs2hitStage === 6.U,
      Valid(7) && i0rs2hitStage === 7.U,
      Valid(8) && i0rs2hitStage === 8.U,
      Valid(9) && i0rs2hitStage === 9.U
    )
    io.out0.bits.BypassCtl.rs1bypasse2 := Seq(
      Valid(4) && i0rs1hitStage === 4.U && FuType(4).alu && FuType(4).subalu,
      Valid(5) && i0rs1hitStage === 5.U && FuType(5).alu && FuType(5).subalu
    )
    io.out0.bits.BypassCtl.rs2bypasse2 := Seq(
      Valid(4) && i0rs2hitStage === 4.U && FuType(4).alu && FuType(4).subalu,
      Valid(5) && i0rs2hitStage === 5.U && FuType(5).alu && FuType(5).subalu
    )
    io.out0.bits.BypassCtl.rs1bypasse3 := Seq(
      false.B,
      Valid(0) && i0rs1hitStage === 0.U && FuType(0).alu && FuType(0).subalu
        || Valid(0) && i0rs1hitStage === 0.U && FuType(0).load
        || Valid(0) && i0rs1hitStage === 0.U && FuType(0).mul,
      Valid(1) && i0rs1hitStage === 1.U && FuType(1).load
        || Valid(1) && i0rs1hitStage === 1.U && FuType(1).alu && FuType(1).subalu
        || Valid(1) && i0rs1hitStage === 1.U && FuType(1).mul,
      Valid(2) && i0rs1hitStage === 2.U && FuType(2).alu && FuType(2).subalu
        || Valid(2) && i0rs1hitStage === 2.U && FuType(2).load
        || Valid(2) && i0rs1hitStage === 2.U && FuType(2).mul,
      Valid(3) && i0rs1hitStage === 3.U && FuType(3).alu && FuType(3).subalu
        || Valid(3) && i0rs1hitStage === 3.U && FuType(3).load
        || Valid(3) && i0rs1hitStage === 3.U && FuType(3).mul
    )



    io.out0.bits.BypassCtl.rs2bypasse3 := Seq(
      false.B,
      Valid(0) && i0rs2hitStage === 0.U && FuType(0).alu && FuType(0).subalu
        || Valid(0) && i0rs2hitStage === 0.U && FuType(0).load
        || Valid(0) && i0rs2hitStage === 0.U && FuType(0).mul,
      Valid(1) && i0rs2hitStage === 1.U && FuType(1).load
        || Valid(1) && i0rs2hitStage === 1.U && FuType(1).alu && FuType(1).subalu
        || Valid(1) && i0rs2hitStage === 1.U && FuType(1).mul,
      Valid(2) && i0rs2hitStage === 2.U && FuType(2).alu && FuType(2).subalu
        || Valid(2) && i0rs2hitStage === 2.U && FuType(2).load
        || Valid(2) && i0rs2hitStage === 2.U && FuType(2).mul,
      Valid(3) && i0rs2hitStage === 3.U && FuType(3).alu && FuType(3).subalu
        || Valid(3) && i0rs2hitStage === 3.U && FuType(3).load
        || Valid(3) && i0rs2hitStage === 3.U && FuType(3).mul
    )
    //BypassPkt out1
    io.out1.bits.BypassCtl.rs1bypasse0 := Seq(
      Valid(0) && i1rs1hitStage === 0.U && FuType(0).alu && !FuType(0).subalu,
      Valid(1) && i1rs1hitStage === 1.U && FuType(1).alu && !FuType(1).subalu,
      Valid(2) && i1rs1hitStage === 2.U && FuType(2).alu && !FuType(2).subalu,
      Valid(3) && i1rs1hitStage === 3.U && FuType(3).alu && !FuType(3).subalu,
      Valid(4) && i1rs1hitStage === 4.U && FuType(4).alu && !FuType(4).subalu,
      Valid(5) && i1rs1hitStage === 5.U && FuType(5).alu && !FuType(5).subalu,
      Valid(4) && i1rs1hitStage === 4.U && FuType(4).load || Valid(5) && i1rs1hitStage === 5.U && FuType(5).load,
      Valid(4) && i1rs1hitStage === 4.U && FuType(4).mul || Valid(5) && i1rs1hitStage === 5.U && FuType(5).mul,
      Valid(6) && i1rs1hitStage === 6.U,
      Valid(7) && i1rs1hitStage === 7.U,
      Valid(8) && i1rs1hitStage === 8.U,
      Valid(9) && i1rs1hitStage === 9.U
    )
    io.out1.bits.BypassCtl.rs2bypasse0 := Seq(
      Valid(0) && i1rs2hitStage === 0.U && FuType(0).alu && !FuType(0).subalu,
      Valid(1) && i1rs2hitStage === 1.U && FuType(1).alu && !FuType(1).subalu,
      Valid(2) && i1rs2hitStage === 2.U && FuType(2).alu && !FuType(2).subalu,
      Valid(3) && i1rs2hitStage === 3.U && FuType(3).alu && !FuType(3).subalu,
      Valid(4) && i1rs2hitStage === 4.U && FuType(4).alu && !FuType(4).subalu,
      Valid(5) && i1rs2hitStage === 5.U && FuType(5).alu && !FuType(5).subalu,
      Valid(4) && i1rs2hitStage === 4.U && FuType(4).load || Valid(5) && i1rs1hitStage === 5.U && FuType(5).load,
      Valid(4) && i1rs2hitStage === 4.U && FuType(4).mul || Valid(5) && i1rs1hitStage === 5.U && FuType(5).mul,
      Valid(6) && i1rs2hitStage === 6.U,
      Valid(7) && i1rs2hitStage === 7.U,
      Valid(8) && i1rs2hitStage === 8.U,
      Valid(9) && i1rs2hitStage === 9.U
    )
    io.out1.bits.BypassCtl.rs1bypasse2 := Seq(
      Valid(4) && i1rs1hitStage === 4.U && FuType(4).alu && FuType(4).subalu,
      Valid(5) && i1rs1hitStage === 5.U && FuType(5).alu && FuType(5).subalu
    )
    io.out1.bits.BypassCtl.rs2bypasse2 := Seq(
      Valid(4) && i1rs2hitStage === 4.U && FuType(4).alu && FuType(4).subalu,
      Valid(5) && i1rs2hitStage === 5.U && FuType(5).alu && FuType(5).subalu
    )
    io.out1.bits.BypassCtl.rs1bypasse3 := Seq(
      io.in(1).bits.ctrl.rfSrc1 === i1decodePkt.rd && i1decodePkt.alu && i1decodePkt.subalu && i0decodePkt.rdvalid && (i0decodePkt.mul || i0decodePkt.load),
      Valid(0) && i0rs1hitStage === 0.U && FuType(0).alu && FuType(0).subalu
        || Valid(0) && i0rs1hitStage === 0.U && FuType(0).load
        || Valid(0) && i0rs1hitStage === 0.U && FuType(0).mul,
      Valid(1) && i0rs1hitStage === 1.U && FuType(1).load
        || Valid(1) && i0rs1hitStage === 1.U && FuType(1).alu && FuType(1).subalu
        || Valid(1) && i0rs1hitStage === 1.U && FuType(1).mul,
      Valid(2) && i0rs1hitStage === 2.U && FuType(2).alu && FuType(2).subalu
        || Valid(2) && i0rs1hitStage === 2.U && FuType(2).load
        || Valid(2) && i0rs1hitStage === 2.U && FuType(2).mul,
      Valid(3) && i0rs1hitStage === 3.U && FuType(3).alu && FuType(3).subalu
        || Valid(3) && i0rs1hitStage === 3.U && FuType(3).load
        || Valid(3) && i0rs1hitStage === 3.U && FuType(3).mul
    )
    io.out1.bits.BypassCtl.rs2bypasse3 := Seq(
      io.in(1).bits.ctrl.rfSrc2 === i1decodePkt.rd && i1decodePkt.alu && i1decodePkt.subalu && i0decodePkt.rdvalid && (i0decodePkt.mul || i0decodePkt.load),
      Valid(0) && i0rs2hitStage === 0.U && FuType(0).alu && FuType(0).subalu
        || Valid(0) && i0rs2hitStage === 0.U && FuType(0).load
        || Valid(0) && i0rs2hitStage === 0.U && FuType(0).mul,
      Valid(1) && i0rs2hitStage === 1.U && FuType(1).load
        || Valid(1) && i0rs2hitStage === 1.U && FuType(1).alu && FuType(1).subalu
        || Valid(1) && i0rs2hitStage === 1.U && FuType(1).mul,
      Valid(2) && i0rs2hitStage === 2.U && FuType(2).alu && FuType(2).subalu
        || Valid(2) && i0rs2hitStage === 2.U && FuType(2).load
        || Valid(2) && i0rs2hitStage === 2.U && FuType(2).mul,
      Valid(3) && i0rs2hitStage === 3.U && FuType(3).alu && FuType(3).subalu
        || Valid(3) && i0rs2hitStage === 3.U && FuType(3).load
        || Valid(3) && i0rs2hitStage === 3.U && FuType(3).mul
    )
}

object DecodeIO2decodePkt {
  def apply(in:DecodeIO,out:decodePkt){
    out.rd <> in.ctrl.rfDest
    out.rdvalid <> in.ctrl.rfWen
    out.alu := in.ctrl.fuType === "b000".U
    //下面的就先置零，subalu 会在其他模块覆盖掉
    out.mul := false.B
    out.div := false.B
    out.load := false.B
    out.store := false.B
    out.subalu := false.B
  }
}
/*class PktPipeline extends Module{
  val io = IO(new Bundle{
    val i0In = Flipped(Decoupled(new BypassPkt))
    val i1In = Flipped(Decoupled(new BypassPkt))
    val stallFlush = Flipped(new StallFlushIO)
    val out = Vec(10,Decoupled(new BypassPkt))
  })
    //stall point
    val i0pipeStall = io.stallFlush.stall(0)
    val i1pipeStall = io.stallFlush.stall(1)
    val memStall = io.stallFlush.stall(2)
    //pipeline in, out, fire
    val pipeFire = VecInit(Seq.fill(10)(false.B))

    val pipein = Wire(Vec(10,Flipped(Decoupled(new BypassPkt))))
    val pipeout = io.out

    //pipeline0 : 1,3,5,7,9
    //pipeline1 : 0,2,4,6,8
    //pipeline in out
    pipein(0) := io.i0In
    pipein(1) := io.i1In
    for (i <- 2 to 9) {
      pipein(i) :=  pipeout(i - 2)
    }

    //fire signal
    for (i <- 0 to 9) {
      if (i == 8 || i == 9) pipeFire(i) := false.B
      else pipeFire(i) := pipeout(i).valid && pipein(i + 2).ready
    }
    //ready & valid
    pipeout(8).ready := true.B
    pipeout(9).ready := true.B

    //pipeline connect
    StallPointConnect(pipein(0), pipeout(0), pipeFire(0), io.stallFlush.flush(0), i0pipeStall)
    StallPointConnect(pipein(1), pipeout(1), pipeFire(1), io.stallFlush.flush(1), i1pipeStall)
    StallPointConnect(pipein(4), pipeout(4), pipeFire(4), io.stallFlush.flush(4), memStall)
    StallPointConnect(pipein(5), pipeout(5), pipeFire(5), io.stallFlush.flush(5), memStall)
    for (i <- 0 to 9) {
      if (i != 0 && i != 1 && i != 4 && i != 5) {
        PipelineConnect(pipein(i), pipeout(i), pipeFire(i), io.stallFlush.flush(i))
      }
    }
    //io.out := pipeout
}*/
class PipeCtl extends Module{
  val io = IO(new Bundle{
    val i0pipeStall = Input(Bool())
    val i1pipeStall = Input(Bool())
    val memStall = Input(Bool())
    val flush = Input(Vec(4,Bool()))  //flushPoint(0,3) -> alu0,alu1,sub_alu0,sub_alu1
  })
  val pipeCtl = IO(new StallFlushIO)

  // stall vec
  pipeCtl.stall(0) := io.i1pipeStall
  pipeCtl.stall(1) := io.i0pipeStall
  pipeCtl.stall(2) := io.memStall

  //flush vec
  val allStageFlushVec = VecInit(Seq.fill(10)(false.B))

  //pipeline0 : 1,3,5,7,9
  //pipeline1 : 0,2,4,6,8
  val alu0FLushList = List(0,1)
  val alu1FLushList = List(0)
  val subalu0FLushList = List(0,1,2,3,4,5,6,7)
  val subalu1FLushList = List(0,1,2,3,4,5,6)


  alu0FLushList.map(i =>{
    allStageFlushVec(i) := io.flush(0)
  })

  alu1FLushList.map(i =>{
    allStageFlushVec(i) := io.flush(1)
  })

  subalu0FLushList.map(i =>{
    allStageFlushVec(i) := io.flush(2)
  })

  subalu1FLushList.map(i =>{
    allStageFlushVec(i) := io.flush(3)
  })

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
  pipeIn(0) <> DecodeIO2BypassPkt.io.out0
  pipeIn(1) <> DecodeIO2BypassPkt.io.out1

  for (i <- 2 to 9) {
    pipeIn(i).bits := pipeOut(i - 2).bits
    pipeOut(i - 2).ready := pipeIn(i).ready
    pipeIn(i).valid := pipeOut(i - 2).valid
  }

  for (i <- 0 to 9) {
    if (i == 8 || i == 9) pipeFire(i) := false.B
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
  DecodeIO2BypassPkt.io.in <> io.in


  io.issueStall := DecodeIO2BypassPkt.io.issueStall
  io.decodeBypassPkt <> Seq(DecodeIO2BypassPkt.io.out0,DecodeIO2BypassPkt.io.out1)
  io.pipeFlush := PipelineCtl.pipeCtl.flush

  //pipeline connect
  //stall stage
  val pipeStage0 = Module(new stallPointConnect(new BypassPkt))
  val pipeStage1 = Module(new stallPointConnect(new BypassPkt))
  val pipeStage4 = Module(new stallPointConnect(new BypassPkt))
  val pipeStage5 = Module(new stallPointConnect(new BypassPkt))

  val stallStageList = List(pipeStage0,pipeStage1,pipeStage4,pipeStage5)
  val stallList = List(0,1,4,5)
  (stallStageList zip stallList).foreach{case (a,b) =>
    a.io.left <> pipeIn(b)
    a.io.right <> pipeOut(b)
    a.io.rightOutFire <> pipeFire(b)
    a.io.isFlush <> PipelineCtl.pipeCtl.flush(b)
  }
  pipeStage0.io.isStall := DecodeIO2BypassPkt.io.issueStall(0)
  pipeStage1.io.isStall := DecodeIO2BypassPkt.io.issueStall(1)
  pipeStage4.io.isStall := io.memStall
  pipeStage5.io.isStall := io.memStall

  //object connect
  /*StallPointConnect(pipeIn(0), pipeOut(0), pipeFire(0), PipelineCtl.pipeCtl.flush(0), DecodeIO2BypassPkt.io.issueStall(0))
  StallPointConnect(pipeIn(1), pipeOut(1), pipeFire(1), PipelineCtl.pipeCtl.flush(1), DecodeIO2BypassPkt.io.issueStall(1))
  StallPointConnect(pipeIn(4), pipeOut(4), pipeFire(4), PipelineCtl.pipeCtl.flush(4), io.memStall)
  StallPointConnect(pipeIn(5), pipeOut(5), pipeFire(5), PipelineCtl.pipeCtl.flush(5), io.memStall)*/

  //normal stage
  val pipeStage2 = Module(new normalPipeConnect(new BypassPkt))
  val pipeStage3 = Module(new normalPipeConnect(new BypassPkt))
  val pipeStage6 = Module(new normalPipeConnect(new BypassPkt))
  val pipeStage7 = Module(new normalPipeConnect(new BypassPkt))
  val pipeStage8 = Module(new normalPipeConnect(new BypassPkt))
  val pipeStage9 = Module(new normalPipeConnect(new BypassPkt))

  val normalStageList = List(pipeStage2,pipeStage3,pipeStage6,pipeStage7,pipeStage8,pipeStage9)
  val noralList = List(2,3,6,7,8,9)

  (normalStageList zip noralList).foreach{case (a,b) =>
    a.io.left <> pipeIn(b)
    a.io.right <> pipeOut(b)
    a.io.rightOutFire <> pipeFire(b)
    a.io.isFlush <> PipelineCtl.pipeCtl.flush(b)
  }

/*  for (i <- 0 to 9) {
    if (i != 0 && i != 1 && i != 4 && i != 5) {
      PipelineConnect(pipeIn(i), pipeOut(i), pipeFire(i), PipelineCtl.pipeCtl.flush(i))
    }
  }*/

}




