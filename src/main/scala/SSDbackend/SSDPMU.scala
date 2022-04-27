package SSDbackend

import chisel3._
import chisel3.util._

class PMUIO0 extends Bundle{
  val normali0 = Output(Bool())
  val normali1 = Output(Bool())

  val frontendi0 = Output(Bool())
  val frontendi1 = Output(Bool())

  val laterStageStalli0 = Output(Bool())
  val laterStageStalli1 = Output(Bool())

  val loadRsNotreadyi1 = Output(Bool())
  val storeRsNotreadyi1 = Output(Bool())
  val mulRsNotreadyi1 = Output(Bool())
  val divRsNotreadyi1 = Output(Bool())

  val i1Stalli0 = Output(Bool())
  val hitSubalui0 = Output(Bool())
  val bothLsui0 = Output(Bool())
  val LsuBri0 = Output(Bool())

  val loadRsNotreadyi0 = Output(Bool())
  val storeRsNotreadyi0 = Output(Bool())
  val mulRsNotreadyi0 = Output(Bool())
  val divRsNotreadyi0 = Output(Bool())

}

class PMU extends Module{
  val io = IO(new Bundle {
    val in0 = Flipped(new PMUIO0)
  })

  val in0 = io.in0
  val issue1Vec = VecInit(Seq(in0.normali1,in0.frontendi1,in0.laterStageStalli1,in0.loadRsNotreadyi1,in0.storeRsNotreadyi1,in0.mulRsNotreadyi1,in0.divRsNotreadyi1))
  val issue0Vec = VecInit(Seq(in0.normali0,in0.frontendi0,in0.laterStageStalli0,in0.i1Stalli0,in0.hitSubalui0,in0.bothLsui0,in0.LsuBri0,
    in0.loadRsNotreadyi0,in0.storeRsNotreadyi0,in0.mulRsNotreadyi0,in0.divRsNotreadyi0))
  //so ugly
  val issue1Num = WireInit(0.U(4.W))
  val issue0Num = WireInit(0.U(4.W))
  issue1Num := in0.normali1+in0.frontendi1+in0.laterStageStalli1+in0.loadRsNotreadyi1+in0.storeRsNotreadyi1+in0.mulRsNotreadyi1+in0.divRsNotreadyi1
  issue0Num := in0.normali0+in0.frontendi0+in0.laterStageStalli0+in0.i1Stalli0+in0.hitSubalui0+in0.bothLsui0+in0.LsuBri0+
    in0.loadRsNotreadyi0+in0.storeRsNotreadyi0+in0.mulRsNotreadyi0+in0.divRsNotreadyi0
//  assert(issue1Num === 1.U)
//  assert(issue0Num === 1.U)
  dontTouch(issue1Num)
  dontTouch(issue0Num)

}
