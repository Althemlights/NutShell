package SSDbackend

import chisel3._
import chisel3.util.{DecoupledIO, _}

//class stallPointConnect[T <: Data](gen : T) extends Module {
//  val io = IO(new Bundle() {
//    val left = Flipped(DecoupledIO(gen))
//    val right = DecoupledIO(gen)
//    val rightOutFire = Input(Bool())
//    val isFlush = Input(Bool())
//    val isStall = Input(Bool())
//  })
//    val (left,right,rightOutFire,isFlush,isStall) = (io.left,io.right,io.rightOutFire,io.isFlush,io.isStall)
//
//    val valid = RegInit(false.B)
//    when (rightOutFire) { valid := false.B }
//    when (left.valid && right.ready && !isStall) { valid := true.B }
//    when (isStall) { valid := true.B }
//    when (isFlush) { valid := false.B }
//
//    //stall时将left.ready拉低，并在该级插入气泡
//    left.ready := right.ready && !isStall
//    right.bits := RegEnable(Mux(isStall,0.U.asTypeOf(left.bits),left.bits), left.valid && right.ready)
//    right.valid := valid //&& !isFlush
//
//
//}
class stallPointConnect[T <: Data](gen : T) extends Module {
  val io = IO(new Bundle() {
    val left = Flipped(DecoupledIO(gen))
    val right = DecoupledIO(gen)
    val rightOutFire = Input(Bool())
    val inValid = Input(Bool())
    val isFlush = Input(Bool())
    val isStall = Input(Bool())
  })
  val (left,right,rightOutFire,isFlush,isStall,inValid) = (io.left,io.right,io.rightOutFire,io.isFlush,io.isStall,io.inValid)

  val valid = RegInit(false.B)
  when (rightOutFire) { valid := false.B }
  when (left.valid && right.ready && !isStall && !inValid) { valid := true.B }
  when (isFlush) { valid := false.B }

  left.ready := right.ready && !isStall
  right.bits := RegEnable(left.bits, left.valid && left.ready && !inValid)
  right.valid := valid //&& !isFlush


}
class normalPipeConnect[T <: Data](gen : T) extends Module {
  val io = IO(new Bundle() {
    val left = Flipped(DecoupledIO(gen))
    val right = DecoupledIO(gen)
    val rightOutFire = Input(Bool())
    val inValid = Input(Bool())
    val isFlush = Input(Bool())
  })
  val (left,right,rightOutFire,isFlush,inValid) = (io.left,io.right,io.rightOutFire,io.isFlush,io.inValid)

  val valid = RegInit(false.B)
  when (rightOutFire) { valid := false.B }
  when (left.valid && right.ready && !inValid) { valid := true.B }
  when (isFlush) { valid := false.B }

  left.ready := right.ready
  right.bits := RegEnable(left.bits, left.valid && right.ready && !inValid)
  right.valid := valid //&& !isFlush


}

//object StallPointConnect {
//  def apply[T <: Data](left: DecoupledIO[T], right: DecoupledIO[T], rightOutFire: Bool, isFlush: Bool, isStall: Bool) = {
//    val valid = RegInit(false.B)
//    when (rightOutFire) { valid := false.B }
//    when (left.valid && right.ready && !isStall) { valid := true.B }
//    when (isStall) { valid := true.B }
//    when (isFlush) { valid := false.B }
//
//    //stall时将left.ready拉低，并在该级插入气泡
//    left.ready := right.ready && !isStall
//    right.bits := Mux(isStall,0.U.asTypeOf(left.bits),RegEnable(left.bits, left.valid && right.ready))
//    right.valid := valid //&& !isFlush
//
//  }
//}
