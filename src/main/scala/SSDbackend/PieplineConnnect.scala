package SSDbackend

import chisel3._
import chisel3.util._

object StallPointConnect {
  def apply[T <: Data](left: DecoupledIO[T], right: DecoupledIO[T], rightOutFire: Bool, isFlush: Bool, isStall: Bool) = {
    val valid = RegInit(false.B)
    when (rightOutFire) { valid := false.B }
    when (left.valid && right.ready && !isStall) { valid := true.B }
    when (isStall) { valid := true.B }
    when (isFlush) { valid := false.B }

    //stall时将left.ready拉低，并在该级插入气泡
    left.ready := right.ready && !isStall
    right.bits := Mux(isStall,0.U.asTypeOf(left.bits),RegEnable(left.bits, left.valid && right.ready))
    right.valid := valid //&& !isFlush

  }
}