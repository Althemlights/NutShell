package utils

import chisel3._
import chisel3.util._

import noop.NOOPConfig

object Debug {
  def apply(flag: Boolean = NOOPConfig().EnableDebug, cond: Bool = true.B)(body: => Unit): Any =
    if (flag) { when (cond && (GTimer() > 60258600.U && GTimer() < 60258850.U)) { body } }
}

object ShowType {
  def apply[T: Manifest](t: T) = println(manifest[T])
}
