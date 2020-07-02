package utils

import chisel3._
import chisel3.util._

import noop.NOOPConfig

object Debug {
  def apply(flag: Boolean = NOOPConfig().EnableDebug, cond: Bool = true.B)(body: => Unit): Any =
    if (flag) { when (cond && GTimer() > 0.U) { body } } // 2645000
    // if (flag) { when (cond && GTimer() > 11125600.U) { body } } // 2645000
}

object ShowType {
  def apply[T: Manifest](t: T) = println(manifest[T])
}
