package SSDbackend

import chisel3._
import chisel3.util._
import nutcore._
import _root_.utils.LogUtil

object myDebug{

  def apply(fmt: String):Any ={
    printf(fmt)
  }
  def apply(cond: Bool,fmt: String, data: Bits*): Any =
    apply(cond,Printable.pack(fmt,data:_*))

  //UInt
  def apply(cond: Bool, pable: Printable): Any = {
      if (SSDCoreConfig().EnableDebug) {
        when(cond) {
          printf(pable)
        }
      }
    }


}

