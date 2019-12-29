package top

import noop.NOOPConfig
import system.NOOPSoC
import device.{AXI4Timer, AXI4VGA, AXI4Flash}
import gpu._

import chisel3._

class Top extends Module {
  val io = IO(new Bundle{})
  val noop = Module(new NOOPSoC()(NOOPConfig()))
  val timer = Module(new AXI4Timer)
  val vga = Module(new AXI4VGA)
  val flash = Module(new AXI4Flash)
//  val gpu = Module(new AXI4GPU)

  noop.io := DontCare
  timer.io := DontCare
  vga.io := DontCare
  flash.io := DontCare
//  gpu.io := DontCare
  dontTouch(noop.io)
  dontTouch(timer.io)
  dontTouch(vga.io)
  dontTouch(flash.io)
//  dontTouch(gpu.io)
}

object TopMain extends App {
  Driver.execute(args, () => new Top)
}
