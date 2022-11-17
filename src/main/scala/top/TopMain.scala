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

package top

import XiaoHe.NutCoreConfig
import bus.axi4.ysyxAXI4IO
import system.XiaoHe
import device.AXI4VGA
import sim.SimTop
import chisel3._
import chisel3.stage._
import top.XiaoHeSim.args
import top.ysyx.args
class riscv_cpu_io extends Bundle {
  val master = new ysyxAXI4IO()
  val slave  = Flipped(new ysyxAXI4IO())
  val interrupt = Input(Bool())
}
class ysyx extends Module {
  val io : riscv_cpu_io = IO(new riscv_cpu_io)
  val core = Module(new XiaoHe()(NutCoreConfig()))
  core.io.master := DontCare
  core.io.slave := DontCare

  core.io.master.ar.ready     := io.master.arready
  io.master.arvalid         := core.io.master.ar.valid
  io.master.araddr          := core.io.master.ar.bits.addr
  io.master.arid            := core.io.master.ar.bits.id
  io.master.arlen           := core.io.master.ar.bits.len
  io.master.arsize          := core.io.master.ar.bits.size
  io.master.arburst         := core.io.master.ar.bits.burst


  core.io.master.r.valid      := io.master.rvalid
  io.master.rready          := core.io.master.r.ready
  core.io.master.r.bits.resp  := io.master.rresp
  core.io.master.r.bits.data  := io.master.rdata
  core.io.master.r.bits.last  := io.master.rlast
  core.io.master.r.bits.id    := io.master.rid


  core.io.master.aw.ready     := io.master.awready
  io.master.awvalid         := core.io.master.aw.valid
  io.master.awaddr          := core.io.master.aw.bits.addr
  io.master.awid            := core.io.master.aw.bits.id
  io.master.awlen           := core.io.master.aw.bits.len
  io.master.awsize          := core.io.master.aw.bits.size
  io.master.awburst         := core.io.master.aw.bits.burst

  core.io.master.w.ready      := io.master.wready
  io.master.wvalid          := core.io.master.w.valid
  io.master.wdata           := core.io.master.w.bits.data
  io.master.wlast           := core.io.master.w.bits.last
  io.master.wstrb           := core.io.master.w.bits.strb

  io.master.bready          := core.io.master.b.ready
  core.io.master.b.valid      := io.master.bvalid
  core.io.master.b.bits.resp  := io.master.bresp
  core.io.master.b.bits.id    := io.master.bid

  io.slave := DontCare
  io.slave.arready := false.B
  io.slave.rvalid  := false.B
  io.slave.awready := false.B
  io.slave.wready  := false.B
  io.slave.bvalid  := false.B
//  io.interrupt := false.B
  core.io.interrupt := false.B
}


class Top extends Module {
  val io = IO(new Bundle{})
  val nutshell = Module(new XiaoHe()(NutCoreConfig()))
  val vga = Module(new AXI4VGA)

  nutshell.io := DontCare
  vga.io := DontCare
//  dontTouch(nutshell.io)
//  dontTouch(vga.io)
}

object TopMain extends App {
  def parseArgs(info: String, args: Array[String]): String = {
    var target = ""
    for (arg <- args) { if (arg.startsWith(info + "=") == true) { target = arg } }
    require(target != "")
    target.substring(info.length()+1)
  }
  val board = parseArgs("BOARD", args)
  val core = parseArgs("CORE", args)

  val s = (board match {
    case "sim"    => Nil
    case "pynq"   => PynqSettings()
    case "axu3cg" => Axu3cgSettings()
    case "PXIe"   => PXIeSettings()
    case "soctest" => SoCTestSettings()
  } ) ++ ( core match {
    case "inorder"  => InOrderSettings()
    case "ooo"  => OOOSettings()
    case "embedded"=> EmbededSettings()
  } )
  s.foreach{Settings.settings += _} // add and overwrite DefaultSettings
  println("====== Settings = (" + board + ", " +  core + ") ======")
  Settings.settings.toList.sortBy(_._1)(Ordering.String).foreach {
    case (f, v: Long) =>
      println(f + " = 0x" + v.toHexString)
    case (f, v) =>
      println(f + " = " + v)
  }
  if (board == "sim") {
    (new chisel3.stage.ChiselStage).execute(args, Seq(
      chisel3.stage.ChiselGeneratorAnnotation(() =>new SimTop())
    ))
  } else {
    // Driver.execute(args, () => new Top)
    (new chisel3.stage.ChiselStage).execute(args, Seq(
      chisel3.stage.ChiselGeneratorAnnotation(() =>new ysyx()),
      firrtl.stage.RunFirrtlTransformAnnotation(new AddModulePrefix()),
      ModulePrefixAnnotation("ysyx_229999_")
    ))
  }
}

object ysyx extends App{
  lazy val config = NutCoreConfig(FPGAPlatform = false)
  //  (new ChiselStage).execute(args, Seq(
  //    ChiselGeneratorAnnotation(() => new NutCore()(config)))
  ////    ChiselGeneratorAnnotation(() => new testModule))
  //  )
  (new chisel3.stage.ChiselStage).execute(args, Seq(
    chisel3.stage.ChiselGeneratorAnnotation(() =>new ysyx()),
    firrtl.stage.RunFirrtlTransformAnnotation(new AddModulePrefix()),
    ModulePrefixAnnotation("ysyx_229999_")
  ))
}

object XiaoHeSim extends App{
  lazy val config = NutCoreConfig(FPGAPlatform = false)
  (new chisel3.stage.ChiselStage).execute(args, Seq(
    chisel3.stage.ChiselGeneratorAnnotation(() =>new SimTop())
  ))
}