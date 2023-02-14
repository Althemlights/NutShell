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

import nutcore.NutCoreConfig
import system.NutShell
import device.{AXI4VGA}
import sim.SimTop

import chisel3._
import chisel3.stage._

import freechips.rocketchip.diplomacy.{DisableMonitors, AddressSet, LazyModule, LazyModuleImp}
import chipsalliance.rocketchip.config._

class Top extends Module {
  val io = IO(new Bundle{})
  lazy val config = new DefaultConfig(FPGAPlatform = false)
  val l_nutshell = LazyModule(new NutShell()(config))
  val nutshell = Module(l_nutshell.module)
  val vga = Module(new AXI4VGA)

  nutshell.io := DontCare
  vga.io := DontCare
  dontTouch(nutshell.io)
  dontTouch(vga.io)
}

object Generator {
  def execute(args: Array[String], mod: => RawModule) {
    (new ChiselStage).execute(args, Seq(
      ChiselGeneratorAnnotation(mod _)))
  }
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
  
  val config = new DefaultConfig(false)

  if (board == "sim") {
    Generator.execute(args, DisableMonitors(p => new SimTop()(p))(config))
    /*(new ChiselStage).execute(args, Seq(
      //ChiselGeneratorAnnotation(DisableMonitors(p => LazyModule(new SimTop()(p)))(config).module _))
      ChiselGeneratorAnnotation(DisableMonitors(p => new SimTop()(p))(config))
      )
    )*/
  } else {
    (new ChiselStage).execute(args, Seq(
      ChiselGeneratorAnnotation(() => new Top))
    )
  }
}

