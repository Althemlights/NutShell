package top

import nutcore.NutCoreConfig
import system.NutShell
import device.{AXI4VGA}
import sim.NutShellSimTop

import chisel3._

class Top extends Module {
  val io = IO(new Bundle{})
  val nutshell = Module(new NutShell()(NutCoreConfig()))
  val vga = Module(new AXI4VGA)

  nutshell.io := DontCare
  vga.io := DontCare
  dontTouch(nutshell.io)
  dontTouch(vga.io)
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
    case "seq"  => InOrderSettings()
    case "ooo"  => OOOSettings()
    case "small"=> EmbededSettings()
  } )
  s.map{Settings.settings += _} // add and overwrite DefaultSettings
  println("====== Settings = (" + board + ", " +  core + ") ======")
  def para2Hex(x: Any) = x match {
    case i: Long => i.toHexString
    case any => any
  }
  Settings.settings.toList.sortBy(_._1)(Ordering.String).map(s => println(s._1 + " = " + para2Hex(s._2)))

  if (board == "sim") {
    Driver.execute(args, () => new NutShellSimTop)
  } else {
    Driver.execute(args, () => new Top)
  }
}
