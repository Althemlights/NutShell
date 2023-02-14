package top

import chisel3._
import chisel3.stage._
import nutcore._
import SSDbackend._
import SSDfrontend._
import sim.SimTop
import system._
import top.TopMain.args

import freechips.rocketchip.diplomacy.{DisableMonitors, LazyModule, LazyModuleImp}

object moduleTop extends App{
  lazy val config = new DefaultConfig(FPGAPlatform = false)
  (new ChiselStage).execute(args, Seq(
    ChiselGeneratorAnnotation(DisableMonitors(p => LazyModule(new NutCore()(p)))(config).module _))
  )
}