package top

import chisel3._
import chisel3.stage._
import nutcore._
import SSDbackend._
import SSDfrontend._
import sim.SimTop
import system._
import top.TopMain.args



object moduleTop extends App{
  lazy val config = NutCoreConfig(FPGAPlatform = false)
//  (new ChiselStage).execute(args, Seq(
//    ChiselGeneratorAnnotation(() => new NutCore()(config)))
////    ChiselGeneratorAnnotation(() => new testModule))
//  )
  (new chisel3.stage.ChiselStage).execute(args, Seq(
    chisel3.stage.ChiselGeneratorAnnotation(() =>Module(new ysyx_229999()(config))),
    firrtl.stage.RunFirrtlTransformAnnotation(new AddModulePrefix()),
    ModulePrefixAnnotation("ysyx_210062_")
  ))
}