package top

import chisel3._
import chisel3.util._
import utils._
import nutcore._
import system._
import huancun._

import chipsalliance.rocketchip.config._
import SSDbackend.{DCacheParamsKey, DCacheParameters, ICacheParamsKey, ICacheParameters}

class DefaultConfig(FPGAPlatform: Boolean = true) extends Config((site, here, up) => {
  case NutCoreParamsKey => NutCoreParameters(FPGAPlatform = FPGAPlatform)
  case DCacheParamsKey => DCacheParameters()
  case ICacheParamsKey => ICacheParameters()
  //case HCCacheParamsKey => HCCacheParameters()
  //case SoCParamsKey => SoCParameters()
  // case XSCoreParamsKey => XSCoreParameters()
  //case MonitorsEnabled => true
})

/*class DefaultConfig(FPGAPlatform: Boolean = true) extends Config(
  new BaseConfig(FPGAPlatform)
)*/