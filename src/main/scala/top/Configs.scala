package top

import chisel3._
import chisel3.util._
import utils._
import XiaoHe._
import system._
import huancun._

import chipsalliance.rocketchip.config._
import XiaoHe.SSDbackend.{DCacheParamsKey, DCacheParameters, ICacheParamsKey, ICacheParameters}
import device.{EnableJtag, XSDebugModuleParams}
import freechips.rocketchip.tile.{XLen, MaxHartIdBits}
import freechips.rocketchip.devices.debug._

class DefaultConfig(FPGAPlatform: Boolean = true) extends Config((site, here, up) => {
  case NutCoreParamsKey => NutCoreParameters(FPGAPlatform = FPGAPlatform)
  case DCacheParamsKey => DCacheParameters()
  case ICacheParamsKey => ICacheParameters()
  // DebugModule use code from XiangShan-nanhu
  case XLen => 64
  // To be different with XiangShan, Wukong remove SimpleBus because Wukong not support memory coherency 
  case DebugModuleKey => Some(XSDebugModuleParams(site(XLen)))
  case MaxHartIdBits => 2
  case EnableJtag => true.B
  //case HCCacheParamsKey => HCCacheParameters()
  //case SoCParamsKey => SoCParameters()
  // case XSCoreParamsKey => XSCoreParameters()
  //case MonitorsEnabled => true
})

/*class DefaultConfig(FPGAPlatform: Boolean = true) extends Config(
  new BaseConfig(FPGAPlatform)
)*/