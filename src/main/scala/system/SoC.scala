package system

import chipsalliance.rocketchip.config.{Field, Parameters}
import chisel3._
import chisel3.util._
import top.Settings
import SSDbackend._

case object NutCoreParamsKey extends Field[NutCoreParameters]

case object MonitorsEnabled extends Field[Boolean]

case class NutCoreParameters (
                           FPGAPlatform: Boolean = true,
                           EnableDebug: Boolean = Settings.get("EnableDebug"),
                           EnhancedLog: Boolean = true
                         )

trait HasNutCoreParameters {
  implicit val p: Parameters
  val p_nutcore = p(NutCoreParamsKey)

  val FPGAPlatform = p_nutcore.FPGAPlatform
}




