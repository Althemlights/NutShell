package SSDbackend

import chisel3._

case class SSDCoreConfig(
                        EnableDebug: Boolean = true,
                        EnablePerfCnt: Boolean = true
                        )