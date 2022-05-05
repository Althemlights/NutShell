package SSDbackend

import chisel3._

case class SSDCoreConfig(
                        EnableDebug: Boolean = true,
                        EnablePipestageDebug : Boolean = false,
                        EnableLSUDebug : Boolean = false,
                        EnableStallCnt: Boolean = true,
                        EnablePerfCnt: Boolean = true,
                        EnableInstCnt: Boolean = true,
                        EnableCacheCnt: Boolean = true
                        )