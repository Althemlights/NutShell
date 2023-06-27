package top

import chisel3._

case class WuKongConfig(
                          EnableDifftest : Boolean = false,
                          EnablePMU : Boolean = true,
                          EnablePipestageDebug : Boolean = false,
                          EnableLSUDebug : Boolean = false,
                          EnableStallCnt: Boolean = false,
                          EnablePerfCnt: Boolean = true,
                          EnableInstCnt: Boolean = false,
                          EnableCacheCnt: Boolean = false,
                          EnableStall1Cnt: Boolean = true,
                          EnableBPUCnt: Boolean = true,
                          EnableGHRDebug: Boolean = false,
                          EnableBPUupdateDebug: Boolean = false,
                          EnableRetDebug: Boolean = false,
                          EnableRedirectDebug: Boolean = false,
                          FPGAPlatform: Boolean = false,
                          EnableDebug: Boolean = Settings.get("EnableDebug"),
                          EnhancedLog: Boolean = true
                        )


