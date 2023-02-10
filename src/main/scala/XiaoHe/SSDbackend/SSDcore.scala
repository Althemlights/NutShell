package XiaoHe.SSDbackend

import chisel3._

case class SSDCoreConfig(
                          EnableDebug: Boolean = true,
                          EnableDifftest : Boolean = true,
                          EnablePMU : Boolean = true,
                          EnablePipestageDebug : Boolean = false,
                          EnableLSUDebug : Boolean = false,
                          EnableStallCnt: Boolean = false,
                          EnablePerfCnt: Boolean = true,
                          EnableInstCnt: Boolean = true,
                          EnableCacheCnt: Boolean = true,
                          EnableStall1Cnt: Boolean = true,
                          EnableBPUCnt: Boolean = false,
                          EnableGHRDebug: Boolean = false,
                          EnableBPUupdateDebug: Boolean = false,
                          EnableRetDebug: Boolean = false,
                          EnableRedirectDebug: Boolean = false
                        )


