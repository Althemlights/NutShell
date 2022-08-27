package SSDbackend

import chisel3._

case class SSDCoreConfig(
                          EnableDebug: Boolean = true,
                          EnableDifftest : Boolean = true,
                          EnablePMU : Boolean = true,
                          EnablePipestageDebug : Boolean = false,
                          EnableLSUDebug : Boolean = false,
                          EnableStallCnt: Boolean = true,
                          EnablePerfCnt: Boolean = true,
                          EnableInstCnt: Boolean = false,
                          EnableCacheCnt: Boolean = false,
                          EnableBPUCnt: Boolean = false,
                          EnableGHRDebug: Boolean = false,
                          EnableBPUupdateDebug: Boolean = false,
                          EnableRetDebug: Boolean = false,
                          EnableRedirectDebug: Boolean = false
                        )


