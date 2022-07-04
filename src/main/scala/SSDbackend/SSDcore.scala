package SSDbackend

import chisel3._

case class SSDCoreConfig(
                          EnableDebug: Boolean = true,
                          EnableDifftest : Boolean = false,
                          EnablePipestageDebug : Boolean = false,
                          EnableLSUDebug : Boolean = false,
                          EnableStallCnt: Boolean = false,
                          EnablePerfCnt: Boolean = true,
                          EnableInstCnt: Boolean = true,
                          EnableCacheCnt: Boolean = false,
                          EnableGHRDebug: Boolean = false,
                          EnableBPUupdateDebug: Boolean = false,
                          EnableRetDebug: Boolean = false,
                          EnableRedirectDebug: Boolean = false
                        )


