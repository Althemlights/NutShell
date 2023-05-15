/**************************************************************************************
 * Copyright (c) 2020 Institute of Computing Technology, CAS
 * Copyright (c) 2020 University of Chinese Academy of Sciences
 *
 * WuKong is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 *             http://license.coscl.org.cn/MulanPSL2
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND, EITHER
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT, MERCHANTABILITY OR
 * FIT FOR A PARTICULAR PURPOSE.
 *
 * See the Mulan PSL v2 for more details.
 ***************************************************************************************/

package WuKong

import WuKong.Frontend.Frontend
import bus.simplebus._
import chisel3._
import chisel3.util._
import WuKong.Backend._
import WuKong.Backend.fu.HasExceptionNO
import top.Settings
import WuKong.Backend.Backend
import WuKong.Backend.{ICacheConfig, ICache}
import top.WuKongConfig
trait HasCoreParameter {
  // General Parameter for WuKong
  val XLEN = if (Settings.get("IsRV32")) 32 else 64
  val HasMExtension = true
  val HasCExtension = Settings.get("EnableRVC")
  val HasDiv = true
  val HasIcache = Settings.get("HasIcache")
  val HasDcache = Settings.get("HasDcache")
  val HasITLB = Settings.get("HasITLB")
  val HasDTLB = Settings.get("HasDTLB")
  val AddrBits = 64 // AddrBits is used in some cases
  val VAddrBits = if (Settings.get("IsRV32")) 32 else 39 // VAddrBits is Virtual Memory addr bits
  val PAddrBits = 32 // PAddrBits is Phyical Memory addr bits
  val AddrBytes = AddrBits / 8 // unused
  val DataBits = XLEN
  val DataBytes = DataBits / 8
  val EnableVirtualMemory = if (Settings.get("HasDTLB") && Settings.get("HasITLB")) true else false
  val EnablePerfCnt = true
  // Parameter for Argo's OoO backend
  val EnableMultiIssue = Settings.get("EnableMultiIssue")
  val EnableOutOfOrderExec = Settings.get("EnableOutOfOrderExec")
  val EnableMultiCyclePredictor = false // false unless a customized condition branch predictor is included
  val EnableOutOfOrderMemAccess = false // enable out of order mem access will improve OoO backend's performance

  val FetchBytes = 8
  val FetchSize = 2 //without compress instr
}

trait HasCoreConst extends HasCoreParameter {
  val CacheReadWidth = 8
  val ICacheUserBundleWidth = VAddrBits*2 + 9  + 4
  val DCacheUserBundleWidth = 16
  val IndependentBru = if (Settings.get("EnableOutOfOrderExec")) true else false
}

trait HasCoreLog { this: RawModule =>
  implicit val moduleName: String = this.name
}

abstract class CoreModule extends Module with HasCoreParameter with HasCoreConst with HasExceptionNO with HasCoreLog
abstract class CoreBundle extends Bundle with HasCoreParameter with HasCoreConst

// Enable EnhancedLog will slow down simulation,
// but make it possible to control debug log using emu parameter

object AddressSpace extends HasCoreParameter {
  // (start, size)
  // address out of MMIO will be considered as DRAM
  def mmio = List(
    (0x00000000L, 0x40000000L),  // internal devices, such as CLINT and PLIC
    (Settings.getLong("MMIOBase"), Settings.getLong("MMIOSize")) // external devices
  )

  def isMMIO(addr: UInt) = mmio.map(range => {
//    require(isPow2(range._2))
    val bits = log2Up(range._2)
    (addr ^ range._1.U)(PAddrBits-1, bits) === 0.U
  }).reduce(_ || _)
}

class Core(implicit val p: WuKongConfig) extends CoreModule {
  class CoreIO extends Bundle {
    val imem = new SimpleBusC
    val dmem = new SimpleBusC
    val mmio = new SimpleBusUC
    val frontend = Flipped(new SimpleBusUC())
  }
  val io = IO(new CoreIO)

  // Frontend
  val frontend = Module(new Frontend)

  val Backend = Module(new Backend)
  Backend.io.in <> frontend.io.out
  frontend.io.redirect <> Backend.io.redirectOut
  frontend.io.ipf := false.B
  // add pipestage
//  PipelineVector2Connect(new DecodeIO, frontend.io.out(0), frontend.io.out(1), Backend.io.in(0), Backend.io.in(1), frontend.io.flushVec(1), 16)
//  PipelineVector2Connect(new DecodeIO, frontend.io.out(2), frontend.io.out(3), Backend.io.in(2), Backend.io.in(3), frontend.io.flushVec(1), 16)
  for(i <- 0 to 3){frontend.io.out(i) <> Backend.io.in(i)}
  val mmioXbar = Module(new SimpleBusCrossbarNto1(2))
  val s2NotReady = WireInit(false.B)
  io.imem <> ICache(in = frontend.io.imem, mmio = mmioXbar.io.in(0), flush = (frontend.io.flushVec(0) | frontend.io.bpFlush))(ICacheConfig(ro = true, userBits = ICacheUserBundleWidth))
  io.dmem <> DCache(in = Backend.io.dmem, mmio = mmioXbar.io.in(1), flush = false.B)(BankedCacheConfig(ro = true))

  // DMA?
  io.frontend.resp.bits := DontCare
  io.frontend.req.ready := false.B
  io.frontend.resp.valid := false.B

  io.mmio <> mmioXbar.io.out

}
