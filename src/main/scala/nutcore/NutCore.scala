/**************************************************************************************
 * Copyright (c) 2020 Institute of Computing Technology, CAS
 * Copyright (c) 2020 University of Chinese Academy of Sciences
 *
 * NutShell is licensed under Mulan PSL v2.
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

package nutcore

import SSDbackend._
import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import bus.simplebus._
import bus.axi4._
import utils._
import top._
import chipsalliance.rocketchip.config.Parameters
import system.HasNutCoreParameters

import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}

trait HasNutCoreParameter {
  // General Parameter for NutShell
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
  val GhrLength = 5
  val GhrMid    = GhrLength / 2
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
}

trait HasNutCoreConst extends HasNutCoreParameter {
  val CacheReadWidth = 8
  val ICacheUserBundleWidth = VAddrBits*2 + 9 + GhrLength + 4
  val DCacheUserBundleWidth = 16
  val IndependentBru = if (Settings.get("EnableOutOfOrderExec")) true else false
}

trait HasNutCoreLog { this: RawModule =>
  implicit val moduleName: String = this.name
}

abstract class NutCoreModule extends Module with HasNutCoreParameter with HasNutCoreConst with HasExceptionNO with HasBackendConst with HasNutCoreLog
abstract class NutCoreBundle extends Bundle with HasNutCoreParameter with HasNutCoreConst with HasBackendConst

case class NutCoreConfig (
                           FPGAPlatform: Boolean = true,
                           EnableDebug: Boolean = Settings.get("EnableDebug"),
                           EnhancedLog: Boolean = true
                         )
// Enable EnhancedLog will slow down simulation,
// but make it possible to control debug log using emu parameter

object AddressSpace extends HasNutCoreParameter {
  // (start, size)
  // address out of MMIO will be considered as DRAM
  def mmio = List(
    (0x00000000L, 0x40000000L),  // internal devices, such as CLINT and PLIC
    (Settings.getLong("MMIOBase"), Settings.getLong("MMIOSize")) // external devices
  )

  def isMMIO(addr: UInt) = mmio.map(range => {
    require(isPow2(range._2))
    val bits = log2Up(range._2)
    (addr ^ range._1.U)(PAddrBits-1, bits) === 0.U
  }).reduce(_ || _)
}

class NutCore()(implicit p: Parameters) extends LazyModule with HasNutCoreParameter with HasNutCoreConst with HasExceptionNO with HasBackendConst with HasNutCoreParameters{

  val dcache = LazyModule(new DCache())
  val icache = LazyModule(new ICache())
  //Debug() {printf("%d", FPGAPlatform)}
  lazy val module = new NutCoreImp(this)
}

class NutCoreImp(outer: NutCore) extends LazyModuleImp(outer) with HasNutCoreParameter with HasNutCoreConst with HasExceptionNO with HasBackendConst{
  
  val dcache = outer.dcache.module
  val icache = outer.icache.module

  class NutCoreIO extends Bundle {
    //val imem = new SimpleBusC
    //val dmem = new SimpleBusC
    val mmio = new SimpleBusUC
    val frontend = Flipped(new SimpleBusUC())
  }
  val io = IO(new NutCoreIO)

  // Frontend
  val frontend = (Settings.get("IsRV32"), Settings.get("EnableOutOfOrderExec")) match {
    case (true, _)      => Module(new Frontend_embedded())
    case (false, true)  => Module(new Frontend_ooo)
    case (false, false) => Module(new Frontend_inorder)
  }

  // Backend
  val BoolTmp0 = WireInit(false.B)
  val BoolTmp1 = WireInit(false.B)


  val SSDbackend = Module(new SSDbackend)
  SSDbackend.io.in <> frontend.io.out
  frontend.io.redirect <> SSDbackend.io.redirectOut
  frontend.io.ipf := false.B
  // add pipestage
//  PipelineVector2Connect(new DecodeIO, frontend.io.out(0), frontend.io.out(1), SSDbackend.io.in(0), SSDbackend.io.in(1), frontend.io.flushVec(1), 16)
//  PipelineVector2Connect(new DecodeIO, frontend.io.out(2), frontend.io.out(3), SSDbackend.io.in(2), SSDbackend.io.in(3), frontend.io.flushVec(1), 16)
  for(i <- 0 to 3){frontend.io.out(i) <> SSDbackend.io.in(i)}
  val mmioXbar = Module(new SimpleBusCrossbarNto1(1))
  mmioXbar.io.in(0) := DontCare
  val s2NotReady = WireInit(false.B)
  //io.imem <> SSDCache(in = frontend.io.imem, mmio = mmioXbar.io.in(0), flush = (frontend.io.flushVec(0) | frontend.io.bpFlush))(SSDCacheConfig(ro = true, name = "icache", userBits = ICacheUserBundleWidth))
  //io.dmem <> SSDCache(in = SSDbackend.io.dmem, mmio = mmioXbar.io.in(1), flush = false.B)(SSDCacheConfig(ro = true, name = "dcache"))
  
  icache.io.in <> frontend.io.imem
  icache.io.flush := frontend.io.flushVec(0) | frontend.io.bpFlush


  dcache.io.in <> SSDbackend.io.dmem
  //dcache.io.mmio <> mmioXbar.io.in(1)
  dcache.io.flush := false.B

  // DMA?
  io.frontend.resp.bits := DontCare
  io.frontend.req.ready := false.B
  io.frontend.resp.valid := false.B

  io.mmio <> mmioXbar.io.out

//  val mmioXbar = Module(new SimpleBusCrossbarNto1(2))
//  val s2NotReady = WireInit(false.B)
//  io.imem <> Cache(in = frontend.io.imem, mmio = mmioXbar.io.in.take(1), flush = Fill(2, frontend.io.flushVec(0) | frontend.io.bpFlush), empty = BoolTmp0, enable = HasIcache)(CacheConfig(ro = true, name = "icache", userBits = ICacheUserBundleWidth))
//  io.dmem <> Cache(in = SSDbackend.io.dmem, mmio = mmioXbar.io.in.drop(1), flush = "b00".U, empty = BoolTmp1, enable = HasDcache)(CacheConfig(ro = true, name = "dcache"))
//
//  // DMA?
//  io.frontend.resp.bits := DontCare
//  io.frontend.req.ready := false.B
//  io.frontend.resp.valid := false.B
//
//  io.mmio <> DontCare

}
