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

package XiaoHe

import XiaoHe.SSDbackend._
import XiaoHe.SSDbackend.backend._
import XiaoHe.SSDbackend.fu._
import XiaoHe.SSDfrontend.Frontend_ooo
import bus.simplebus._
import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import bus.simplebus._
import bus.axi4._
import utils._
import top._
import chipsalliance.rocketchip.config.Parameters
import system.HasNutCoreParameters
import device.TLTimer
import freechips.rocketchip.tilelink._ 

import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp, AddressSet}
import freechips.rocketchip.interrupts.{IntSinkNode, IntSinkPortSimple}

trait HasNutCoreParameter {
  // General Parameter for NutShell
  val XLEN = if (Settings.get("IsRV32")) 32 else 64
  val HasMExtension = true
  val HasFPU = true
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

trait HasNutCoreConst extends HasNutCoreParameter {
  val CacheReadWidth = 8
  val ICacheUserBundleWidth = VAddrBits*2 + 9  + 4
  val DCacheUserBundleWidth = 16
  val IndependentBru = if (Settings.get("EnableOutOfOrderExec")) true else false
}

trait HasNutCoreLog { this: RawModule =>
  implicit val moduleName: String = this.name
}

abstract class NutCoreModule extends Module with HasNutCoreParameter with HasNutCoreConst with SSDHasExceptionNO with HasNutCoreLog
abstract class NutCoreBundle extends Bundle with HasNutCoreParameter with HasNutCoreConst

case class NutCoreConfig (
                           FPGAPlatform: Boolean = false,
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
//    require(isPow2(range._2))
    val bits = log2Up(range._2)
    (addr ^ range._1.U)(PAddrBits-1, bits) === 0.U
  }).reduce(_ || _)
}

class NutCore()(implicit p: Parameters) extends LazyModule with HasNutCoreParameter with HasNutCoreConst with SSDHasExceptionNO  with HasNutCoreParameters{

  val dcache = LazyModule(new DCache())
  val icache = LazyModule(new ICache())
  val dUncache = LazyModule(new UnCache())
  val iUncache = LazyModule(new IUnCache())
  val clintSpace = Seq(AddressSet(Settings.getLong("CLINTBase"), Settings.getLong("CLINTSize") - 0x1L)) // CLINT
  val timer = LazyModule(new TLTimer(clintSpace, sim = true))
  val mmioxbar = TLXbar()
  mmioxbar := TLBuffer.chainNode(2) := TLTempNode() := dUncache.clientNode
  mmioxbar := TLBuffer.chainNode(2) := TLTempNode() := iUncache.clientNode
  timer.node := mmioxbar
  
  //val debug_int_sink = IntSinkNode(IntSinkPortSimple(1, 1))
  lazy val module = new NutCoreImp(this)
}

class NutCoreImp(outer: NutCore) extends LazyModuleImp(outer) with HasNutCoreParameter with HasNutCoreConst with SSDHasExceptionNO {
  
  val dcache = outer.dcache.module
  val icache = outer.icache.module
  val dUncache = outer.dUncache.module
  val iUncache = outer.iUncache.module
  val timer = outer.timer.module
  //val debugInt = outer.debug_int_sink.in.head._1(0)
  val debugInt = false.B 

  BoringUtils.addSource(timer.io.mtip, "mtip")
  BoringUtils.addSource(timer.io.msip, "msip")

  class NutCoreIO extends Bundle {
    //val imem = new SimpleBusC
    //val dmem = new SimpleBusC
    //val mmio = new SimpleBusUC
    val frontend = Flipped(new SimpleBusUC())
    val hartid = Input(UInt(XLEN.W))
    //val diff = Flipped(new DIFFTESTIO)
  }
  val io = IO(new NutCoreIO)

  // Frontend
  val frontend =  Module(new Frontend_ooo)

  // Backend
  val BoolTmp0 = WireInit(false.B)
  val BoolTmp1 = WireInit(false.B)

  val SSDbackend = Module(new SSDbackend)
  SSDbackend.io.in <> frontend.io.out
  SSDbackend.io.hartid <> io.hartid
  //io.diff <> SSDbackend.io.diff
  SSDbackend.io.debugInt := debugInt
  frontend.io.pipelineEmpty := SSDbackend.io.pipelineEmpty
  frontend.io.bpuUpdateReq := SSDbackend.io.bpuUpdateReq
  frontend.io.redirect <> SSDbackend.io.redirectOut
  frontend.io.ipf := false.B

  for(i <- 0 to 3){frontend.io.out(i) <> SSDbackend.io.in(i)}

  val s2NotReady = WireInit(false.B)
  
  val addrSpace = List(
    (Settings.getLong("MemBase"), Settings.getLong("MemSize")),          //cache
    (Settings.getLong("UnCacheBase"), Settings.getLong("UnCacheSize"))    //uncache
  )
  //icache.io.in <> frontend.io.imem
  val imemxbar = Module(new ImemSimpleBusCrossbar1toN(addrSpace))
  imemxbar.io.in <> frontend.io.imem
  icache.io.in <> imemxbar.io.out(0)
  iUncache.io.in <> imemxbar.io.out(1)
  icache.io.flush := frontend.io.flushVec(0) | frontend.io.bpFlush

  val dmemxbar = Module(new DmemSimpleBusCrossbar1toN(addrSpace))
  dmemxbar.io.in <> SSDbackend.io.dmem
  dmemxbar.io.mxbarflush := SSDbackend.io.mxbarflush

  dcache.io.in <> dmemxbar.io.out(0)
  dcache.io.flush := false.B
  dUncache.io.in <> dmemxbar.io.out(1)
  dUncache.mmioflush := SSDbackend.io.mmioflush

  // DMA?
  io.frontend.resp.bits := DontCare
  io.frontend.req.ready := false.B
  io.frontend.resp.valid := false.B

  // TODO: Debug Interrupt into CSR
  // := debugInt

}
