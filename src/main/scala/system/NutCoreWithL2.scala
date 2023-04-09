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

package system

import nutcore._
import bus.axi4.{AXI4, AXI4Lite}
import bus.simplebus._
import device.{AXI4CLINT, AXI4PLIC}
import top._

import huancun.debug.TLLogger
import huancun.{HCCacheParamsKey, HuanCun}
import huancun.utils.{ResetGen}
import freechips.rocketchip.amba.axi4._ 
import freechips.rocketchip.tilelink._ 
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp, MemoryDevice, AddressSet, InModuleBody, TransferSizes, RegionType, SimpleDevice}
import freechips.rocketchip.diplomacy._
import utils._
import huancun._
import chipsalliance.rocketchip.config._
//import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp}

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

class NutcoreWithL2()(implicit p: Parameters) extends LazyModule{
  
  val nutcore = LazyModule(new NutCore())

  val l2cache = LazyModule(new HuanCun()(new Config((_, _, _) => {
    case HCCacheParamsKey => HCCacheParameters(
      name = s"L2",
      level = 2,
      inclusive = false,
      alwaysReleaseData = true,
      clientCaches = Seq(
        CacheParameters(sets = 128, ways = 4, blockGranularity = 6, name = "icache"),
        CacheParameters(sets = 128, ways = 4, blockGranularity = 6, name = "dcache")
      ),
      //prefetch = Some(huancun.prefetch.BOPParameters()),
      reqField = Seq(),
      echoField = Seq(),
      enableDebug = true
    )
  })))

  val tlBus = TLXbar()
  tlBus := nutcore.dcache.clientNode
  tlBus := nutcore.icache.clientNode
  val memory_port = TLTempNode()
  memory_port := TLBuffer() := l2cache.node :=* tlBus

  //mmio_port: peripheralXbar
  val mmio_port = TLTempNode()
  mmio_port := nutcore.uncache.clientNode

  //val core_reset_sink = BundleBridgeSink(Some(() => Reset()))
  val core_reset_sink = BundleBridgeSink(Some(() => Bool()))

  lazy val module = new NutcoreWithL2Imp(this)
}

class NutcoreWithL2Imp(outer: NutcoreWithL2) extends LazyModuleImp(outer) with HasNutCoreParameters with HasSoCParameter with HasNutCoreParameter{
  val io = IO(new Bundle{
    //val frontend = Flipped(new SimpleBusUC())
    val hartId = Input(UInt(XLEN.W))
    val meip = Input(UInt(Settings.getInt("NrExtIntr").W))
    val ila = if (FPGAPlatform && EnableILA) Some(Output(new ILABundle)) else None
  })

  val nutcore = outer.nutcore.module
  val core_reset_sink = outer.core_reset_sink
  val core_soft_rst = outer.core_reset_sink.in.head._1

  nutcore.io.hartid := io.hartId
  
  //val resetChain = Seq(Seq(nutcore, outer.l2cache.module))
  val resetChain = Seq(Seq(nutcore))

  //ResetGen(resetChain, reset, !FPGAPlatform)
  /*withClockAndReset(clock, core_soft_rst) {
    ResetGen(resetChain, reset, !FPGAPlatform)
  }*/

  //val reset_sync = withClockAndReset(clock, reset) { ResetGen() }
  //val core_reset_sync = RegNext(RegNext(core_soft_rst.asAsyncReset))

  when (io.hartId > 0.U) {
    withClockAndReset(clock, reset) {
      nutcore.reset := core_soft_rst.asAsyncReset
      //ResetGen(resetChain, core_soft_rst.asAsyncReset, !FPGAPlatform)
    }
  }


  // ILA
  if (FPGAPlatform) {
    def BoringUtilsConnect(sink: UInt, id: String) {
      val temp = WireInit(0.U(64.W))
      BoringUtils.addSink(temp, id)
      sink := temp
    }

    val dummy = WireInit(0.U.asTypeOf(new ILABundle))
    val ila = io.ila.getOrElse(dummy)
//    BoringUtilsConnect(ila.WBUpc      ,"ilaWBUpc")
//    BoringUtilsConnect(ila.WBUvalid   ,"ilaWBUvalid")
//    BoringUtilsConnect(ila.WBUrfWen   ,"ilaWBUrfWen")
//    BoringUtilsConnect(ila.WBUrfDest  ,"ilaWBUrfDest")
//    BoringUtilsConnect(ila.WBUrfData  ,"ilaWBUrfData")
//    BoringUtilsConnect(ila.InstrCnt   ,"ilaInstrCnt")
  }
}
