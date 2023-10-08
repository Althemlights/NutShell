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

import XiaoHe._
import XiaoHe.mem.{Cache, CacheConfig}
import bus.axi4.{AXI4, AXI4Lite}
import bus.simplebus._
import device.{DebugModule, AXI4CLINT, AXI4PLIC}
import top._

import huancun.debug.TLLogger
import huancun.{HCCacheParamsKey, HuanCun}
import huancun.utils.{ResetGen}
import freechips.rocketchip.amba.axi4._ 
import freechips.rocketchip.tilelink._ 
import freechips.rocketchip.devices.tilelink.{CLINT, CLINTParams, DevNullParams, PLICParams, TLError, TLPLIC}
import freechips.rocketchip.interrupts.{IntSourceNode, IntSourcePortSimple}
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp, MemoryDevice, AddressSet, InModuleBody, TransferSizes, RegionType, SimpleDevice}
import freechips.rocketchip.diplomacy._
import utils._
import huancun._
import chipsalliance.rocketchip.config._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.jtag.JTAGIO
//import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp}

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

trait HasSoCParameter {
  val EnableILA = Settings.get("EnableILA")
  val HasL2cache = Settings.get("HasL2cache")
  val HasPrefetch = Settings.get("HasPrefetch")
}

class ILABundle extends NutCoreBundle {
  val WBUpc = UInt(VAddrBits.W)
  val WBUvalid = UInt(1.W)
  val WBUrfWen = UInt(1.W)
  val WBUrfDest = UInt(5.W)
  val WBUrfData = UInt(XLEN.W)
  val InstrCnt = UInt(64.W)
}

class NutShell()(implicit p: Parameters) extends LazyModule{
  val corenum = Settings.getInt("CoreNums")
  val extIntr = Settings.getInt("NrExtIntr")
  val core_with_l2 = Array.fill(corenum){LazyModule(new NutcoreWithL2())}

  //axi4ram slave node
  val device = new MemoryDevice
  val memRange = AddressSet(0x00000000L, 0xffffffffL).subtract(AddressSet(0x0L, 0x7fffffffL))
  val memAXI4SlaveNode = AXI4SlaveNode(Seq(
    AXI4SlavePortParameters(
      slaves = Seq(
        AXI4SlaveParameters(
          address = memRange,
          regionType = RegionType.UNCACHED,
          executable = true,
          supportsRead = TransferSizes(1, 64),
          //supportsRead = TransferSizes(1, 8),
          supportsWrite = TransferSizes(1, 64),
          //supportsWrite = TransferSizes(1, 8),
          interleavedId = Some(0),
          resources = device.reg("mem")
        )
      ),
      //beatBytes = 32
      beatBytes = 8
    )
  ))

  val peripheralXbar = TLXbar()
  
  // jtag Debug Module
  val debugModule = LazyModule(new DebugModule(corenum)(p))
  debugModule.debug.node :=* peripheralXbar        // debug mmio 0x38020000 +：10000

  val l2_mem_tlxbar = TLXbar()
  for (i <- 0 until corenum) {
    core_with_l2(i).debug_int_sink := debugModule.debug.dmOuter.dmOuter.intnode
    l2_mem_tlxbar := TLBuffer() := core_with_l2(i).memory_port
    peripheralXbar :=* core_with_l2(i).mmio_port
  }

  //memAXI4SlaveNode := AXI4UserYanker() := AXI4Deinterleaver(8) := AXI4Buffer():= TLToAXI4() := TLWidthWidget(32) := TLBuffer() := TLCacheCork() :=* l2_mem_tlxbar
  memAXI4SlaveNode := AXI4UserYanker() := AXI4Deinterleaver(8) := AXI4Buffer():= TLToAXI4() := TLWidthWidget(32) := TLBuffer() :=* l2_mem_tlxbar

  val onChipPeripheralRange = AddressSet(0x38000000L, 0x07ffffffL)
  val uartRange = AddressSet(0x40600000L, 0xf)
  val uartDevice = new SimpleDevice("serial", Seq("xilinx,uartlite"))
  val uartParams = AXI4SlaveParameters(
    address = Seq(uartRange),
    regionType = RegionType.UNCACHED,
    supportsRead = TransferSizes(1, 8),
    supportsWrite = TransferSizes(1, 8),
    resources = uartDevice.reg
  )
  val peripheralRange = AddressSet(
    0x0, 0x7fffffff
  ).subtract(onChipPeripheralRange).flatMap(x => x.subtract(uartRange))
  val peripheralNode = AXI4SlaveNode(Seq(AXI4SlavePortParameters(
    Seq(uartParams),
    beatBytes = 8
  )))

  peripheralNode :=
    AXI4IdIndexer(idBits = 4) :=
    //AXI4Buffer() :=
    //AXI4Buffer() :=
    //AXI4Buffer() :=
    AXI4Buffer() :=
    AXI4UserYanker() :=
    AXI4Deinterleaver(8) :=
    TLToAXI4() :=
    TLBuffer() :=*
    //TLBuffer.chainNode(3) :=
    peripheralXbar

  lazy val module = new NutShellImp(this)
}

class NutShellImp(outer: NutShell) extends LazyRawModuleImp(outer) with HasNutCoreParameters with HasSoCParameter{
  val corenum = Settings.getInt("CoreNums")
  
  val io = IO(new Bundle{
    val clock = Input(Bool())
    val reset = Input(AsyncReset())
    //val frontend = Flipped(new AXI4)
    val meip = Input(UInt(Settings.getInt("NrExtIntr").W))
    val ila = if (FPGAPlatform && EnableILA) Some(Output(new ILABundle)) else None
    val diff = Flipped(Vec(corenum, new DIFFTESTIO))
    val systemjtag = new Bundle {
      val jtag = Flipped(new JTAGIO(hasTRSTn = false))
      val reset = Input(AsyncReset()) // No reset allowed on top
      val mfr_id = Input(UInt(11.W))
      val part_number = Input(UInt(16.W))
      val version = Input(UInt(4.W))
    }
    val debug_reset = Output(Bool())   // jtag debug reset
  })

  val memory = outer.memAXI4SlaveNode.makeIOs()
  val peripheral = outer.peripheralNode.makeIOs()
  val nutcore_withl2 = outer.core_with_l2.map(_.module)
  (io.diff zip nutcore_withl2) map {case (i, o) => i <> o.io.diff} 

  for (i <- 0 until corenum) {
    outer.core_with_l2(i).module.io.hartId := i.U
  }

  // jtag debug module ndreset(reset for All except Debug Module)
  val debug_module_io = outer.debugModule.module.io
  val jtag_reset_sync = withClockAndReset(io.systemjtag.jtag.TCK, io.systemjtag.reset) { ResetGen() }

  io.debug_reset := debug_module_io.debugIO.ndreset
  debug_module_io.resetCtrl.hartIsInReset := outer.core_with_l2.map(_.module.reset.asBool)
  debug_module_io.clock := io.clock
  debug_module_io.reset := io.reset

  debug_module_io.debugIO.reset := io.reset
  debug_module_io.debugIO.clock := io.clock.asClock
  debug_module_io.debugIO.dmactiveAck := debug_module_io.debugIO.dmactive
  // jtag connector
  debug_module_io.debugIO.systemjtag.foreach { x =>
    x.jtag        <> io.systemjtag.jtag
    x.reset       := jtag_reset_sync
    x.mfr_id      := io.systemjtag.mfr_id
    x.part_number := io.systemjtag.part_number
    x.version     := io.systemjtag.version
  }

  val reset_sync = withClockAndReset(io.clock.asClock, io.reset) { ResetGen() }

  // override LazyRawModuleImp's clock and reset
  childClock := io.clock.asClock
  childReset := reset_sync

  withClockAndReset(io.clock.asClock, reset_sync) {
    // Modules are reset one by one
    // reset ----> SYNC --> {L3 Cache, Cores}
    val resetChain = Seq(Seq() ++ nutcore_withl2)
    ResetGen(resetChain, reset_sync, !FPGAPlatform)
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
