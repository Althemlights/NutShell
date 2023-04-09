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
import freechips.rocketchip.devices.tilelink.{CLINT, CLINTParams, DevNullParams, PLICParams, TLError, TLPLIC}
import freechips.rocketchip.interrupts.{IntSourceNode, IntSourcePortSimple}
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp, MemoryDevice, AddressSet, InModuleBody, TransferSizes, RegionType, SimpleDevice}
import freechips.rocketchip.diplomacy._
import utils._
import huancun._
import chipsalliance.rocketchip.config._
import freechips.rocketchip.interrupts._
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
  //val nutcore = LazyModule(new NutCore())
  val corenum = Settings.getInt("CoreNums")
  val extIntr = Settings.getInt("NrExtIntr")
  val core_with_l2 = Array.fill(corenum){LazyModule(new NutcoreWithL2())}
  //val imem = LazyModule(new SB2AXI4MasterNode(true))
  //val dmemory_port = TLIdentityNode()
  //dmemory_port := l2cache.node := nutcore.dcache.clientNode

  //axi4ram slave node
  val device = new MemoryDevice
  val memRange = AddressSet(0x00000000L, 0xfffffffffL).subtract(AddressSet(0x0L, 0x7fffffffL))
  val memAXI4SlaveNode = AXI4SlaveNode(Seq(
    AXI4SlavePortParameters(
      slaves = Seq(
        AXI4SlaveParameters(
          address = memRange,
          regionType = RegionType.UNCACHED,
          executable = true,
          supportsRead = TransferSizes(1, 64),
          supportsWrite = TransferSizes(1, 64),
          interleavedId = Some(0),
          resources = device.reg("mem")
        )
      ),
      beatBytes = 32
    )
  ))

  val l2_mem_tlxbar = TLXbar()
  val peripheralXbar = TLXbar()
  for (i <- 0 until corenum) {
    l2_mem_tlxbar := core_with_l2(i).memory_port
    peripheralXbar := core_with_l2(i).mmio_port
  }

  //l3 cache
  val l3cacheOpt = LazyModule(new HuanCun()(new Config((_, _, _) => {
    case HCCacheParamsKey => HCCacheParameters(
      name = s"L3",
      level = 3,
      inclusive = false,
      clientCaches = Seq(CacheParameters(sets = 128, ways = 4, blockGranularity = 7, name = "L2")),
      ctrl = Some(CacheCtrl(
        address = 0x39000000,
        numCores = corenum
      )),
      //prefetch = Some(huancun.prefetch.BOPParameters()),
      reqField = Seq(),
      echoField = Seq(),
      enableDebug = true
    )
  })))

  l3cacheOpt.ctlnode.map(_ := peripheralXbar)

  //val fake_plic = LazyModule(new FakeTLPLIC())
  //l3cacheOpt.intnode.map(int => {fake_plic.intnode := int})
  l3cacheOpt.intnode.map(int => {IntSinkNode(IntSinkPortSimple()) := int})

  val core_rst_nodes = l3cacheOpt.rst_nodes.get
  /*(core_rst_nodes zip core_with_l2) foreach{
    case (source, sink) => sink.core_reset_sink := source
  }*/
  core_rst_nodes.zip(core_with_l2.map(_.core_reset_sink)).foreach({
    case (source, sink) =>  sink := source
  })

  memAXI4SlaveNode := AXI4UserYanker() := AXI4Deinterleaver(64) := TLToAXI4() := TLCacheCork() := l3cacheOpt.node :=* l2_mem_tlxbar

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
    //AXI4Buffer() :=
    AXI4UserYanker() :=
    AXI4Deinterleaver(8) :=
    TLToAXI4() :=
    //TLBuffer.chainNode(3) :=
    peripheralXbar

  lazy val module = new NutShellImp(this)
}

class NutShellImp(outer: NutShell) extends LazyRawModuleImp(outer) with HasNutCoreParameters with HasSoCParameter{
  val io = IO(new Bundle{
    val clock = Input(Bool())
    val reset = Input(AsyncReset())
    //val frontend = Flipped(new AXI4)
    val meip = Input(UInt(Settings.getInt("NrExtIntr").W))
    val ila = if (FPGAPlatform && EnableILA) Some(Output(new ILABundle)) else None
  })

  //val memory = IO(outer.memory.cloneType)
  val memory = outer.memAXI4SlaveNode.makeIOs()
  val peripheral = outer.peripheralNode.makeIOs()
  //val nutcore = outer.nutcore.module
  val nutcore_withl2 = outer.core_with_l2.map(_.module)
  //val imem = outer.imem.module
  val core_rst_nodes = outer.core_rst_nodes

  //val axi2sb = Module(new AXI42SimpleBusConverter())
  //axi2sb.io.in <> io.frontend
  //io.frontend <> DontCare
  //nutcore.io.frontend <> axi2sb.io.out
  
  val corenum = Settings.getInt("CoreNums")
  /*for (i <- 0 until corenum) {
    //nutcore_withl2(i).io.frontend <> axi2sb.io.out
    nutcore_withl2(i).io.frontend <> DontCare
  }*/

  val l3cacheOpt = outer.l3cacheOpt.module
  /*if(l3cacheOpt.rst_nodes.isEmpty){
    // tie off core soft reset
    for(node <- core_rst_nodes){
      node.out.head._1 := false.B.asAsyncReset()
    }
  }*/

  for (i <- 0 until corenum) {
    outer.core_with_l2(i).module.io.hartId := i.U
  }

  val reset_sync = withClockAndReset(io.clock.asClock, io.reset) { ResetGen() }

  // override LazyRawModuleImp's clock and reset
  childClock := io.clock.asClock
  childReset := reset_sync

  withClockAndReset(io.clock.asClock, reset_sync) {
    // Modules are reset one by one
    // reset ----> SYNC --> {L3 Cache, Cores}
    val resetChain = Seq(Seq(l3cacheOpt) ++ nutcore_withl2)
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
