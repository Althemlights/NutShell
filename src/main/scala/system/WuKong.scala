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

package system

import WuKong._
import bus.axi4.{AXI4, AXI4Lite}
import bus.simplebus._
import device.{AXI4CLINT, AXI4PLIC}
import top.Settings
import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import WuKong.{CoreBundle}
import top.WuKongConfig

trait HasSoCParameter {
  val EnableILA = Settings.get("EnableILA")
  val HasL2cache = Settings.get("HasL2cache")
  val HasPrefetch = Settings.get("HasPrefetch")
}

class ILABundle extends CoreBundle {
  val WBUpc = UInt(VAddrBits.W)
  val WBUvalid = UInt(1.W)
  val WBUrfWen = UInt(1.W)
  val WBUrfDest = UInt(5.W)
  val WBUrfData = UInt(XLEN.W)
  val InstrCnt = UInt(64.W)
}

class WuKong(implicit val p: WuKongConfig) extends Module with HasSoCParameter {
  val io = IO(new Bundle{
    val master = new AXI4
    val mmio = if (Settings.get("SoCTest")) null else { if (p.FPGAPlatform) { new AXI4 } else { new SimpleBusUC } }
    val slave = Flipped(new AXI4)
    val interrupt = Input(UInt(Settings.getInt("NrExtIntr").W))
    val ila = if (p.FPGAPlatform && EnableILA) Some(Output(new ILABundle)) else None
  })

  val core = Module(new Core)
//  val cohMg = Module(new CoherenceManager)
  val xbar = Module(new SimpleBusCrossbarNto1(2))
//  cohMg.io.in <> core.io.imem.mem
//  core.io.dmem.coh <> cohMg.io.out.coh
  xbar.io.in(0) <> core.io.imem.mem //cohMg.io.out.mem
  xbar.io.in(1) <> core.io.dmem.mem

  val axi2sb = Module(new AXI42SimpleBusConverter())
  axi2sb.io.in <> io.slave
  core.io.frontend <> axi2sb.io.out

  val memport = xbar.io.out.toMemPort()
  memport.resp.bits.data := DontCare
  memport.resp.valid := DontCare
  memport.req.ready := DontCare

  val mem = xbar.io.out


  val memMapRegionBits = Settings.getInt("MemMapRegionBits")
  val memMapBase = Settings.getLong("MemMapBase")
  val memAddrMap = Module(new SimpleBusAddressMapper((memMapRegionBits, memMapBase)))
  memAddrMap.io.in <> mem
//  io.mem <> memAddrMap.io.out.toAXI4(true)
  
  core.io.imem.coh.resp.ready := true.B
  core.io.imem.coh.req.valid := false.B
  core.io.imem.coh.req.bits := DontCare
  core.io.dmem.coh.resp.ready := true.B
  core.io.dmem.coh.req.valid := false.B
  core.io.dmem.coh.req.bits := DontCare

  val addrSpace = List(
    (Settings.getLong("MMIOBase"), Settings.getLong("MMIOSize")), // external devices
    (Settings.getLong("CLINTBase"), Settings.getLong("CLINTSize")), // CLINT
    // (Settings.getLong("PLICBase"), 0x04000000L)  // PLIC
    (Settings.getLong("PLICBase"), 0x00000000L)  // PLIC
  )
  val mmioXbar = Module(new SimpleBusCrossbar1toN(addrSpace))
  mmioXbar.io.in <> core.io.mmio

  val extDev = mmioXbar.io.out(0)
//  if (p.FPGAPlatform) { io.mmio <> extDev.toAXI4() }
//  else { io.mmio <> extDev }
  if (!Settings.get("SoCTest")) {
    if (p.FPGAPlatform) {
      io.mmio <> extDev.toAXI4()
    }
    else {
      io.mmio <> extDev
    }
    io.master <> memAddrMap.io.out.toAXI4(true)
  } else {
    val outputXbar = Module(new SimpleBusCrossbarNto1(2))
    outputXbar.io.in(0) <> memAddrMap.io.out
    outputXbar.io.in(1) <> extDev
    io.master <> outputXbar.io.out.toAXI4(true)
  }

  val clint = Module(new AXI4CLINT(sim = !p.FPGAPlatform))
  clint.io.in <> mmioXbar.io.out(1).toAXI4Lite()
  val mtipSync = clint.io.extra.get.mtip
  val msipSync = clint.io.extra.get.msip
  BoringUtils.addSource(mtipSync, "mtip")
  BoringUtils.addSource(msipSync, "msip")

  val plic = Module(new AXI4PLIC(nrIntr = Settings.getInt("NrExtIntr"), nrHart = 1))
  plic.io.in <> mmioXbar.io.out(2).toAXI4Lite()
  plic.io.extra.get.intrVec := RegNext(RegNext(io.interrupt))
  val meipSync = plic.io.extra.get.meip(0)
  BoringUtils.addSource(meipSync, "meip")
  

  // ILA
  if (p.FPGAPlatform) {
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
