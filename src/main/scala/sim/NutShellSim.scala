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

package sim

import XiaoHe.NutCoreConfig
import system._
import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

//import bus.axi4._
//import device.AXI4RAM
import XiaoHe._
import utils.GTimer
import difftest._
import freechips.rocketchip.amba.axi4.{AXI4Xbar, AXI4Delayer}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp}
import top.DefaultConfig
import chipsalliance.rocketchip.config.Parameters
import device.{AXI4MemorySlave, SimJTAG}
import top.Settings
import utils._

class SimTop(implicit p: Parameters) extends Module {
  val io = IO(new Bundle{
    val logCtrl = new LogCtrlIO
    val perfInfo = new PerfInfoIO
    val uart = new UARTIO
    //val test = Input(Bool())
  })
  val l_soc = LazyModule(new NutShell())
  val soc = Module(l_soc.module)
  //val core_with_l2 = Array.fill(Settings.getLong("CoreNums")){LazyModule(new NutShell())}
  
  // AXI4RAM
  val l_simAXIMem = AXI4MemorySlave(
    l_soc.memAXI4SlaveNode,
    128 * 1024 * 1024,
    useBlackBox = true,
    dynamicLatency = false
  )

  //Debug(reset.asBool, "SimTop reset\n")
  //Debug(clock.asBool, "SimTop clock\n")
  soc.io.clock := clock.asBool
  // include global reset and jtag debug reset
  soc.io.reset := (reset.asBool || soc.io.debug_reset).asAsyncReset  
  val debug_reset = soc.io.debug_reset
  val debug_resetReg = RegNext(debug_reset)   
  Debug(!debug_resetReg && debug_reset, "Debug reset on\n")
  Debug(debug_resetReg && !debug_reset, "Debug reset off\n")

  val l_mmio = LazyModule(new SimMMIO(l_soc.peripheralNode.in.head._2))
  val mmio = Module(l_mmio.module)
  l_mmio.io_axi4 <> soc.peripheral

  val simAXIMem = Module(l_simAXIMem.module)
  l_simAXIMem.io_axi4 <> soc.memory

  //mmio.io.rw <> soc.io.mmio
  mmio.io.rw <> DontCare

  soc.io.meip := mmio.io.meip

  val log_begin, log_end, log_level = WireInit(0.U(64.W))
  log_begin := io.logCtrl.log_begin
  log_end := io.logCtrl.log_end
  log_level := io.logCtrl.log_level

  assert(log_begin <= log_end)
  BoringUtils.addSource((GTimer() >= log_begin) && (GTimer() < log_end), "DISPLAY_ENABLE")

  // make BoringUtils not report boring exception when EnableDebug is set to false
  val dummyWire = WireInit(false.B)
  BoringUtils.addSink(dummyWire, "DISPLAY_ENABLE")

  io.uart <> mmio.io.uart

  //difftest
  val corenum = Settings.getInt("CoreNums")
  val dt_ld1 = Seq.fill(corenum)(Module(new DifftestLoadEvent))
  val dt_ld0 = Seq.fill(corenum)(Module(new DifftestLoadEvent))

  (dt_ld0 zip soc.io.diff) map { case (i,o) => i.io <> o.dt_ld0 }
  (dt_ld1 zip soc.io.diff) map { case (i,o) => i.io <> o.dt_ld1 }

  val dt_sb1 = Seq.fill(corenum)(Module(new DifftestSbufferEvent))
  val dt_sb0 = Seq.fill(corenum)(Module(new DifftestSbufferEvent))

  (dt_sb0 zip soc.io.diff) map { case (i,o) => i.io <> o.dt_sb0 }
  (dt_sb1 zip soc.io.diff) map { case (i,o) => i.io <> o.dt_sb1 }

  val dt_ic1 = Seq.fill(corenum)(Module(new DifftestInstrCommit))
  val dt_ic0 = Seq.fill(corenum)(Module(new DifftestInstrCommit))

  (dt_ic0 zip soc.io.diff) map { case (i,o) => i.io <> o.dt_ic0 }
  (dt_ic1 zip soc.io.diff) map { case (i,o) => i.io <> o.dt_ic1 }

  val dt_iw1 = Seq.fill(corenum)(Module(new DifftestIntWriteback))
  val dt_iw0 = Seq.fill(corenum)(Module(new DifftestIntWriteback))

  (dt_iw0 zip soc.io.diff) map { case (i,o) => i.io <> o.dt_iw0 }
  (dt_iw1 zip soc.io.diff) map { case (i,o) => i.io <> o.dt_iw1 }

  val dt_ae = Seq.fill(corenum)(Module(new DifftestArchEvent))
  (dt_ae zip soc.io.diff) map { case (i,o) => i.io <> o.dt_ae }

  val dt_te = Seq.fill(corenum)(Module(new DifftestTrapEvent))
  (dt_te zip soc.io.diff) map { case (i,o) => i.io <> o.dt_te }
  val cycle_cnt = RegInit(0.U(64.W))
  cycle_cnt := cycle_cnt + 1.U
  dt_te.zipWithIndex map { case (i,j) => i.io.cycleCnt <> cycle_cnt}

  val dt_cs = Seq.fill(corenum)(Module(new DifftestCSRState))
  (dt_cs zip soc.io.diff) map { case (i,o) => i.io <> o.dt_cs }

  val dt_irs = Seq.fill(corenum)(Module(new DifftestArchIntRegState))
  (dt_irs zip soc.io.diff) map { case (i,o) => i.io <> o.dt_irs }

  //uart
  val success = Wire(Bool())
  val jtag = Module(new SimJTAG(tickDelay=3)(p))
  jtag.connect(soc.io.systemjtag.jtag, clock, reset.asBool, !reset.asBool, success)
  soc.io.systemjtag.reset := reset.asAsyncReset
  soc.io.systemjtag.mfr_id := 0.U(11.W)
  soc.io.systemjtag.part_number := 0.U(16.W)
  soc.io.systemjtag.version := 0.U(4.W)
}
