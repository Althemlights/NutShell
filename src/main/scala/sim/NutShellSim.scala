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
import bus.axi4._
import device.AXI4RAM
import _root_.utils.GTimer
//import difftest._
import top.Settings
import XiaoHe.HasNutCoreParameter
class SimTop extends Module {
  val io = IO(new Bundle {
    //    val logCtrl = new LogCtrlIO
    //    val perfInfo = new PerfInfoIO
    //    val uart = new UARTIO
  })

  lazy val config = NutCoreConfig(FPGAPlatform = false)
  val nutshell = Module(new ysyx_229999()(config))

  if (Settings.get("SoCTest")) {

    nutshell.io := DontCare
    //    io.difftestCtrl.enable := false.B

    /*
    val ysyxSoC = Module(new ysyxSoCFull())
    ysyxSoC.io.clock := clock
    ysyxSoC.io.reset := reset
    nutshell.reset := ysyxSoC.io.cpu_reset
    nutshell.io.interrupt := ysyxSoC.io.cpu_intr

    ysyxSoC.io.cpu_master_0_awready <> nutshell.io.master.aw.ready
    ysyxSoC.io.cpu_master_0_awvalid <> nutshell.io.master.aw.valid
    ysyxSoC.io.cpu_master_0_awid <> nutshell.io.master.aw.bits.id
    ysyxSoC.io.cpu_master_0_awaddr <> nutshell.io.master.aw.bits.addr
    ysyxSoC.io.cpu_master_0_awlen <> nutshell.io.master.aw.bits.len
    ysyxSoC.io.cpu_master_0_awsize <> nutshell.io.master.aw.bits.size
    ysyxSoC.io.cpu_master_0_awburst <> nutshell.io.master.aw.bits.burst
    ysyxSoC.io.cpu_master_0_wready <> nutshell.io.master.w.ready
    ysyxSoC.io.cpu_master_0_wvalid <> nutshell.io.master.w.valid
    ysyxSoC.io.cpu_master_0_wdata <> nutshell.io.master.w.bits.data
    ysyxSoC.io.cpu_master_0_wstrb <> nutshell.io.master.w.bits.strb
    ysyxSoC.io.cpu_master_0_wlast <> nutshell.io.master.w.bits.last
    ysyxSoC.io.cpu_master_0_bready <> nutshell.io.master.b.ready
    ysyxSoC.io.cpu_master_0_bvalid <> nutshell.io.master.b.valid
    ysyxSoC.io.cpu_master_0_bid <> nutshell.io.master.b.bits.id
    ysyxSoC.io.cpu_master_0_bresp <> nutshell.io.master.b.bits.resp
    ysyxSoC.io.cpu_master_0_arready <> nutshell.io.master.ar.ready
    ysyxSoC.io.cpu_master_0_arvalid <> nutshell.io.master.ar.valid
    ysyxSoC.io.cpu_master_0_arid <> nutshell.io.master.ar.bits.id
    ysyxSoC.io.cpu_master_0_araddr <> nutshell.io.master.ar.bits.addr
    ysyxSoC.io.cpu_master_0_arlen <> nutshell.io.master.ar.bits.len
    ysyxSoC.io.cpu_master_0_arsize <> nutshell.io.master.ar.bits.size
    ysyxSoC.io.cpu_master_0_arburst <> nutshell.io.master.ar.bits.burst
    ysyxSoC.io.cpu_master_0_rready <> nutshell.io.master.r.ready
    ysyxSoC.io.cpu_master_0_rvalid <> nutshell.io.master.r.valid
    ysyxSoC.io.cpu_master_0_rid <> nutshell.io.master.r.bits.id
    ysyxSoC.io.cpu_master_0_rdata <> nutshell.io.master.r.bits.data
    ysyxSoC.io.cpu_master_0_rresp <> nutshell.io.master.r.bits.resp
    ysyxSoC.io.cpu_master_0_rlast <> nutshell.io.master.r.bits.last

    // TODO: leave cpu_slave hanging
    ysyxSoC.io.uart_rx := 1.U
    */

//    dontTouch(nutshell.io)

    //  } else {
    //    val mem = Module(new AXI4RAM(memByte = 128 * 1024 * 1024, useBlackBox = true))
    //    // Be careful with the commit checking of emu.
    //    // A large delay will make emu incorrectly report getting stuck.
    //    val memdelay = Module(new AXI4Delayer(0))
    //    val mmio = Module(new SimMMIO)
    //
    //    nutshell.io.slave <> mmio.io.dma
    //
    //    memdelay.io.in <> nutshell.io.master
    //    mem.io.in <> memdelay.io.out
    //
    //    mmio.io.rw <> nutshell.io.mmio
    //
    //    nutshell.io.interrupt := mmio.io.meip
    //
    //    val log_begin, log_end, log_level = WireInit(0.U(64.W))
    //    log_begin := io.logCtrl.log_begin
    //    log_end := io.logCtrl.log_end
    //    log_level := io.logCtrl.log_level
    //
    //    assert(log_begin <= log_end)
    //    //BoringUtils.addSource((GTimer() >= log_begin) && (GTimer() < log_end), "DISPLAY_ENABLE")
    //
    //    // make BoringUtils not report boring exception when EnableDebug is set to false
    //    val dummyWire = WireInit(false.B)
    //    BoringUtils.addSink(dummyWire, "DISPLAY_ENABLE")
    //
    //    //  io.uart <> mmio.io.uart
    //  }
  }
}
