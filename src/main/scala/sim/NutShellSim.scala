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
import difftest._
import top.Settings
import XiaoHe.HasNutCoreParameter
class SimTop extends Module {
  val io = IO(new Bundle {
        val logCtrl = new LogCtrlIO
        val perfInfo = new PerfInfoIO
        val uart = new UARTIO
  })

  lazy val config = NutCoreConfig(FPGAPlatform = false)
  val xiaohe = Module(new XiaoHe()(config))
  val mem = Module(new AXI4RAM(memByte = 128 * 1024 * 1024, useBlackBox = true))
  val memdelay = Module(new AXI4Delayer(0))
  val mmio = Module(new SimMMIO)
  xiaohe.io.slave <> mmio.io.dma

  memdelay.io.in <> xiaohe.io.master
  mem.io.in <> memdelay.io.out

  mmio.io.rw <> xiaohe.io.mmio

  xiaohe.io.interrupt := mmio.io.meip

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

  //for difftest
  val dt_ic1 = Module(new DifftestInstrCommit)
  val dt_ic0 = Module(new DifftestInstrCommit)

  val dt_ic1_valid  = WireInit(false.B)
  val dt_ic1_pc     = WireInit(0.U(64.W))
  val dt_ic1_instr  = WireInit(0.U(32.W))
  val dt_ic1_isRVC  = WireInit(false.B)
  val dt_ic1_skip   = WireInit(false.B)
  val dt_ic1_wen    = WireInit(false.B)
  val dt_ic1_wpdest = WireInit(0.U(8.W))
  val dt_ic1_wdest  = WireInit(0.U(8.W))

  val dt_ic0_valid  = WireInit(false.B)
  val dt_ic0_pc     = WireInit(0.U(64.W))
  val dt_ic0_instr  = WireInit(0.U(32.W))
  val dt_ic0_isRVC  = WireInit(false.B)
  val dt_ic0_skip   = WireInit(false.B)
  val dt_ic0_wen    = WireInit(false.B)
  val dt_ic0_wpdest = WireInit(0.U(8.W))
  val dt_ic0_wdest  = WireInit(0.U(8.W))

  BoringUtils.addSink(dt_ic1_valid, "dt_ic1_valid")
  BoringUtils.addSink(dt_ic1_pc   , "dt_ic1_pc")
  BoringUtils.addSink(dt_ic1_instr, "dt_ic1_instr")
  BoringUtils.addSink(dt_ic1_isRVC, "dt_ic1_isRVC")
  BoringUtils.addSink(dt_ic1_skip  , "dt_ic1_skip")
  BoringUtils.addSink(dt_ic1_wen   , "dt_ic1_wen")
  BoringUtils.addSink(dt_ic1_wpdest, "dt_ic1_wpdest")
  BoringUtils.addSink(dt_ic1_wdest , "dt_ic1_wdest")
  BoringUtils.addSink(dt_ic0_valid, "dt_ic0_valid")
  BoringUtils.addSink(dt_ic0_pc   , "dt_ic0_pc")
  BoringUtils.addSink(dt_ic0_instr, "dt_ic0_instr")
  BoringUtils.addSink(dt_ic0_isRVC, "dt_ic0_isRVC")
  BoringUtils.addSink(dt_ic0_skip, "dt_ic0_skip")
  BoringUtils.addSink(dt_ic0_wen, "dt_ic0_wen")
  BoringUtils.addSink(dt_ic0_wpdest, "dt_ic0_wpdest")
  BoringUtils.addSink(dt_ic0_wdest, "dt_ic0_wdest")

  dt_ic1.io.clock   := clock
  dt_ic1.io.coreid  := 0.U
  dt_ic1.io.index   := 0.U
  dt_ic1.io.valid   := dt_ic1_valid
  dt_ic1.io.pc      := dt_ic1_pc
  dt_ic1.io.instr   := dt_ic1_instr
  dt_ic1.io.special := 0.U
  dt_ic1.io.skip    := dt_ic1_skip
  dt_ic1.io.isRVC   := dt_ic1_isRVC
  dt_ic1.io.scFailed:= false.B
  dt_ic1.io.wen     := dt_ic1_wen
  dt_ic1.io.wpdest  := dt_ic1_wpdest
  dt_ic1.io.wdest   := dt_ic1_wdest

  dt_ic0.io.clock   := clock
  dt_ic0.io.coreid  := 0.U
  dt_ic0.io.index   := 1.U
  dt_ic0.io.valid   := dt_ic0_valid
  dt_ic0.io.pc      := dt_ic0_pc
  dt_ic0.io.instr   := dt_ic0_instr
  dt_ic0.io.special := 0.U
  dt_ic0.io.skip    := dt_ic0_skip
  dt_ic0.io.isRVC   := dt_ic0_isRVC
  dt_ic0.io.scFailed := false.B
  dt_ic0.io.wen     := dt_ic0_wen
  dt_ic0.io.wpdest  := dt_ic0_wpdest
  dt_ic0.io.wdest   := dt_ic0_wdest

  val dt_iw1 = Module(new DifftestIntWriteback)
  val dt_iw0 = Module(new DifftestIntWriteback)

  val dt_iw0_valid   = WireInit(false.B)
  val dt_iw0_dest    = WireInit(0.U(5.W))
  val dt_iw0_data    = WireInit(0.U(64.W))

  val dt_iw1_valid   = WireInit(false.B)
  val dt_iw1_dest    = WireInit(0.U(5.W))
  val dt_iw1_data    = WireInit(0.U(64.W))

  BoringUtils.addSink(dt_iw0_valid, "dt_iw0_valid")
  BoringUtils.addSink(dt_iw0_dest , "dt_iw0_dest")
  BoringUtils.addSink(dt_iw0_data , "dt_iw0_data")
  BoringUtils.addSink(dt_iw1_valid, "dt_iw1_valid")
  BoringUtils.addSink(dt_iw1_dest , "dt_iw1_dest")
  BoringUtils.addSink(dt_iw1_data , "dt_iw1_data")

  dt_iw0.io.clock    := clock
  dt_iw0.io.coreid   := 0.U
  // dt_iw0.io.index    := 1.U
  dt_iw0.io.valid    := dt_iw0_valid
  dt_iw0.io.dest     := dt_iw0_dest
  dt_iw0.io.data     := dt_iw0_data

  dt_iw1.io.clock  := clock
  dt_iw1.io.coreid := 0.U
  // dt_iw1.io.index  := 0.U
  dt_iw1.io.valid  := dt_iw1_valid
  dt_iw1.io.dest   := dt_iw1_dest
  dt_iw1.io.data   := dt_iw1_data

  val dt_ae = Module(new DifftestArchEvent)

  val dt_ae_intrNO        = WireInit(0.U(32.W))
  val dt_ae_cause         = WireInit(0.U(32.W))
  val dt_ae_exceptionPC   = WireInit(0.U(64.W))
  val dt_ae_exceptionInst = WireInit(0.U(32.W))

  BoringUtils.addSink(dt_ae_intrNO       , "dt_ae_intrNO")
  BoringUtils.addSink(dt_ae_cause        , "dt_ae_cause")
  BoringUtils.addSink(dt_ae_exceptionPC  , "dt_ae_exceptionPC")
  BoringUtils.addSink(dt_ae_exceptionInst, "dt_ae_exceptionInst")
  dt_ae.io.clock   := clock
  dt_ae.io.coreid  := 0.U
  dt_ae.io.intrNO  := dt_ae_intrNO
  dt_ae.io.cause   := dt_ae_cause
  dt_ae.io.exceptionPC := dt_ae_exceptionPC
  dt_ae.io.exceptionInst := dt_ae_exceptionInst

  val dt_te = Module(new DifftestTrapEvent)

  val dt_te_valid    = WireInit(0.U(32.W))
  val dt_te_code     = WireInit(0.U(32.W))
  val dt_te_pc       = WireInit(0.U(64.W))
  val dt_te_cycleCnt = WireInit(0.U(32.W))
  val dt_te_instrCnt = WireInit(0.U(32.W))

  BoringUtils.addSink(dt_te_valid   , "dt_te_valid")
  BoringUtils.addSink(dt_te_code    , "dt_te_code")
  BoringUtils.addSink(dt_te_pc      , "dt_te_pc")
  BoringUtils.addSink(dt_te_cycleCnt, "dt_te_cycleCnt")
  BoringUtils.addSink(dt_te_instrCnt, "dt_te_instrCnt")

  dt_te.io.clock      := clock
  dt_te.io.coreid     := 0.U
  dt_te.io.valid      := dt_te_valid
  dt_te.io.code       := dt_te_code
  dt_te.io.pc         := dt_te_pc
  dt_te.io.cycleCnt   := dt_te_cycleCnt
  dt_te.io.instrCnt   := dt_te_instrCnt

  val dt_cs = Module(new DifftestCSRState)

  val dt_cs_mstatus  = WireInit(0.U(64.W))
  val dt_cs_sstatus  = WireInit(0.U(64.W))
  val dt_cs_mepc     = WireInit(0.U(64.W))
  val dt_cs_sepc     = WireInit(0.U(64.W))
  val dt_cs_mtval    = WireInit(0.U(64.W))
  val dt_cs_stval    = WireInit(0.U(64.W))
  val dt_cs_mtvec    = WireInit(0.U(64.W))
  val dt_cs_stvec    = WireInit(0.U(64.W))
  val dt_cs_mcause   = WireInit(0.U(64.W))
  val dt_cs_scause   = WireInit(0.U(64.W))
  val dt_cs_satp     = WireInit(0.U(64.W))
  val dt_cs_mip      = WireInit(0.U(64.W))
  val dt_cs_mie      = WireInit(0.U(64.W))
  val dt_cs_mscratch = WireInit(0.U(64.W))
  val dt_cs_sscratch = WireInit(0.U(64.W))
  val dt_cs_mideleg  = WireInit(0.U(64.W))
  val dt_cs_medeleg  = WireInit(0.U(64.W))

  BoringUtils.addSink(dt_cs_mstatus ,"dt_cs_mstatus")
  BoringUtils.addSink(dt_cs_sstatus ,"dt_cs_sstatus")
  BoringUtils.addSink(dt_cs_mepc    ,"dt_cs_mepc")
  BoringUtils.addSink(dt_cs_sepc    ,"dt_cs_sepc")
  BoringUtils.addSink(dt_cs_mtval   ,"dt_cs_mtval")
  BoringUtils.addSink(dt_cs_stval   ,"dt_cs_stval")
  BoringUtils.addSink(dt_cs_mtvec   ,"dt_cs_mtvec")
  BoringUtils.addSink(dt_cs_stvec   ,"dt_cs_stvec")
  BoringUtils.addSink(dt_cs_mcause  ,"dt_cs_mcause")
  BoringUtils.addSink(dt_cs_scause  ,"dt_cs_scause")
  BoringUtils.addSink(dt_cs_satp    ,"dt_cs_satp")
  BoringUtils.addSink(dt_cs_mip     ,"dt_cs_mip")
  BoringUtils.addSink(dt_cs_mie     ,"dt_cs_mie")
  BoringUtils.addSink(dt_cs_mscratch,"dt_cs_mscratch")
  BoringUtils.addSink(dt_cs_sscratch,"dt_cs_sscratch")
  BoringUtils.addSink(dt_cs_mideleg ,"dt_cs_mideleg")
  BoringUtils.addSink(dt_cs_medeleg ,"dt_cs_medeleg")

  dt_cs.io.clock := clock
  dt_cs.io.coreid := 0.U
  dt_cs.io.priviledgeMode := 3.U
  dt_cs.io.mstatus :=dt_cs_mstatus
  dt_cs.io.sstatus :=dt_cs_sstatus
  dt_cs.io.mepc :=dt_cs_mepc
  dt_cs.io.sepc :=dt_cs_sepc
  dt_cs.io.mtval :=dt_cs_mtval
  dt_cs.io.stval :=dt_cs_stval
  dt_cs.io.mtvec :=dt_cs_mtvec
  dt_cs.io.stvec :=dt_cs_stvec
  dt_cs.io.mcause :=dt_cs_mcause
  dt_cs.io.scause :=dt_cs_scause
  dt_cs.io.satp :=dt_cs_satp
  dt_cs.io.mip :=dt_cs_mip
  dt_cs.io.mie :=dt_cs_mie
  dt_cs.io.mscratch :=dt_cs_mscratch
  dt_cs.io.sscratch :=dt_cs_sscratch
  dt_cs.io.mideleg := dt_cs_mideleg
  dt_cs.io.medeleg :=dt_cs_medeleg

  val dt_irs = Module(new DifftestArchIntRegState)

  val dt_irs_gpr = WireInit(VecInit(Seq.fill(32)(0.U(64.W))))

  BoringUtils.addSink(dt_irs_gpr, "dt_irs_gpr")
  dt_irs.io.clock:= clock
  dt_irs.io.coreid := 0.U
  dt_irs.io.gpr := dt_irs_gpr

}
