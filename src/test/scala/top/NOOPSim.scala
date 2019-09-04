package top

import system._
import noop.NOOPConfig

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import bus.axi4._
import device.AXI4RAM
import utils.DiffTestIO

class NOOPSimTop extends Module {
  val io = IO(new Bundle{
    val difftest = new DiffTestIO
  })

  val soc = Module(new NOOPSoC()(NOOPConfig(FPGAPlatform = false)))
  val mem = Module(new AXI4RAM(memByte = 128 * 1024 * 1024, useBlackBox = true))
  // Be careful with the commit checking of emu.
  // A large delay will make emu incorrectly report getting stuck.
  val memdelay = Module(new AXI4Delayer(0))
  val mmio = Module(new SimMMIO)

  memdelay.io.in <> soc.io.mem
  mem.io.in <> memdelay.io.out

  mmio.io.rw <> soc.io.mmio

  val difftest = WireInit(0.U.asTypeOf(new DiffTestIO))
  BoringUtils.addSink(difftest.commit, "difftestCommit")
  BoringUtils.addSink(difftest.thisPC, "difftestThisPC")
  BoringUtils.addSink(difftest.isMMIO, "difftestIsMMIO")
  BoringUtils.addSink(difftest.r, "difftestRegs")
  io.difftest := difftest
}

object TestMain extends App {
  chisel3.Driver.execute(args, () => new NOOPSimTop)
}
