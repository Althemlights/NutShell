/** *************************************************************************************
  * Copyright (c) 2020 Institute of Computing Technology, CAS
  * Copyright (c) 2020 University of Chinese Academy of Sciences
  * Copyright (c) 2020-2021 Peng Cheng Laboratory
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
  * *************************************************************************************
  */

// See LICENSE.SiFive for license details.

package huancun.utils

import chisel3._
import chisel3.util._
import chisel3.experimental.ExtModule
import freechips.rocketchip.tilelink.LFSR64

class TS5N28HPCPLVTA512X64M4F extends ExtModule with HasExtModuleResource {
  //  val io = IO(new Bundle {
  val Q =   IO(Output(UInt(64.W)))
  val CLK = IO(Input(Clock()))
  val CEB = IO(Input(Bool()))
  val WEB = IO(Input(Bool()))
  val A =   IO(Input(UInt(9.W)))
  val D =   IO(Input(UInt(64.W)))
  //  })
  addResource("/TS5N28HPCPLVTA512X64M4F.v")
}

class TS5N28HPCPLVTA128X8M2F extends ExtModule with HasExtModuleResource {
  //  val io = IO(new Bundle {
  val Q =   IO(Output(UInt(8.W)))
  val CLK = IO(Input(Clock()))
  val CEB = IO(Input(Bool()))
  val WEB = IO(Input(Bool()))
  val A =   IO(Input(UInt(7.W)))
  val D =   IO(Input(UInt(8.W)))
  //  })
  addResource("/TS5N28HPCPLVTA128X8M2F.v")
}

class S011HD1P extends ExtModule with HasExtModuleResource {
  val Q = IO(Output(UInt()))
  val CLK = IO(Input(Clock()))
  val CEB = IO(Input(Bool()))
  val WEB = IO(Input(Bool()))
  val A = IO(Input(UInt()))
  val D = IO(Input(UInt()))
}

/*class S011HD1P_64_20 extends ExtModule with HasExtModuleResource {
  //  val io = IO(new Bundle {
  val Q =   IO(Output(UInt(20.W)))
  val CLK = IO(Input(Clock()))
  val CEB = IO(Input(Bool()))
  val WEB = IO(Input(Bool()))
  val A =   IO(Input(UInt(6.W)))
  val D =   IO(Input(UInt(20.W)))
  //  })
  addResource("/S011HD1P_64_20_1.v")
}*/

class S011HD1P_64_20_1 extends S011HD1P {
  //  val io = IO(new Bundle {
  override val Q = IO(Output(UInt(32.W)))
  override val A = IO(Input(UInt(6.W)))
  override val D = IO(Input(UInt(32.W)))
  //  })
  addResource("/S011HD1P_64_20_1.v")
}

class S011HD1P_128_19 extends S011HD1P {
  //  val io = IO(new Bundle {
  override val Q = IO(Output(UInt(19.W)))
  override val A = IO(Input(UInt(7.W)))
  override val D = IO(Input(UInt(19.W)))
  //  })
  addResource("/S011HD1P_128_19.v")
}

class TS5N28HPCPLVTA128X32M2F extends ExtModule with HasExtModuleResource {
  //  val io = IO(new Bundle {
  val Q =   IO(Output(UInt(32.W)))
  val CLK = IO(Input(Clock()))
  val CEB = IO(Input(Bool()))
  val WEB = IO(Input(Bool()))
  val A =   IO(Input(UInt(7.W)))
  val D =   IO(Input(UInt(32.W)))
  //  })
  addResource("/TS5N28HPCPLVTA128X32M2F.v")
}

// class S011HD1P_128_7 extends ExtModule with HasExtModuleResource {
//   //  val io = IO(new Bundle {
//   val Q =   IO(Output(UInt(8.W)))
//   val CLK = IO(Input(Clock()))
//   val CEB = IO(Input(Bool()))
//   val WEB = IO(Input(Bool()))
//   val A =   IO(Input(UInt(7.W)))
//   val D =   IO(Input(UInt(8.W)))
//   //  })
//   addResource("/S011HD1P_128_7.v")
// }

/*class S011HD1P_128_19 extends ExtModule with HasExtModuleResource {
  //  val io = IO(new Bundle {
  val Q =   IO(Output(UInt(19.W)))
  val CLK = IO(Input(Clock()))
  val CEB = IO(Input(Bool()))
  val WEB = IO(Input(Bool()))
  val A =   IO(Input(UInt(7.W)))
  val D =   IO(Input(UInt(19.W)))
  //  })
  addResource("/S011HD1P_128_19.v")
}*/

object HoldUnless {
  def apply[T <: Data](x: T, en: Bool): T = Mux(en, x, RegEnable(x, 0.U.asTypeOf(x), en))
}

object DelayTwoCycle {
  def apply[T <: Data](x: T, en: Bool): T = {
    val en1 = RegNext(en)
    val data_reg = RegEnable(x, en1)
    val en2 = RegNext(en1)
    Mux(en2, data_reg, LFSR64().asTypeOf(data_reg))
  }
}

object ReadAndHold {
  def apply[T <: Data](x: Mem[T], addr:         UInt, en: Bool): T = HoldUnless(x.read(addr), en)
  def apply[T <: Data](x: SyncReadMem[T], addr: UInt, en: Bool): T = HoldUnless(x.read(addr, en), RegNext(en))
}

class SRAMBundleA(val set: Int) extends Bundle {
  val setIdx = Output(UInt(log2Up(set).W))

  def apply(setIdx: UInt) = {
    this.setIdx := setIdx
    this
  }
}

class SRAMBundleAW[T <: Data](private val gen: T, set: Int, val way: Int = 1) extends SRAMBundleA(set) {
  val data = Output(Vec(way, gen))
  val waymask = if (way > 1) Some(Output(UInt(way.W))) else None

  def apply(data: Vec[T], setIdx: UInt, waymask: UInt): SRAMBundleAW[T] = {
    super.apply(setIdx)
    this.data := data
    this.waymask.map(_ := waymask)
    this
  }
  // this could only be used when waymask is onehot or nway is 1
  def apply(data: T, setIdx: UInt, waymask: UInt): SRAMBundleAW[T] = {
    apply(VecInit(Seq.fill(way)(data)), setIdx, waymask)
    this
  }
}

class SRAMBundleR[T <: Data](private val gen: T, val way: Int = 1) extends Bundle {
  val data = Output(Vec(way, gen))
}

class SRAMReadBus[T <: Data](private val gen: T, val set: Int, val way: Int = 1) extends Bundle {
  val req = Decoupled(new SRAMBundleA(set))
  val resp = Flipped(new SRAMBundleR(gen, way))

  def apply(valid: Bool, setIdx: UInt) = {
    this.req.bits.apply(setIdx)
    this.req.valid := valid
    this
  }
}

class SRAMWriteBus[T <: Data](private val gen: T, val set: Int, val way: Int = 1) extends Bundle {
  val req = Decoupled(new SRAMBundleAW(gen, set, way))

  def apply(valid: Bool, data: Vec[T], setIdx: UInt, waymask: UInt): SRAMWriteBus[T] = {
    this.req.bits.apply(data = data, setIdx = setIdx, waymask = waymask)
    this.req.valid := valid
    this
  }
  def apply(valid: Bool, data: T, setIdx: UInt, waymask: UInt): SRAMWriteBus[T] = {
    apply(valid, VecInit(Seq.fill(way)(data)), setIdx, waymask)
    this
  }
}

class SRAMTemplate[T <: Data]
(
  gen: T, set: Int, way: Int = 1,
  shouldReset: Boolean = false, holdRead: Boolean = false,
  singlePort: Boolean = false, bypassWrite: Boolean = false,
  clk_div_by_2: Boolean = false, input_clk_div_by_2: Boolean = false
) extends Module {
  val io = IO(new Bundle {
    val r = Flipped(new SRAMReadBus(gen, set, way))
    val w = Flipped(new SRAMWriteBus(gen, set, way))
  })
  override def desiredName: String = if (input_clk_div_by_2) s"ClkDiv2SRAMTemplate" else super.desiredName
  val wordType = UInt(gen.getWidth.W)
  val array = SyncReadMem(set, Vec(way, wordType))
  val (resetState, resetSet) = (WireInit(false.B), WireInit(0.U))

  if (shouldReset) {
    val _resetState = RegInit(true.B)
    val (_resetSet, resetFinish) = Counter(_resetState, set)
    when (resetFinish) { _resetState := false.B }

    resetState := _resetState
    resetSet := _resetSet
  }

  println("L2 + L3 len: %d, set: %d, way: %d\n", gen.getWidth.W, set, way)

  val (ren, wen) = (io.r.req.valid, io.w.req.valid || resetState)
  val realRen = (if (singlePort) ren && !wen else ren)

  val setIdx = Mux(resetState, resetSet, io.w.req.bits.setIdx)
  val wdata = VecInit(Mux(resetState, 0.U.asTypeOf(Vec(way, gen)), io.w.req.bits.data).map(_.asTypeOf(wordType)))
  val waymask = Mux(resetState, Fill(way, "b1".U), io.w.req.bits.waymask.getOrElse("b1".U))
  when (wen) { array.write(setIdx, wdata, waymask.asBools) }

  val raw_rdata = array.read(io.r.req.bits.setIdx, realRen)

  // bypass for dual-port SRAMs
  require(!bypassWrite || bypassWrite && !singlePort)
  def need_bypass(wen: Bool, waddr: UInt, wmask: UInt, ren: Bool, raddr: UInt) : UInt = {
    val need_check = RegNext(ren && wen)
    val waddr_reg = RegNext(waddr)
    val raddr_reg = RegNext(raddr)
    require(wmask.getWidth == way)
    val bypass = Fill(way, need_check && waddr_reg === raddr_reg) & RegNext(wmask)
    bypass.asTypeOf(UInt(way.W))
  }
  val bypass_wdata = if (bypassWrite) VecInit(RegNext(io.w.req.bits.data).map(_.asTypeOf(wordType)))
  else VecInit((0 until way).map(_ => LFSR64().asTypeOf(wordType)))
  val bypass_mask = need_bypass(io.w.req.valid, io.w.req.bits.setIdx, io.w.req.bits.waymask.getOrElse("b1".U), io.r.req.valid, io.r.req.bits.setIdx)
  val mem_rdata = {
    if (singlePort) raw_rdata
    else VecInit(bypass_mask.asBools.zip(raw_rdata).zip(bypass_wdata).map {
      case ((m, r), w) => Mux(m, w, r)
    })
  }

  // hold read data for SRAMs
  val rdata = (
    if(clk_div_by_2){
      // DelayTwoCycle(mem_rdata, realRen)
      // Now we assume rdata will not change during two cycles
      mem_rdata
    } else if (holdRead) {
      HoldUnless(mem_rdata, RegNext(realRen))
    } else {
      mem_rdata
    }).map(_.asTypeOf(gen))

  if(clk_div_by_2){
    CustomAnnotations.annotateClkDivBy2(this)
  }
  if(!isPow2(set)){
    CustomAnnotations.annotateSpecialDepth(this)
  }

  io.r.resp.data := VecInit(rdata)
  io.r.req.ready := !resetState && (if (singlePort) !wen else true.B)
  io.w.req.ready := true.B

}

class ReplaceSRAMTemplate[T <: Data]
(
  gen: T, set: Int, way: Int = 1,
  shouldReset: Boolean = false, holdRead: Boolean = false,
  singlePort: Boolean = false, bypassWrite: Boolean = false,
  clk_div_by_2: Boolean = false, input_clk_div_by_2: Boolean = false
) extends Module {
  val io = IO(new Bundle {
    val r = Flipped(new SRAMReadBus(gen, set, way))
    val w = Flipped(new SRAMWriteBus(gen, set, way))
  })
  override def desiredName: String = if (input_clk_div_by_2) s"ClkDiv2SRAMTemplate" else super.desiredName
  val wordType = UInt(gen.getWidth.W)
  //val array = SyncReadMem(set, Vec(way, wordType))
  val sram = Seq.fill(way)(Module(new TS5N28HPCPLVTA128X8M2F()))
  val (resetState, resetSet) = (WireInit(false.B), WireInit(0.U))

  if (shouldReset) {
    val _resetState = RegInit(true.B)
    val (_resetSet, resetFinish) = Counter(_resetState, set)
    when (resetFinish) { _resetState := false.B }

    resetState := _resetState
    resetSet := _resetSet
  }

  println("L2 + L3 Replace len: %d, set: %d, way: %d\n", gen.getWidth.W, set, way)

  val (ren, wen) = (io.r.req.valid, io.w.req.valid || resetState)
  val realRen = (if (singlePort) ren && !wen else ren)

  val setIdx = Mux(resetState, resetSet, io.w.req.bits.setIdx)
  val wdata = VecInit(Mux(resetState, 0.U.asTypeOf(Vec(way, gen)), io.w.req.bits.data).map(_.asTypeOf(wordType)))
  val wdataword = Mux(resetState, 0.U.asTypeOf(wordType), io.w.req.bits.data.asUInt)
  val waymask = Mux(resetState, Fill(way, "b1".U), io.w.req.bits.waymask.getOrElse("b1".U))
  //when (wen) { array.write(setIdx, wdata, waymask.asBools) }
  sram.map(_.CLK := clock)
  sram.map(_.A := Mux(wen, setIdx, io.r.req.bits.setIdx))
  sram.zipWithIndex.map{
    case (s, i) => s.CEB := ~(wen || realRen)
  }
  sram.zipWithIndex.map{
    case (s, i) => s.WEB := ~(wen && OHToUInt(io.w.req.bits.waymask.getOrElse("b0".U)) === i.U)
  }
  sram.map(_.D := wdataword)
  //val raw_rdata = array.read(io.r.req.bits.setIdx, realRen)
  val raw_rdata = VecInit(sram.map(_.Q))
  // bypass for dual-port SRAMs
  require(!bypassWrite || bypassWrite && !singlePort)
  def need_bypass(wen: Bool, waddr: UInt, wmask: UInt, ren: Bool, raddr: UInt) : UInt = {
    val need_check = RegNext(ren && wen)
    val waddr_reg = RegNext(waddr)
    val raddr_reg = RegNext(raddr)
    require(wmask.getWidth == way)
    val bypass = Fill(way, need_check && waddr_reg === raddr_reg) & RegNext(wmask)
    bypass.asTypeOf(UInt(way.W))
  }
  val bypass_wdata = if (bypassWrite) VecInit(RegNext(io.w.req.bits.data).map(_.asTypeOf(wordType)))
  else VecInit((0 until way).map(_ => LFSR64().asTypeOf(wordType)))
  val bypass_mask = need_bypass(io.w.req.valid, io.w.req.bits.setIdx, io.w.req.bits.waymask.getOrElse("b1".U), io.r.req.valid, io.r.req.bits.setIdx)
  val mem_rdata = {
    if (singlePort) raw_rdata
    else VecInit(bypass_mask.asBools.zip(raw_rdata).zip(bypass_wdata).map {
      case ((m, r), w) => Mux(m, w, r)
    })
  }

  // hold read data for SRAMs
  val rdata = (
    if(clk_div_by_2){
      // DelayTwoCycle(mem_rdata, realRen)
      // Now we assume rdata will not change during two cycles
      mem_rdata
    } else if (holdRead) {
      HoldUnless(mem_rdata, RegNext(realRen))
    } else {
      mem_rdata
    }).map(_.asTypeOf(gen))

  if(clk_div_by_2){
    CustomAnnotations.annotateClkDivBy2(this)
  }
  if(!isPow2(set)){
    CustomAnnotations.annotateSpecialDepth(this)
  }

  io.r.resp.data := VecInit(rdata)
  io.r.req.ready := !resetState && (if (singlePort) !wen else true.B)
  io.w.req.ready := true.B

}

class DataSRAMTemplate[T <: Data]
(
  gen: T, set: Int, way: Int = 1,
  shouldReset: Boolean = false, holdRead: Boolean = false,
  singlePort: Boolean = false, bypassWrite: Boolean = false,
  clk_div_by_2: Boolean = false, input_clk_div_by_2: Boolean = false
) extends Module {
  val io = IO(new Bundle {
    val r = Flipped(new SRAMReadBus(gen, set, way))
    val w = Flipped(new SRAMWriteBus(gen, set, way))
  })
  override def desiredName: String = if (input_clk_div_by_2) s"ClkDiv2SRAMTemplate" else super.desiredName
  val wordType = UInt(gen.getWidth.W)
  //val array = SyncReadMem(set, Vec(way, wordType))
  val sram = Seq.fill(way)(Module(new TS5N28HPCPLVTA512X64M4F()))
  val (resetState, resetSet) = (WireInit(false.B), WireInit(0.U))

  if (shouldReset) {
    val _resetState = RegInit(true.B)
    val (_resetSet, resetFinish) = Counter(_resetState, set)
    when (resetFinish) { _resetState := false.B }

    resetState := _resetState
    resetSet := _resetSet
  }

  println("L2 + L3 Data len: %d, set: %d, way: %d\n", gen.getWidth.W, set, way)

  val (ren, wen) = (io.r.req.valid, io.w.req.valid || resetState)
  val realRen = (if (singlePort) ren && !wen else ren)

  val setIdx = Mux(resetState, resetSet, io.w.req.bits.setIdx)
  val wdata = VecInit(Mux(resetState, 0.U.asTypeOf(Vec(way, gen)), io.w.req.bits.data).map(_.asTypeOf(wordType)))
  val wdataword = Mux(resetState, 0.U.asTypeOf(wordType), io.w.req.bits.data.asUInt)
  val waymask = Mux(resetState, Fill(way, "b1".U), io.w.req.bits.waymask.getOrElse("b1".U))
  //when (wen) { array.write(setIdx, wdata, waymask.asBools) }
  sram.map(_.CLK := clock)
  sram.map(_.A := Mux(wen, setIdx, io.r.req.bits.setIdx))
  sram.zipWithIndex.map{
    case (s, i) => s.CEB := ~(wen || realRen)
  }
  sram.zipWithIndex.map{
    case (s, i) => s.WEB := ~(wen && OHToUInt(io.w.req.bits.waymask.getOrElse("b0".U)) === i.U)
  }
  sram.map(_.D := wdataword)
  //val raw_rdata = array.read(io.r.req.bits.setIdx, realRen)
  val raw_rdata = VecInit(sram.map(_.Q))
  // bypass for dual-port SRAMs
  require(!bypassWrite || bypassWrite && !singlePort)
  def need_bypass(wen: Bool, waddr: UInt, wmask: UInt, ren: Bool, raddr: UInt) : UInt = {
    val need_check = RegNext(ren && wen)
    val waddr_reg = RegNext(waddr)
    val raddr_reg = RegNext(raddr)
    require(wmask.getWidth == way)
    val bypass = Fill(way, need_check && waddr_reg === raddr_reg) & RegNext(wmask)
    bypass.asTypeOf(UInt(way.W))
  }
  val bypass_wdata = if (bypassWrite) VecInit(RegNext(io.w.req.bits.data).map(_.asTypeOf(wordType)))
  else VecInit((0 until way).map(_ => LFSR64().asTypeOf(wordType)))
  val bypass_mask = need_bypass(io.w.req.valid, io.w.req.bits.setIdx, io.w.req.bits.waymask.getOrElse("b1".U), io.r.req.valid, io.r.req.bits.setIdx)
  val mem_rdata = {
    if (singlePort) raw_rdata
    else VecInit(bypass_mask.asBools.zip(raw_rdata).zip(bypass_wdata).map {
      case ((m, r), w) => Mux(m, w, r)
    })
  }

  // hold read data for SRAMs
  val rdata = (
    if(clk_div_by_2){
      // DelayTwoCycle(mem_rdata, realRen)
      // Now we assume rdata will not change during two cycles
      mem_rdata
    } else if (holdRead) {
      HoldUnless(mem_rdata, RegNext(realRen))
    } else {
      mem_rdata
    }).map(_.asTypeOf(gen))

  if(clk_div_by_2){
    CustomAnnotations.annotateClkDivBy2(this)
  }
  if(!isPow2(set)){
    CustomAnnotations.annotateSpecialDepth(this)
  }

  io.r.resp.data := VecInit(rdata)
  io.r.req.ready := !resetState && (if (singlePort) !wen else true.B)
  io.w.req.ready := true.B

}

class MetaSRAMTemplate[T <: Data]
(
  gen: T, set: Int, way: Int = 1,
  shouldReset: Boolean = false, holdRead: Boolean = false,
  singlePort: Boolean = false, bypassWrite: Boolean = false,
  clk_div_by_2: Boolean = false, input_clk_div_by_2: Boolean = false
) extends Module {
  val io = IO(new Bundle {
    val r = Flipped(new SRAMReadBus(gen, set, way))
    val w = Flipped(new SRAMWriteBus(gen, set, way))
  })
  override def desiredName: String = if (input_clk_div_by_2) s"ClkDiv2SRAMTemplate" else super.desiredName
  val wordType = UInt(gen.getWidth.W)
  //val array = SyncReadMem(set, Vec(way, wordType))
  val sram = Seq.fill(way)(Module(new TS5N28HPCPLVTA128X8M2F()))

  val (resetState, resetSet) = (WireInit(false.B), WireInit(0.U))

  if (shouldReset) {
    val _resetState = RegInit(true.B)
    val (_resetSet, resetFinish) = Counter(_resetState, set)
    when (resetFinish) { _resetState := false.B }

    resetState := _resetState
    resetSet := _resetSet
  }

  println("L2 + L3 Meta len: %d, set: %d, way: %d\n", gen.getWidth.W, set, way)

  val (ren, wen) = (io.r.req.valid, io.w.req.valid || resetState)
  val realRen = (if (singlePort) ren && !wen else ren)

  val setIdx = Mux(resetState, resetSet, io.w.req.bits.setIdx)
  val wdata = VecInit(Mux(resetState, 0.U.asTypeOf(Vec(way, gen)), io.w.req.bits.data).map(_.asTypeOf(wordType)))
  val wdataword = Mux(resetState, 0.U.asTypeOf(wordType), io.w.req.bits.data.asUInt)
  val waymask = Mux(resetState, Fill(way, "b1".U), io.w.req.bits.waymask.getOrElse("b1".U))
  //when (wen) { array.write(setIdx, wdata, waymask.asBools) }
  sram.map(_.CLK := clock)
  sram.map(_.A := Mux(wen, setIdx, io.r.req.bits.setIdx))
  sram.zipWithIndex.map{
    case (s, i) => s.CEB := ~(wen || realRen)
  }
  sram.zipWithIndex.map{
    case (s, i) => s.WEB := ~(wen && OHToUInt(io.w.req.bits.waymask.getOrElse("b0".U)) === i.U)
  }
  sram.map(_.D := wdataword)
  //val raw_rdata = array.read(io.r.req.bits.setIdx, realRen)
  val raw_rdata = VecInit(sram.map(_.Q))
  // bypass for dual-port SRAMs
  require(!bypassWrite || bypassWrite && !singlePort)
  def need_bypass(wen: Bool, waddr: UInt, wmask: UInt, ren: Bool, raddr: UInt) : UInt = {
    val need_check = RegNext(ren && wen)
    val waddr_reg = RegNext(waddr)
    val raddr_reg = RegNext(raddr)
    require(wmask.getWidth == way)
    val bypass = Fill(way, need_check && waddr_reg === raddr_reg) & RegNext(wmask)
    bypass.asTypeOf(UInt(way.W))
  }
  val bypass_wdata = if (bypassWrite) VecInit(RegNext(io.w.req.bits.data).map(_.asTypeOf(wordType)))
  else VecInit((0 until way).map(_ => LFSR64().asTypeOf(wordType)))
  val bypass_mask = need_bypass(io.w.req.valid, io.w.req.bits.setIdx, io.w.req.bits.waymask.getOrElse("b1".U), io.r.req.valid, io.r.req.bits.setIdx)
  val mem_rdata = {
    if (singlePort) raw_rdata
    else VecInit(bypass_mask.asBools.zip(raw_rdata).zip(bypass_wdata).map {
      case ((m, r), w) => Mux(m, w, r)
    })
  }

  // hold read data for SRAMs
  val rdata = (
    if(clk_div_by_2){
      // DelayTwoCycle(mem_rdata, realRen)
      // Now we assume rdata will not change during two cycles
      mem_rdata
    } else if (holdRead) {
      HoldUnless(mem_rdata, RegNext(realRen))
    } else {
      mem_rdata
    }).map(_.asTypeOf(gen))

  if(clk_div_by_2){
    CustomAnnotations.annotateClkDivBy2(this)
  }
  if(!isPow2(set)){
    CustomAnnotations.annotateSpecialDepth(this)
  }

  io.r.resp.data := VecInit(rdata)
  io.r.req.ready := !resetState && (if (singlePort) !wen else true.B)
  io.w.req.ready := true.B

}

class TagSRAMTemplate[T <: Data]
(
  gen: T, set: Int, way: Int = 1,
  shouldReset: Boolean = false, holdRead: Boolean = false,
  singlePort: Boolean = false, bypassWrite: Boolean = false,
  clk_div_by_2: Boolean = false, input_clk_div_by_2: Boolean = false
) extends Module {
  val io = IO(new Bundle {
    val r = Flipped(new SRAMReadBus(gen, set, way))
    val w = Flipped(new SRAMWriteBus(gen, set, way))
  })
  override def desiredName: String = if (input_clk_div_by_2) s"ClkDiv2SRAMTemplate" else super.desiredName
  val wordType = UInt(gen.getWidth.W)
  //val array = SyncReadMem(set, Vec(way, wordType))
  val sram = Seq.fill(way)(Module(new TS5N28HPCPLVTA128X32M2F()))

  val (resetState, resetSet) = (WireInit(false.B), WireInit(0.U))

  if (shouldReset) {
    val _resetState = RegInit(true.B)
    val (_resetSet, resetFinish) = Counter(_resetState, set)
    when (resetFinish) { _resetState := false.B }

    resetState := _resetState
    resetSet := _resetSet
  }

  println("L2 + L3 Tag len: %d, set: %d, way: %d\n", gen.getWidth.W, set, way)

  val (ren, wen) = (io.r.req.valid, io.w.req.valid || resetState)
  val realRen = (if (singlePort) ren && !wen else ren)

  val setIdx = Mux(resetState, resetSet, io.w.req.bits.setIdx)
  val wdata = VecInit(Mux(resetState, 0.U.asTypeOf(Vec(way, gen)), io.w.req.bits.data).map(_.asTypeOf(wordType)))
  val wdataword = Mux(resetState, 0.U.asTypeOf(wordType), io.w.req.bits.data.asUInt)
  val waymask = Mux(resetState, Fill(way, "b1".U), io.w.req.bits.waymask.getOrElse("b1".U))
  //when (wen) { array.write(setIdx, wdata, waymask.asBools) }
  sram.map(_.CLK := clock)
  sram.map(_.A := Mux(wen, setIdx, io.r.req.bits.setIdx))
  sram.zipWithIndex.map{
    case (s, i) => s.CEB := ~(wen || realRen)
  }
  sram.zipWithIndex.map{
    case (s, i) => s.WEB := ~(wen && OHToUInt(io.w.req.bits.waymask.getOrElse("b0".U)) === i.U)
  }
  sram.map(_.D := wdataword)
  //val raw_rdata = array.read(io.r.req.bits.setIdx, realRen)
  val raw_rdata = VecInit(sram.map(_.Q))
  // bypass for dual-port SRAMs
  require(!bypassWrite || bypassWrite && !singlePort)
  def need_bypass(wen: Bool, waddr: UInt, wmask: UInt, ren: Bool, raddr: UInt) : UInt = {
    val need_check = RegNext(ren && wen)
    val waddr_reg = RegNext(waddr)
    val raddr_reg = RegNext(raddr)
    require(wmask.getWidth == way)
    val bypass = Fill(way, need_check && waddr_reg === raddr_reg) & RegNext(wmask)
    bypass.asTypeOf(UInt(way.W))
  }
  val bypass_wdata = if (bypassWrite) VecInit(RegNext(io.w.req.bits.data).map(_.asTypeOf(wordType)))
  else VecInit((0 until way).map(_ => LFSR64().asTypeOf(wordType)))
  val bypass_mask = need_bypass(io.w.req.valid, io.w.req.bits.setIdx, io.w.req.bits.waymask.getOrElse("b1".U), io.r.req.valid, io.r.req.bits.setIdx)
  val mem_rdata = {
    if (singlePort) raw_rdata
    else VecInit(bypass_mask.asBools.zip(raw_rdata).zip(bypass_wdata).map {
      case ((m, r), w) => Mux(m, w, r)
    })
  }

  // hold read data for SRAMs
  val rdata = (
    if(clk_div_by_2){
      // DelayTwoCycle(mem_rdata, realRen)
      // Now we assume rdata will not change during two cycles
      mem_rdata
    } else if (holdRead) {
      HoldUnless(mem_rdata, RegNext(realRen))
    } else {
      mem_rdata
    }).map(_.asTypeOf(gen))

  if(clk_div_by_2){
    CustomAnnotations.annotateClkDivBy2(this)
  }
  if(!isPow2(set)){
    CustomAnnotations.annotateSpecialDepth(this)
  }

  io.r.resp.data := VecInit(rdata)
  io.r.req.ready := !resetState && (if (singlePort) !wen else true.B)
  io.w.req.ready := true.B

}

class EccSRAMTemplate[T <: Data]
(
  gen: T, set: Int, way: Int = 1,
  shouldReset: Boolean = false, holdRead: Boolean = false,
  singlePort: Boolean = false, bypassWrite: Boolean = false,
  clk_div_by_2: Boolean = false, input_clk_div_by_2: Boolean = false
) extends Module {
  val io = IO(new Bundle {
    val r = Flipped(new SRAMReadBus(gen, set, way))
    val w = Flipped(new SRAMWriteBus(gen, set, way))
  })
  override def desiredName: String = if (input_clk_div_by_2) s"ClkDiv2SRAMTemplate" else super.desiredName
  val wordType = UInt(gen.getWidth.W)
  val array = SyncReadMem(set, Vec(way, wordType))
  val (resetState, resetSet) = (WireInit(false.B), WireInit(0.U))

  if (shouldReset) {
    val _resetState = RegInit(true.B)
    val (_resetSet, resetFinish) = Counter(_resetState, set)
    when (resetFinish) { _resetState := false.B }

    resetState := _resetState
    resetSet := _resetSet
  }

  println("L2 + L3 Ecc len: %d, set: %d, way: %d\n", gen.getWidth.W, set, way)

  val (ren, wen) = (io.r.req.valid, io.w.req.valid || resetState)
  val realRen = (if (singlePort) ren && !wen else ren)

  val setIdx = Mux(resetState, resetSet, io.w.req.bits.setIdx)
  val wdata = VecInit(Mux(resetState, 0.U.asTypeOf(Vec(way, gen)), io.w.req.bits.data).map(_.asTypeOf(wordType)))
  val waymask = Mux(resetState, Fill(way, "b1".U), io.w.req.bits.waymask.getOrElse("b1".U))
  when (wen) { array.write(setIdx, wdata, waymask.asBools) }

  val raw_rdata = array.read(io.r.req.bits.setIdx, realRen)

  // bypass for dual-port SRAMs
  require(!bypassWrite || bypassWrite && !singlePort)
  def need_bypass(wen: Bool, waddr: UInt, wmask: UInt, ren: Bool, raddr: UInt) : UInt = {
    val need_check = RegNext(ren && wen)
    val waddr_reg = RegNext(waddr)
    val raddr_reg = RegNext(raddr)
    require(wmask.getWidth == way)
    val bypass = Fill(way, need_check && waddr_reg === raddr_reg) & RegNext(wmask)
    bypass.asTypeOf(UInt(way.W))
  }
  val bypass_wdata = if (bypassWrite) VecInit(RegNext(io.w.req.bits.data).map(_.asTypeOf(wordType)))
  else VecInit((0 until way).map(_ => LFSR64().asTypeOf(wordType)))
  val bypass_mask = need_bypass(io.w.req.valid, io.w.req.bits.setIdx, io.w.req.bits.waymask.getOrElse("b1".U), io.r.req.valid, io.r.req.bits.setIdx)
  val mem_rdata = {
    if (singlePort) raw_rdata
    else VecInit(bypass_mask.asBools.zip(raw_rdata).zip(bypass_wdata).map {
      case ((m, r), w) => Mux(m, w, r)
    })
  }

  // hold read data for SRAMs
  val rdata = (
    if(clk_div_by_2){
      // DelayTwoCycle(mem_rdata, realRen)
      // Now we assume rdata will not change during two cycles
      mem_rdata
    } else if (holdRead) {
      HoldUnless(mem_rdata, RegNext(realRen))
    } else {
      mem_rdata
    }).map(_.asTypeOf(gen))

  if(clk_div_by_2){
    CustomAnnotations.annotateClkDivBy2(this)
  }
  if(!isPow2(set)){
    CustomAnnotations.annotateSpecialDepth(this)
  }

  io.r.resp.data := VecInit(rdata)
  io.r.req.ready := !resetState && (if (singlePort) !wen else true.B)
  io.w.req.ready := true.B

}

class SRAMTemplateWithArbiter[T <: Data](nRead: Int, gen: T, set: Int, way: Int = 1,
                                         shouldReset: Boolean = false) extends Module {
  val io = IO(new Bundle {
    val r = Flipped(Vec(nRead, new SRAMReadBus(gen, set, way)))
    val w = Flipped(new SRAMWriteBus(gen, set, way))
  })

  val ram = Module(new SRAMTemplate(gen, set, way, shouldReset, holdRead = false, singlePort = true))
  ram.io.w <> io.w

  val readArb = Module(new Arbiter(chiselTypeOf(io.r(0).req.bits), nRead))
  readArb.io.in <> io.r.map(_.req)
  ram.io.r.req <> readArb.io.out

  // latch read results
  io.r.map{ case r => {
    r.resp.data := HoldUnless(ram.io.r.resp.data, RegNext(r.req.fire()))
  }}
}