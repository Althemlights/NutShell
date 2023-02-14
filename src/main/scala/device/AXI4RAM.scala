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

package device

import chisel3._
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFile

import nutcore.HasNutCoreParameter
import bus.axi4._
import utils._

import freechips.rocketchip.amba.axi4.{AXI4MasterNode, AXI4Parameters, AXI4SlaveNode}
import freechips.rocketchip.diplomacy.{AddressSet, InModuleBody, LazyModule, LazyModuleImp}
import chipsalliance.rocketchip.config.Parameters
import chisel3.experimental.ExtModule

class RAMHelper(memByte: BigInt) extends BlackBox with HasNutCoreParameter {
  val io = IO(new Bundle {
    val clk = Input(Clock())
    val rIdx = Input(UInt(DataBits.W))
    val rdata = Output(UInt(DataBits.W))
    val wIdx = Input(UInt(DataBits.W))
    val wdata = Input(UInt(DataBits.W))
    val wmask = Input(UInt(DataBits.W))
    val wen = Input(Bool())
    val en = Input(Bool())
  }).suggestName("io")
}

class AXI4RAM[T <: AXI4Lite](_type: T = new AXI4, memByte: Int,
  useBlackBox: Boolean = false) extends AXI4SlaveModule(_type) with HasNutCoreParameter {

  val offsetBits = log2Up(memByte)
  val offsetMask = (1 << offsetBits) - 1
  def index(addr: UInt) = (addr & offsetMask.U) >> log2Ceil(DataBytes)
  def inRange(idx: UInt) = idx < (memByte / 8).U

  val wIdx = index(waddr) + writeBeatCnt
  val rIdx = index(raddr) + readBeatCnt
  val wen = in.w.fire && inRange(wIdx)

  val rdata = if (useBlackBox) {
    val mem = Module(new RAMHelper(memByte))
    mem.io.clk := clock
    mem.io.rIdx := rIdx
    mem.io.wIdx := wIdx
    mem.io.wdata := in.w.bits.data
    mem.io.wmask := fullMask
    mem.io.wen := wen
    mem.io.en := true.B
    mem.io.rdata
  } else {
    val mem = Mem(memByte / DataBytes, Vec(DataBytes, UInt(8.W)))

    val wdata = VecInit.tabulate(DataBytes) { i => in.w.bits.data(8*(i+1)-1, 8*i) }
    when (wen) { mem.write(wIdx, wdata, in.w.bits.strb.asBools) }

    Cat(mem.read(rIdx).reverse)
  }

  in.r.bits.data := RegEnable(rdata, ren)
}

class AXI4RAM1
(
  address: Seq[AddressSet],
  memByte: Long,
  useBlackBox: Boolean = false,
  executable: Boolean = true,
  beatBytes: Int = 8,
  burstLen: Int = 16,
)(implicit p: Parameters)
  extends AXI4SlaveModulexs(address, executable, beatBytes, burstLen)
{
  override lazy val module = new AXI4SlaveModuleImp(this){

    val split = beatBytes / 8
    val bankByte = memByte / split
    val offsetBits = log2Up(memByte)

    require(address.length >= 1)
    val baseAddress = address(0).base

    def index(addr: UInt) = ((addr - baseAddress.U)(offsetBits - 1, 0) >> log2Ceil(beatBytes)).asUInt()

    def inRange(idx: UInt) = idx < (memByte / beatBytes).U

    val wIdx = index(waddr) + writeBeatCnt
    val rIdx = index(raddr) + readBeatCnt
    val wen = in.w.fire() && inRange(wIdx)
    require(beatBytes >= 8)

    val rdata = if (useBlackBox) {
      val mems = (0 until split).map {_ => Module(new RAMHelper(bankByte))}
      mems.zipWithIndex map { case (mem, i) =>
        mem.io.clk   := clock
        mem.io.en    := !reset.asBool() && ((state === s_rdata) || (state === s_wdata))
        mem.io.rIdx  := (rIdx << log2Up(split)) + i.U
        mem.io.wIdx  := (wIdx << log2Up(split)) + i.U
        mem.io.wdata := in.w.bits.data((i + 1) * 64 - 1, i * 64)
        mem.io.wmask := MaskExpand(in.w.bits.strb((i + 1) * 8 - 1, i * 8))
        mem.io.wen   := wen
      }
      val rdata = mems.map {mem => mem.io.rdata}
      Cat(rdata.reverse)
    } else {
      val mem = Mem(memByte / beatBytes, Vec(beatBytes, UInt(8.W)))

      val wdata = VecInit.tabulate(beatBytes) { i => in.w.bits.data(8 * (i + 1) - 1, 8 * i) }
      when(wen) {
        mem.write(wIdx, wdata, in.w.bits.strb.asBools())
      }

      Cat(mem.read(rIdx).reverse)
    }
    in.r.bits.data := rdata
  }
}

class AXI4RAMWrapper (
  slave: AXI4SlaveNode,
  memByte: Long,
  useBlackBox: Boolean = false
 )(implicit p: Parameters) extends AXI4MemorySlave(slave, memByte, useBlackBox) {
  val ram = LazyModule(new AXI4RAM1(
    slaveParam.address, memByte, useBlackBox,
    slaveParam.executable, portParam.beatBytes, burstLen
  ))
  ram.node := master
}
