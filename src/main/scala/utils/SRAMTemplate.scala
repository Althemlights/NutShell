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

package utils

import chisel3._
import chisel3.experimental.ExtModule
import chisel3.util._
import top.Settings
class S011HD1P_X32Y2D128 extends ExtModule with HasExtModuleResource {
  //  val io = IO(new Bundle {
  val Q =   IO(Output(UInt(128.W)))
  val CLK = IO(Input(Clock()))
  val CEN = IO(Input(Bool()))
  val WEN = IO(Input(Bool()))
  val A =   IO(Input(UInt(6.W)))
  val D =   IO(Input(UInt(128.W)))
  //  })
  addResource("/vsrc/S011HD1P_X32Y2D128.v")
}

class S011HD1P_X32Y2D128_BW extends ExtModule with HasExtModuleResource {
  //  val io = IO(new Bundle {
  val Q =    IO(Output(UInt(128.W)))
  val CLK =  IO(Input(Clock()))
  val CEN =  IO(Input(Bool()))
  val WEN =  IO(Input(Bool()))
  val BWEN = IO(Input(UInt(128.W)))
  val A =    IO(Input(UInt(6.W)))
  val D =    IO(Input(UInt(128.W)))
  //  })
  addResource("/vsrc/S011HD1P_X32Y2D128_BW.v")
}
class SRAMBundleA(val set: Int) extends Bundle {
  val setIdx = Output(UInt(log2Up(set).W))

  def apply(setIdx: UInt) = {
    this.setIdx := setIdx
    this
  }
}
class SramIO extends Bundle {
  val rData: UInt = Output(UInt(128.W))

  val en: Bool = Input(Bool())
  val idx: UInt =Input(UInt(6.W))
  val wen: Bool =Input(Bool())
//  val wMask: UInt = Input(UInt(128.W))
  val wData: UInt = Input(UInt(128.W))
}
class SramIO4 extends Bundle {
  val rData: UInt = Output(UInt(128.W))

  val en: Bool = Input(Bool())
  val idx: UInt =Input(UInt(9.W))
  val wen: Bool =Input(Bool())
//  val wMask: UInt = Input(UInt(128.W))
  val wData: UInt = Input(UInt(128.W))
}

class Sram extends Module {
  val io = IO(new SramIO)
  val sram: S011HD1P_X32Y2D128 = Module(new S011HD1P_X32Y2D128)
  io.rData  := sram.Q
  sram.CLK  := clock
  sram.CEN  := !io.en
  sram.A    := io.idx
  sram.WEN  := !io.wen
//  sram.BWEN := ~io.wMask
  sram.D    := io.wData
}
class Sram4 extends Module {
  val io = IO(new SramIO4)
  val sram = Seq.fill(4)(Module(new S011HD1P_X32Y2D128_BW))
  for(i <-0 until 4) {
    when (i.U === io.idx(2, 1)) {
//      io.rData := sram(i).Q
      sram(i).CLK := clock
      sram(i).CEN := !io.en
      sram(i).A := io.idx(8, 3)
      sram(i).WEN := !io.wen
      sram(i).D := Mux(io.idx(0), Cat(io.wData,0.U(64.W)), Cat(0.U(64.W), io.wData))
      sram(i).BWEN := Mux(io.idx(0), "h0000_0000_0000_0000_ffff_ffff_ffff_ffff".U,
        "hffff_ffff_ffff_ffff_0000_0000_0000_0000".U)
    }.elsewhen(i.U =/= io.idx(2, 1)) {
//      io.rData := sram(i).Q
      sram(i).CLK := clock
      sram(i).CEN := true.B
      sram(i).A := io.idx(8, 3)
      sram(i).WEN := true.B
      sram(i).D := 0.U
      sram(i).BWEN := "hffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff".U
    }
  }
  val rdatasel = WireInit(0.U)
  when (0.U === RegNext(io.idx(2, 1))) {
    rdatasel := sram(0).Q
  }.elsewhen(1.U === RegNext(io.idx(2, 1))) {
    rdatasel := sram(1).Q
  }.elsewhen(2.U === RegNext(io.idx(2, 1))) {
    rdatasel := sram(2).Q
  }.otherwise {
    rdatasel := sram(3).Q
  }
  io.rData := Mux((RegNext(io.idx(0))), rdatasel(127,64), rdatasel(63,0))
}
class SRAMBundleAW[T <: Data](private val gen: T, set: Int, val way: Int = 1) extends SRAMBundleA(set) {
  val data = Output(gen)
  val waymask = if (way > 1) Some(Output(UInt(way.W))) else None

  def apply(data: T, setIdx: UInt, waymask: UInt) = {
    super.apply(setIdx)
    this.data := data
    this.waymask.map(_ := waymask)
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

  def apply(valid: Bool, data: T, setIdx: UInt, waymask: UInt) = {
    this.req.bits.apply(data = data, setIdx = setIdx, waymask = waymask)
    this.req.valid := valid
    this
  }
}

class SRAMTemplate[T <: Data](gen: T, set: Int, way: Int = 1,
                              shouldReset: Boolean = false, holdRead: Boolean = false, singlePort: Boolean = false) extends Module {
  val io = IO(new Bundle {
    val r = Flipped(new SRAMReadBus(gen, set, way))
    val w = Flipped(new SRAMWriteBus(gen, set, way))
  })

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

  val (ren, wen) = (io.r.req.valid, io.w.req.valid || resetState)
  val realRen = (if (singlePort) ren && !wen else ren)

  val setIdx = Mux(resetState, resetSet, io.w.req.bits.setIdx)
  val wdataword = Mux(resetState, 0.U.asTypeOf(wordType), io.w.req.bits.data.asUInt)
  val waymask = Mux(resetState, Fill(way, "b1".U), io.w.req.bits.waymask.getOrElse("b1".U))
  val wdata = VecInit(Seq.fill(way)(wdataword))
  when (wen) { array.write(setIdx, wdata, waymask.asBools) }

  val rdata = (if (holdRead) ReadAndHold(array, io.r.req.bits.setIdx, realRen)
  else array.read(io.r.req.bits.setIdx, realRen)).map(_.asTypeOf(gen))
  io.r.resp.data := VecInit(rdata)

  io.r.req.ready := !resetState && (if (singlePort) !wen else true.B)
  io.w.req.ready := true.B

  Debug(false) {
    when (wen) {
      printf("%d: SRAMTemplate: write %x to idx = %d\n", GTimer(), wdata.asUInt, setIdx)
    }
    when (RegNext(realRen)) {
      printf("%d: SRAMTemplate: read %x at idx = %d\n", GTimer(), VecInit(rdata).asUInt, RegNext(io.r.req.bits.setIdx))
    }
  }
}

class ysyxDataSRAMTemplate[T <: Data](gen: T, set: Int, way: Int = 1,
                                  shouldReset: Boolean = false, holdRead: Boolean = false, singlePort: Boolean = false) extends Module {
  val io = IO(new Bundle {
    val r = Flipped(new SRAMReadBus(gen, set, way))
    val w = Flipped(new SRAMWriteBus(gen, set, way))
  })

  require(!holdRead)
  val wordType = UInt(gen.getWidth.W)
  // val array = SyncReadMem(set, Vec(way, wordType))
  val sram = Seq.fill(way)(Module(new S011HD1P_X32Y2D128()))
  val (resetState, resetSet) = (WireInit(false.B), WireInit(0.U))

  if (shouldReset) {
    val _resetState = RegInit(true.B)
    val (_resetSet, resetFinish) = Counter(_resetState, set)
    when (resetFinish) { _resetState := false.B }

    resetState := _resetState
    resetSet := _resetSet
  }

  val (ren, wen) = (io.r.req.valid, io.w.req.valid || resetState)
  val realRen = (if (singlePort) ren && !wen else ren)

  val setIdx = Mux(resetState, resetSet, io.w.req.bits.setIdx)
  val wdataword = Mux(resetState, 0.U.asTypeOf(wordType), io.w.req.bits.data.asUInt)
  val waymask = Mux(resetState, Fill(way, "b1".U), io.w.req.bits.waymask.getOrElse("b1".U))
  val wdata = VecInit(Seq.fill(way)(wdataword))
  // when (wen) { array.write(setIdx, wdata, waymask.asBools) }

  sram.map(_.CLK := clock)
  sram.map(_.A := Mux(wen, setIdx, io.r.req.bits.setIdx))
  sram.zipWithIndex.map{
    case (s, i) => s.CEN := ~(wen || realRen)
  }
  sram.zipWithIndex.map{
    case (s, i) => s.WEN := ~(wen && OHToUInt(io.w.req.bits.waymask.getOrElse("b0".U)) === i.U)
  }
  sram.map(_.D := wdataword)

  val rdata = VecInit(sram.map(_.Q)).map(_.asTypeOf(gen))
  io.r.resp.data := VecInit(rdata)

  io.r.req.ready := !resetState && (if (singlePort) !wen else true.B)
  io.w.req.ready := true.B

  Debug(false) {
    when (wen) {
      printf("%d: SRAMTemplate: write %x to idx = %d\n", GTimer(), wdata.asUInt, setIdx)
    }
    when (RegNext(realRen)) {
      printf("%d: SRAMTemplate: read %x at idx = %d\n", GTimer(), VecInit(rdata).asUInt, RegNext(io.r.req.bits.setIdx))
    }
  }
}


class ysyxSRAMTemplate[T <: Data](gen: T, set: Int, way: Int = 1,
                              shouldReset: Boolean = false, holdRead: Boolean = false, singlePort: Boolean = true) extends Module {
  val io = IO(new Bundle {
    val r = Flipped(new SRAMReadBus(gen, set, way))
    val w = Flipped(new SRAMWriteBus(gen, set, way))
  })

  val wordType = UInt(gen.getWidth.W)
//  val array = SyncReadMem(set, Vec(way, wordType))
//  val sramnum = 32
  val sram = Seq.fill(4)(Module(new Sram4()))
  val (resetState, resetSet) = (WireInit(false.B), WireInit(0.U))

  if (shouldReset) {
    val _resetState = RegInit(true.B)
    val (_resetSet, resetFinish) = Counter(_resetState, set)
    when (resetFinish) { _resetState := false.B }

    resetState := _resetState
    resetSet := _resetSet
  }

  val (ren, wen) = (io.r.req.valid, io.w.req.valid || resetState)
  val realRen = (if (singlePort) ren && !wen else ren)

  val setIdx = Mux(resetState, resetSet, io.w.req.bits.setIdx)
  val wdataword = Mux(resetState, 0.U.asTypeOf(wordType), io.w.req.bits.data.asUInt)
  val waymask = Mux(resetState, Fill(way, "b1".U), io.w.req.bits.waymask.getOrElse("b1".U))
  val wdata = VecInit(Seq.fill(way)(wdataword))
//  when (wen) { array.write(setIdx, wdata, waymask.asBools) }
//  val Sram_read   = (WireInit(VecInit(Seq.fill(way)(Cat(0.U((128/way-gen.getWidth).W), wdataword)))))
//  val Sram_write  = WireInit(VecInit(Seq.fill(way)(Cat(0.U((128/way-gen.getWidth).W), wdataword))))
  val Sram_read = WireInit(VecInit(Seq.fill(way)(0.U(64.W))))

  sram.map(_.io.idx := Mux(wen, setIdx, io.r.req.bits.setIdx))
  sram.zipWithIndex.map{
    case (s, i) => s.io.en := (wen || realRen)
  }
  sram.zipWithIndex.map{
    case (s, i) => s.io.wen := (wen && OHToUInt(io.w.req.bits.waymask.getOrElse("b0".U)) === i.U)
  }
  sram.map(_.io.wData := wdataword)
  val rdata = VecInit(sram.map(_.io.rData))
  io.r.resp.data := VecInit(rdata.map(_.asTypeOf(gen)))

  io.r.req.ready := !resetState && (if (singlePort) !wen else true.B)
  io.w.req.ready := true.B

//  Debug(false) {
//    when (wen) {
//      printf("%d: SRAMTemplate: write %x to idx = %d\n", GTimer(), wdata.asUInt, setIdx)
//    }
//    when (RegNext(realRen)) {
//      printf("%d: SRAMTemplate: read %x at idx = %d\n", GTimer(), VecInit(rdata).asUInt, RegNext(io.r.req.bits.setIdx))
//    }
//  }
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

class ysyxSRAMTemplateWithArbiter[T <: Data](nRead: Int, gen: T, set: Int, way: Int = 1,
                                         shouldReset: Boolean = false) extends Module {
  val io = IO(new Bundle {
    val r = Flipped(Vec(nRead, new SRAMReadBus(gen, set, way)))
    val w = Flipped(new SRAMWriteBus(gen, set, way))
  })

  val ram = Module(new ysyxSRAMTemplate(gen, set, way, shouldReset, holdRead = false, singlePort = true))
  ram.io.w <> io.w

  val readArb = Module(new Arbiter(chiselTypeOf(io.r(0).req.bits), nRead))
  readArb.io.in <> io.r.map(_.req)
  ram.io.r.req <> readArb.io.out

  // latch read results
  io.r.map{ case r => {
    r.resp.data := HoldUnless(ram.io.r.resp.data, RegNext(r.req.fire()))
  }}
}

class BTBSRAMTemplate[T <: Data](gen: T, set: Int, way: Int = 1,
                                 shouldReset: Boolean = false, holdRead: Boolean = false, singlePort: Boolean = false) extends Module {
  val io = IO(new Bundle {
    val r = Flipped(new SRAMReadBus(gen, set, way))
    val w = Flipped(new SRAMWriteBus(gen, set, way))
  })

  require(holdRead)
  val wordType = UInt(gen.getWidth.W)
  // val array = SyncReadMem(set, Vec(way, wordType))
  val sram = Module(new S011HD1P_X32Y2D128())

  val (resetState, resetSet) = (WireInit(false.B), WireInit(0.U))

  if (shouldReset) {
    val _resetState = RegInit(true.B)
    val (_resetSet, resetFinish) = Counter(_resetState, set)
    when (resetFinish) { _resetState := false.B }

    resetState := _resetState
    resetSet := _resetSet
  }

  val (ren, wen) = (io.r.req.valid, io.w.req.valid || resetState)
  val realRen = (if (singlePort) ren && !wen else ren)

  val setIdx = Mux(resetState, resetSet, io.w.req.bits.setIdx)
  val wdataword = Mux(resetState, 0.U.asTypeOf(wordType), io.w.req.bits.data.asUInt)
  val waymask = Mux(resetState, Fill(way, "b1".U), io.w.req.bits.waymask.getOrElse("b1".U))
  val wdata = VecInit(Seq.fill(way)(wdataword))
  // when (wen) { array.write(setIdx, wdata, waymask.asBools) }

  sram.CLK := clock
  sram.A := Mux(wen, setIdx, io.r.req.bits.setIdx)
  sram.CEN := ~(wen || realRen)
  sram.WEN := ~wen
  sram.D := Cat(0.U((128-gen.getWidth).W), Cat(wdata))

  val rdata = HoldUnless(sram.Q, RegNext(realRen, false.B))
  io.r.resp.data := rdata(gen.getWidth-1, 0).asTypeOf(io.r.resp.data)

  io.r.req.ready := !resetState && (if (singlePort) !wen else true.B)
  io.w.req.ready := true.B

  Debug(false) {
    when (wen) {
      printf("%d: SRAMTemplate: write %x to idx = %d\n", GTimer(), wdata.asUInt, setIdx)
    }
    when (RegNext(realRen)) {
      printf("%d: SRAMTemplate: read %x at idx = %d\n", GTimer(), VecInit(rdata).asUInt, RegNext(io.r.req.bits.setIdx))
    }
  }
}

class MetaSRAMTemplate[T <: Data](gen: T, set: Int, way: Int = 1,
                                  shouldReset: Boolean = false, holdRead: Boolean = false, singlePort: Boolean = false) extends Module {
  val io = IO(new Bundle {
    val r = Flipped(new SRAMReadBus(gen, set, way))
    val w = Flipped(new SRAMWriteBus(gen, set, way))
  })
  require(!holdRead)
  val wordType = UInt(gen.getWidth.W)
  // val array = SyncReadMem(set, Vec(way, wordType))
  val sram = Module(new S011HD1P_X32Y2D128_BW())
  val (resetState, resetSet) = (WireInit(false.B), WireInit(0.U))

  if (shouldReset) {
    val _resetState = RegInit(true.B)
    val (_resetSet, resetFinish) = Counter(_resetState, set)
    when (resetFinish) { _resetState := false.B }

    resetState := _resetState
    resetSet := _resetSet
  }

  val (ren, wen) = (io.r.req.valid, io.w.req.valid || resetState)
  val realRen = (if (singlePort) ren && !wen else ren)

  val setIdx = Mux(resetState, resetSet, io.w.req.bits.setIdx)
  val wdataword = Mux(resetState, 0.U.asTypeOf(wordType), io.w.req.bits.data.asUInt)
  val waymask = Mux(resetState, Fill(way, "b1".U), io.w.req.bits.waymask.getOrElse("b1".U))
  val wdata = VecInit(Seq.fill(way)(Cat(0.U((128/way-gen.getWidth).W), wdataword)))
  // when (wen) { array.write(setIdx, wdata, waymask.asBools) }

  sram.CLK := clock
  sram.A := Mux(wen, setIdx, io.r.req.bits.setIdx)
  sram.CEN := ~(wen || realRen)
  sram.WEN := ~wen
  sram.BWEN := ~FillInterleaved(128/way, waymask)
  sram.D := Cat(wdata)

  val rdata = sram.Q.asTypeOf(Vec(way, UInt((128/way).W))).map(_.asTypeOf(gen))
  io.r.resp.data := VecInit(rdata)

  io.r.req.ready := !resetState && (if (singlePort) !wen else true.B)
  io.w.req.ready := true.B

  Debug(false) {
    when (wen) {
      printf("%d: SRAMTemplate: write %x to idx = %d\n", GTimer(), wdata.asUInt, setIdx)
      printf("%d: SRAMTemplate: BWEN: %x D: %x\n", GTimer(), sram.BWEN, sram.D)
    }
    when (RegNext(realRen)) {
      printf("%d: SRAMTemplate: read %x at idx = %d\n", GTimer(), VecInit(rdata).asUInt, RegNext(io.r.req.bits.setIdx))
      printf("%d: SRAMTemplate: Q %x at idx = %d\n", GTimer(), sram.Q, RegNext(io.r.req.bits.setIdx))
    }
  }
}

class DataSRAMTemplate[T <: Data](gen: T, set: Int, way: Int = 1,
                                  shouldReset: Boolean = false, holdRead: Boolean = false, singlePort: Boolean = false) extends Module {
  val io = IO(new Bundle {
    val r = Flipped(new SRAMReadBus(gen, set, way))
    val w = Flipped(new SRAMWriteBus(gen, set, way))
  })

  require(!holdRead)
  val wordType = UInt(gen.getWidth.W)
  // val array = SyncReadMem(set, Vec(way, wordType))
  val sram = Seq.fill(way)(Module(new Sram()))
  val (resetState, resetSet) = (WireInit(false.B), WireInit(0.U))

  if (shouldReset) {
    val _resetState = RegInit(true.B)
    val (_resetSet, resetFinish) = Counter(_resetState, set)
    when (resetFinish) { _resetState := false.B }

    resetState := _resetState
    resetSet := _resetSet
  }

  val (ren, wen) = (io.r.req.valid, io.w.req.valid || resetState)
  val realRen = (if (singlePort) ren && !wen else ren)

  val setIdx = Mux(resetState, resetSet, io.w.req.bits.setIdx)
  val wdataword = Mux(resetState, 0.U.asTypeOf(wordType), io.w.req.bits.data.asUInt)
  val waymask = Mux(resetState, Fill(way, "b1".U), io.w.req.bits.waymask.getOrElse("b1".U))
  val wdata = VecInit(Seq.fill(way)(wdataword))
  // when (wen) { array.write(setIdx, wdata, waymask.asBools) }

//  sram.map(_.CLK := clock)
  sram.map(_.io.idx := Mux(wen, setIdx(5,0), io.r.req.bits.setIdx(5,0)))
  sram.zipWithIndex.map{
    case (s, i) => s.io.en := (wen || realRen)
  }
  sram.zipWithIndex.map{
    case (s, i) => s.io.wen := (wen && OHToUInt(io.w.req.bits.waymask.getOrElse("b0".U)) === i.U)
  }
  sram.map(_.io.wData := Mux(io.w.req.bits.setIdx(6), Cat(wdataword,(0.U(64.W))), Cat((0.U(64.W)),wdataword)))
//  sram.map(_.io.wMask := Mux(io.w.req.bits.setIdx(6),  Cat(VecInit(Seq.fill(64)(true.B)).asUInt(), VecInit(Seq.fill(64)(false.B)).asUInt()), Cat(VecInit(Seq.fill(64)(false.B)).asUInt(), VecInit(Seq.fill(64)(true.B)).asUInt())))
  val rdata = Mux(io.r.req.bits.setIdx(6),  VecInit(sram.map(_.io.rData(127, 64))),  VecInit(sram.map(_.io.rData(63, 0))))
  io.r.resp.data := VecInit(rdata.map(_.asTypeOf(gen)))

  io.r.req.ready := !resetState && (if (singlePort) !wen else true.B)
  io.w.req.ready := true.B

//  Debug(false) {
//    when (wen) {
//      printf("%d: SRAMTemplate: write %x to idx = %d\n", GTimer(), wdata.asUInt, setIdx)
//    }
//    when (RegNext(realRen)) {
//      printf("%d: SRAMTemplate: read %x at idx = %d\n", GTimer(), VecInit(rdata).asUInt, RegNext(io.r.req.bits.setIdx))
//    }
//  }
}

//class SRAMTemplateWithArbiter[T <: Data](nRead: Int, gen: T, set: Int, way: Int = 1,
//                                         shouldReset: Boolean = false, isData: Boolean = false) extends Module {
//  val io = IO(new Bundle {
//    val r = Flipped(Vec(nRead, new SRAMReadBus(gen, set, way)))
//    val w = Flipped(new SRAMWriteBus(gen, set, way))
//  })
//
//  val ram = if (isData) Module(new DataSRAMTemplate(gen, set, way, shouldReset, holdRead = false, singlePort = true))
//            else Module(new MetaSRAMTemplate(gen, set, way, shouldReset, holdRead = false, singlePort = true))
////  when(isData.asBool()) {
//    val ram = Module(new DataSRAMTemplate(gen, set, way, shouldReset, holdRead = false, singlePort = true))
////  }.otherwise {
////    val ram = Module(new MetaSRAMTemplate(gen, set, way, shouldReset, holdRead = false, singlePort = true))
////  }
//  println("len: %d, set: %d, way: %d\n", gen.getWidth.W, set, way)
//  ram.io.w <> io.w
//
//  val readArb = Module(new Arbiter(chiselTypeOf(io.r(0).req.bits), nRead))
//  readArb.io.in <> io.r.map(_.req)
//  ram.io.r.req <> readArb.io.out
//
//  // latch read results
//  io.r.map{ case r => {
//    r.resp.data := HoldUnless(ram.io.r.resp.data, RegNext(r.req.fire()))
//  }}
//}

class DataSRAMTemplateWithArbiter[T <: Data](nRead: Int, gen: T, set: Int, way: Int = 1,
                                             shouldReset: Boolean = false) extends Module {
  val io = IO(new Bundle {
    val r = Flipped(Vec(nRead, new SRAMReadBus(gen, set, way)))
    val w = Flipped(new SRAMWriteBus(gen, set, way))
  })

  //  val ram = if (isData) Module(new DataSRAMTemplate(gen, set, way, shouldReset, holdRead = false, singlePort = true))
  //  else Module(new MetaSRAMTemplate(gen, set, way, shouldReset, holdRead = false, singlePort = true))
  //  when(isData.asBool()) {
  val ram = Module(new DataSRAMTemplate(gen, set, way, shouldReset, holdRead = false, singlePort = true))
  //  }.otherwise {
  //    val ram = Module(new MetaSRAMTemplate(gen, set, way, shouldReset, holdRead = false, singlePort = true))
  //  }
  println("len: %d, set: %d, way: %d\n", gen.getWidth.W, set, way)
  ram.io.w <> io.w

  val readArb = Module(new Arbiter(chiselTypeOf(io.r(0).req.bits), nRead))
  readArb.io.in <> io.r.map(_.req)
  ram.io.r.req <> readArb.io.out

  // latch read results
  io.r.map{ case r => {
    r.resp.data := HoldUnless(ram.io.r.resp.data, RegNext(r.req.fire()))
  }}
}

class MetaSRAMTemplateWithArbiter[T <: Data](nRead: Int, nWrite: Int = 1, gen: T, set: Int, way: Int = 1,
                                             shouldReset: Boolean = false) extends Module {
  val io = IO(new Bundle {
    val r = Flipped(Vec(nRead, new SRAMReadBus(gen, set, way)))
    val w = Flipped(Vec(nWrite, new SRAMWriteBus(gen, set, way)))
  })

  //  val ram = if (isData) Module(new DataSRAMTemplate(gen, set, way, shouldReset, holdRead = false, singlePort = true))
  //  else Module(new MetaSRAMTemplate(gen, set, way, shouldReset, holdRead = false, singlePort = true))
  //  when(isData.asBool()) {
  val ram = Module(new MetaSRAMTemplate(gen, set, way, shouldReset, holdRead = false, singlePort = true))
  //  }.otherwise {
  //    val ram = Module(new MetaSRAMTemplate(gen, set, way, shouldReset, holdRead = false, singlePort = true))
  //  }
  println("len: %d, set: %d, way: %d\n", gen.getWidth.W, set, way)
  val writeArb = Module(new Arbiter(chiselTypeOf(io.w(0).req.bits), nWrite))
  writeArb.io.in <> io.w.map(_.req)
  ram.io.w.req <> writeArb.io.out


  val readArb = Module(new Arbiter(chiselTypeOf(io.r(0).req.bits), nRead))
  readArb.io.in <> io.r.map(_.req)
  ram.io.r.req <> readArb.io.out

  // latch read results
  io.r.map{ case r => {
    r.resp.data := HoldUnless(ram.io.r.resp.data, RegNext(r.req.fire()))
  }}
}