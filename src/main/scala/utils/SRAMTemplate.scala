package utils

import chisel3._
import chisel3.util._

class SRAMBundleA(set: Int) extends Bundle {
  val idx = Output(UInt(log2Up(set).W))
  override def cloneType = new SRAMBundleA(set).asInstanceOf[this.type]
}

class SRAMBundleAW[T <: Data](gen: T, set: Int, way: Int = 1) extends SRAMBundleA(set) {
  val data = Output(gen)
  val wordIndex = Output(UInt(log2Up(way).W))
  override def cloneType = new SRAMBundleAW(gen, set, way).asInstanceOf[this.type]
}

class SRAMBundleR[T <: Data](gen: T, way: Int = 1) extends Bundle {
  val data = Output(Vec(way, gen))
  override def cloneType = new SRAMBundleR(gen, way).asInstanceOf[this.type]
}

class SRAMReadBus[T <: Data](gen: T, set: Int, way: Int = 1) extends Bundle {
  val req = Decoupled(new SRAMBundleA(set))
  val resp = Flipped(new SRAMBundleR(gen, way))
  override def cloneType = new SRAMReadBus(gen, set, way).asInstanceOf[this.type]
}

class SRAMWriteBus[T <: Data](gen: T, set: Int, way: Int = 1) extends Bundle {
  val req = Decoupled(new SRAMBundleAW(gen, set, way))
  override def cloneType = new SRAMWriteBus(gen, set, way).asInstanceOf[this.type]
}

class SRAMTemplate[T <: Data](gen: T, set: Int, way: Int = 1,
  shouldReset: Boolean = false, holdRead: Boolean = false, singlePort: Boolean = false) extends Module {
  val io = IO(new Bundle {
    val r = Flipped(new SRAMReadBus(gen, set, way))
    val w = Flipped(new SRAMWriteBus(gen, set, way))
  })

  val wordType = UInt(gen.getWidth.W)
  val wayType = Vec(way, wordType)
  val array = SyncReadMem(set, wayType)
  val (resetState, resetIdx) = (WireInit(false.B), WireInit(0.U))

  if (shouldReset) {
    val _resetState = RegInit(true.B)
    val (_resetIdx, resetFinish) = Counter(_resetState, set)
    when (resetFinish) { _resetState := false.B }

    resetState := _resetState
    resetIdx := _resetIdx
  }

  val idx = Mux(resetState, resetIdx, io.w.req.bits.idx)
  val wdataword = Mux(resetState, 0.U.asTypeOf(wordType), io.w.req.bits.data.asUInt)
  val wordIndex = if (way > 1) io.w.req.bits.wordIndex else 0.U
  val wdata = WordShift(wdataword, wordIndex, gen.getWidth).asTypeOf(wayType)
  val wmask = if (way > 1) (1.U << wordIndex).asBools else Seq(true.B)

  val (ren, wen) = (io.r.req.valid, io.w.req.valid || resetState)
  val realRen = (if (singlePort) ren && !wen else ren)
  when (wen) { array.write(idx, wdata, wmask) }

  val rdata = (if (holdRead) ReadAndHold(array, io.r.req.bits.idx, realRen)
              else array.read(io.r.req.bits.idx, realRen)).map(_.asTypeOf(gen))
  io.r.resp.data := VecInit(rdata)
  io.r.req.ready := !resetState && (if (singlePort) !wen else true.B)
  io.w.req.ready := true.B
}
