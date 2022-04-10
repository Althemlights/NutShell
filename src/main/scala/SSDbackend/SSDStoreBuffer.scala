package SSDbackend

import bus.simplebus.SimpleBusCmd
import chisel3._
import nutcore._
import chisel3.util._
import utils._

trait HasStoreBufferConst{
  val StoreBufferSize = 8
}

class StoreBufferEntry extends NutCoreBundle{
  val paddr    = Output(UInt(PAddrBits.W))
  val data     = Output(UInt(XLEN.W))
  val mask     = Output(UInt((XLEN/8).W))
  val size     = Output(UInt(2.W))
}
class StoreBufferContent extends NutCoreBundle{
  val paddr    = UInt(PAddrBits.W)
  val data     = UInt(XLEN.W)
  val mask     = UInt((XLEN/8).W)
  val size     = UInt(2.W)
}
class StoreBufferIO extends NutCoreBundle with HasStoreBufferConst {
  val in = Flipped(Decoupled(new StoreBufferEntry))
  val out = Decoupled(new StoreBufferEntry)
  val writePtr = Output(UInt((log2Up(StoreBufferSize)+1).W))
  val readPtr = Output(UInt((log2Up(StoreBufferSize)+1).W))
  val isFull = Output(Bool())     // StoreBufferSize - 2, for store inst in pipeline stage 4, 5
  val isEmpty = Output(Bool())
  val snapshot = Vec(StoreBufferSize, new StoreBufferEntry)
}

class StoreBuffer extends NutCoreModule with HasStoreBufferConst{
  val io = IO(new StoreBufferIO)

  //Ready, Valid & Fire
  io.in.ready := !io.isFull
  io.out.valid := !io.isEmpty
  val writeFire = Wire(Bool())
  val readFire = Wire(Bool())
  writeFire := io.in.valid && !io.isFull
  readFire := io.out.fire()
  //StoreBuffer Memory
  val StoreBuffer = RegInit(VecInit(Seq.fill(8)(0.U.asTypeOf( new StoreBufferEntry))))
  //merge reference signal
  val merge = Wire(Bool())
  val mergeHit = WireInit(VecInit(Seq.fill(StoreBufferSize)(false.B)))
  val mergeAddr = WireInit(0.U(log2Up(StoreBufferSize).W))
  val addrIndex = Wire(Vec(StoreBufferSize,UInt((log2Up(StoreBufferSize).W))))
  for(i <- 0 to StoreBufferSize-1){addrIndex(i) := i.U}
  val mergeData = WireInit(0.U(XLEN.W))
  val mergeMask = WireInit(0.U((XLEN/8).W))
  val FinalwriteAddr = WireInit(0.U(log2Up(StoreBufferSize).W))
  val writeData = WireInit(0.U(XLEN.W))
  val writeMask = WireInit(0.U((XLEN/8).W))
  val SBDataVec = Wire(Vec(StoreBufferSize,UInt(XLEN.W)))
  val SBMaskVec = Wire(Vec(StoreBufferSize,UInt((XLEN/8).W)))

  when(writeFire && !io.isFull){
    StoreBuffer(FinalwriteAddr).data := writeData
    StoreBuffer(FinalwriteAddr).paddr:= io.in.bits.paddr
    StoreBuffer(FinalwriteAddr).mask := writeMask
    StoreBuffer(FinalwriteAddr).size := io.in.bits.size

  }
  //Pointer Counter
  val writeAddr = RegInit(0.U(log2Up(StoreBufferSize).W))
  val writeFlag = RegInit(0.U(1.W))
  val readAddr = RegInit(0.U(log2Up(StoreBufferSize).W))
  val readFlag = RegInit(0.U(1.W))
  when(writeFire && !merge || merge && mergeHit(readAddr)) {
    when(writeAddr =/= readAddr) {
      writeFlag := (Cat(writeFlag, writeAddr) + 1.U) (log2Up(StoreBufferSize))
      writeAddr := (Cat(writeFlag, writeAddr) + 1.U) (log2Up(StoreBufferSize) - 1, 0)
    }.elsewhen(writeFlag === readFlag) {
      writeFlag := (Cat(writeFlag, writeAddr) + 1.U) (log2Up(StoreBufferSize))
      writeAddr := (Cat(writeFlag, writeAddr) + 1.U) (log2Up(StoreBufferSize) - 1, 0)
    }
  }
  when(readFire) {
    when(writeAddr =/= readAddr) {
      readFlag := (Cat(readFlag, readAddr) + 1.U) (log2Up(StoreBufferSize))
      readAddr := (Cat(readFlag, readAddr) + 1.U) (log2Up(StoreBufferSize) - 1, 0)
    }.elsewhen(writeFlag =/= readFlag) {
      readFlag := (Cat(readFlag, readAddr) + 1.U) (log2Up(StoreBufferSize))
      readAddr := (Cat(readFlag, readAddr) + 1.U) (log2Up(StoreBufferSize) - 1, 0)
    }
  }
  //Flag Logic
  io.isFull := writeAddr === readAddr && writeFlag =/= readFlag
  io.isEmpty := writeAddr === readAddr && writeFlag === readFlag

  //merge check & merge
  for(i <- 0 to StoreBufferSize-1){
    SBDataVec(i) := StoreBuffer(i).data
    SBMaskVec(i) := StoreBuffer(i).mask
  }
  merge := mergeHit.asUInt.orR
  mergeAddr := Mux1H(mergeHit,addrIndex)
  mergeData := MergeData(io.in.bits.data,Mux1H(mergeHit,SBDataVec),io.in.bits.mask,Mux1H(mergeHit,SBMaskVec))
  mergeMask := io.in.bits.mask | Mux1H(mergeHit,SBMaskVec)
  FinalwriteAddr := Mux(merge,mergeAddr,writeAddr)
  writeData := Mux(merge,mergeData,io.in.bits.data)
  writeMask := Mux(merge,mergeMask,io.in.bits.mask)

  for(i <- 0 to 2*StoreBufferSize-1){
    when(i.U < io.writePtr && i.U >= io.readPtr && !readFire){
      mergeHit(i.U(log2Up(StoreBufferSize)-1,0)) := StoreBuffer(i.U(log2Up(StoreBufferSize)-1,0)).paddr(StoreBufferSize-1,3) === io.in.bits.paddr(StoreBufferSize-1,3) && writeFire
    }.elsewhen(i.U < io.writePtr && i.U > io.readPtr && readFire){
      mergeHit(i.U(log2Up(StoreBufferSize)-1,0)) := StoreBuffer(i.U(log2Up(StoreBufferSize)-1,0)).paddr(StoreBufferSize-1,3) === io.in.bits.paddr(StoreBufferSize-1,3) && writeFire
    }.otherwise{
      mergeHit(i.U(log2Up(StoreBufferSize)-1,0)) := false.B
    }
  }

  //output
  io.writePtr := Cat(writeFlag,writeAddr)
  io.readPtr := Cat(readFlag,readAddr)
  io.snapshot := StoreBuffer
  io.out.bits := StoreBuffer(readAddr)
  //for debug
  val SBCounter = WireInit(0.U((log2Up(StoreBufferSize)+1).W))
  when(writeFlag === readFlag){SBCounter := Cat(0.U(1.W),writeAddr) - Cat(0.U(1.W),readAddr)}
    .otherwise{SBCounter := Cat(1.U(1.W),writeAddr) - Cat(0.U(1.W),readAddr)}
  dontTouch(SBCounter)

}

class StoreBuffer_fake extends NutCoreModule with HasStoreBufferConst {
  val io = IO(new StoreBufferIO)
  io.in <> io.out
  io.writePtr := 0.U((log2Up(StoreBufferSize)+1).W)
  io.readPtr := 0.U((log2Up(StoreBufferSize)+1).W)
  io.isFull := false.B
  io.isEmpty := false.B
  io.snapshot := VecInit(Seq.fill(StoreBufferSize)(0.U.asTypeOf(new StoreBufferEntry)))
}
object MergeData{
  def apply(data: UInt, beMergedData: UInt, dataMask: UInt, beMergedDataMask: UInt): UInt ={
    data & MaskExpand(dataMask) | beMergedData & MaskExpand(beMergedDataMask) & ~MaskExpand(dataMask)
  }
}