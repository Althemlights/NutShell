package SSDbackend

import bus.simplebus.SimpleBusCmd
import chisel3._
import nutcore._
import chisel3.util._
import utils._

trait HasStoreBufferConst{
  val StoreBufferSize = 16
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
  val isAlmostFull = Output(Bool())
  val isFull = Output(Bool())
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
  readFire := io.out.fire
  //StoreBuffer Memory
  val StoreBuffer = RegInit(VecInit(Seq.fill(StoreBufferSize)(0.U.asTypeOf( new StoreBufferEntry))))
  //merge reference signal
  val merge = Wire(Bool())
  val mergedIsReading = Wire(Bool())
  val mergeHit = WireInit(VecInit(Seq.fill(2*StoreBufferSize)(false.B)))
  val mergeAddr = WireInit(0.U(log2Up(StoreBufferSize).W))
  val addrIndex = Wire(Vec(2*StoreBufferSize,UInt(((log2Up(StoreBufferSize)+1).W))))
  for(i <- 0 to 2*StoreBufferSize-1){addrIndex(i) := i.U}
  val mergeData = WireInit(0.U(XLEN.W))
  val mergeMask = WireInit(0.U((XLEN/8).W))
  val FinalwriteAddr = WireInit(0.U(log2Up(StoreBufferSize).W))
  val writeData = WireInit(0.U(XLEN.W))
  val writeMask = WireInit(0.U((XLEN/8).W))
  val SBDataVec = Wire(Vec(2*StoreBufferSize,UInt(XLEN.W)))
  val SBMaskVec = Wire(Vec(2*StoreBufferSize,UInt((XLEN/8).W)))

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
  when(writeFire && (!merge || merge && mergedIsReading)) {
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
  io.isAlmostFull := (writeAddr === readAddr - 1.U) && writeFlag =/= readFlag
  io.isFull := writeAddr === readAddr && writeFlag =/= readFlag
  io.isEmpty := writeAddr === readAddr && writeFlag === readFlag

  //merge check & merge
  for(i <- 0 to 2*StoreBufferSize-1){
    SBDataVec(i) := StoreBuffer(i.U(log2Up(StoreBufferSize)-1,0)).data
    SBMaskVec(i) := StoreBuffer(i.U(log2Up(StoreBufferSize)-1,0)).mask
  }
  val writeAddrExpand = Mux(readFlag =/= writeFlag,Cat(1.U(1.W),writeAddr),Cat(0.U(1.W),writeAddr))
  val readAddrExpand = Cat(0.U(1.W),readAddr)
  val AddrExpand = Wire(Vec(2*StoreBufferSize,UInt(PAddrBits.W)))

  for(i <- 0 to StoreBufferSize*2-1){ AddrExpand(i) := StoreBuffer(i.U(log2Up(StoreBufferSize)-1,0)).paddr}
//  for(i <- 0 to StoreBufferSize*2-1){
//    when(i.U < writeAddrExpand.asUInt && i.U >= readAddrExpand.asUInt && !readFire){
//      mergeHit(i) := (AddrExpand(i).paddr(PAddrBits-1,3) === io.in.bits.paddr(PAddrBits-1,3)) && writeFire
//    }.elsewhen(i.U < writeAddrExpand && i.U > readAddrExpand && readFire){
//      mergeHit(i) := (AddrExpand(i).paddr(PAddrBits-1,3) === io.in.bits.paddr(PAddrBits-1,3)) && writeFire
//    }.otherwise{
//      mergeHit(i) := false.B
//    }
//  }
  def mergeCheck(headAddr:UInt, tailAddr:UInt, AddrExpand:Vec[UInt], AddrIn:UInt):Vec[Bool] ={
  val mergeHitVec = WireInit(VecInit(Seq.fill(2*StoreBufferSize)(false.B)))
  for(i <- 0 to StoreBufferSize*2-1){
    when(i.U < headAddr && i.U >= tailAddr){
      mergeHitVec(i) := AddrExpand(i)(PAddrBits-1,3) === AddrIn(PAddrBits-1,3)
    }.otherwise{
      mergeHitVec(i) := false.B
    }
  }
  mergeHitVec
}
  mergeHit := mergeCheck(writeAddrExpand,readAddrExpand,AddrExpand,io.in.bits.paddr)
  merge := mergeHit.asUInt.orR
  mergedIsReading := readFire && (mergeHit(Cat(0.U(1.W),readAddr)) || mergeHit(Cat(1.U(1.W),readAddr)))
  mergeAddr := Mux1H(mergeHit,addrIndex)(log2Up(StoreBufferSize)-1,0)
  mergeData := MergeData(io.in.bits.data,Mux1H(mergeHit,SBDataVec),io.in.bits.mask,Mux1H(mergeHit,SBMaskVec))
  mergeMask := io.in.bits.mask | Mux1H(mergeHit,SBMaskVec)
  FinalwriteAddr := Mux(merge && !mergedIsReading,mergeAddr,writeAddr)
  writeData := Mux(merge && !mergedIsReading,mergeData,io.in.bits.data)
  writeMask := Mux(merge && !mergedIsReading,mergeMask,io.in.bits.mask)
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