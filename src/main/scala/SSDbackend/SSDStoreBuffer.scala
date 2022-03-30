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

class StoreBufferIO extends NutCoreBundle with HasStoreBufferConst {
  val in = Flipped(Decoupled(new StoreBufferEntry))
  val out = Decoupled(new StoreBufferEntry)
  val writePtr = Output(UInt((log2Up(StoreBufferSize)+1).W))
  val readPtr = Output(UInt((log2Up(StoreBufferSize)+1).W))
  val isFull = Output(Bool())     // StoreBufferSize - 2, for store inst in pipeline stage 4, 5
  val isEmpty = Output(Bool())
  val snapshotena = Input(Bool())
  val snapshot = Vec(StoreBufferSize, new StoreBufferEntry)
}

class StoreBuffer extends NutCoreModule with HasStoreBufferConst{
  val io = new StoreBufferIO

  //Ready, Valid & Fire
  io.in.ready := !io.isFull
  io.out.valid := !io.isEmpty
  val writeFire = Wire(Bool())
  val readFire = Wire(Bool())
  writeFire := io.in.fire()
  readFire := io.out.fire()
  //StoreBuffer Memory
  val StoreBuffer = Reg(VecInit(Seq.fill(StoreBufferSize)(0.U.asTypeOf(new StoreBufferEntry))))
  when(writeFire && !io.isFull){
    StoreBuffer(FinalwriteAddr).data := writeData
    StoreBuffer(FinalwriteAddr).paddr:= io.in.bits.paddr
    StoreBuffer(FinalwriteAddr).mask := writeMask
    StoreBuffer(FinalwriteAddr).data := io.in.bits.size

  }
  io.out.bits := Mux(!io.isEmpty && readFire,StoreBuffer(readFire.asUInt),0.U.asTypeOf(new StoreBufferEntry))
  //Pointer Counter
  val writeAddr = RegInit(0.U(log2Up(StoreBufferSize).W))
  val writeFlag = RegInit(0.U(1.W))
  val readAddr = RegInit(0.U(log2Up(StoreBufferSize).W))
  val readFlag = RegInit(0.U(1.W))
  when(writeFire && !merge || merge && mergeHit(readAddr)){
    when(writeAddr =/= readAddr){ Cat(writeFlag,writeAddr) := Cat(writeFlag,writeAddr) + 1.U
    }.elsewhen(writeFlag === readFlag){ Cat(writeFlag,writeAddr) := Cat(writeFlag,writeAddr) + 1.U }
  }
  when(readFire){
    when(writeAddr =/= readAddr){ Cat(readFlag,readAddr) := Cat(readFlag,readAddr) + 1.U
    }.elsewhen(writeFlag =/= readFlag){ Cat(readFlag,readAddr) := Cat(readFlag,readAddr) + 1.U }
  }
  //Flag Logic
  io.isFull := writeAddr === readAddr && writeFlag =/= readFlag
  io.isEmpty := writeAddr === readAddr && writeFlag === readFlag

  //merge check & merge
  val merge = Wire(Bool())
  val mergeHit = Wire(Vec(StoreBufferSize,false.B))
  val mergeAddr = Wire(0.U(log2Up(StoreBufferSize).W))
  val mergeData = Wire(0.U(XLEN.W))
  val mergeMask = Wire(0.U((XLEN/8).W))
  val FinalwriteAddr = Wire(0.U(log2Up(StoreBufferSize).W))
  val writeData = Wire(0.U(XLEN.W))
  val writeMask = Wire(0.U((XLEN/8).W))
  val SBDataVec = Wire(Vec(StoreBufferSize,0.U(XLEN.W)))
  val SBMaskVec = Wire(Vec(StoreBufferSize,0.U((XLEN/8).W)))
  for(i <- 0 to StoreBufferSize-1){
    SBDataVec(i) := StoreBuffer(i).data
    SBMaskVec(i) := StoreBuffer(i).mask
  }
  merge := mergeHit.asUInt.orR
  mergeAddr := Mux1H(mergeHit,Vec(0 to StoreBufferSize-1))
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
    }.otherwise(){
      mergeHit(i.U(log2Up(StoreBufferSize)-1,0)) := false.B
    }
  }
  //snapshot
  val snapshotReg = Reg(VecInit(Seq.fill(StoreBufferSize)(0.U.asTypeOf(new StoreBufferEntry))))
  when(io.snapshotena) { snapshotReg := StoreBuffer }
  //output
  io.writePtr := Cat(writeFlag,writeAddr)
  io.readPtr := Cat(readFlag,readAddr)

}

object MergeData{
  def applay(data: UInt, beMergedData: UInt, dataMask: UInt, beMergedDataMask: UInt)={
    data & MaskExpand(dataMask) | beMergedData & MaskExpand(beMergedDataMask) & ~dataMask
  }
}