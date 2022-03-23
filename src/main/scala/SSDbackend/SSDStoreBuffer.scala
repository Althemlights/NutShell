package SSDbackend

import bus.simplebus.SimpleBusCmd
import chisel3._
import nutcore._
import chisel3.util._

trait HasStoreBufferConst{
  val StoreBufferSize = 8
}

class StoreBufferEntry extends NutCoreBundle{
  val cmd      = Output(SimpleBusCmd())
  val paddr    = Output(UInt(PAddrBits.W))
  val data     = Output(UInt(XLEN.W))
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
  val snapshot = Output(Vec(StoreBufferSize, new StoreBufferEntry))
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
  when(writeFire && !io.isFull){ StoreBuffer(writeFire.asUInt) := io.in.bits}
  io.out.bits := Mux(!io.isEmpty && readFire,StoreBuffer(readFire.asUInt),0.U.asTypeOf(new StoreBufferEntry))
  //Pointer Counter
  val writeAddr = RegInit(0.U(log2Up(StoreBufferSize).W))
  val writeFlag = RegInit(0.U(1.W))
  val readAddr = RegInit(0.U(log2Up(StoreBufferSize).W))
  val readFlag = RegInit(0.U(1.W))
  when(writeFire){
    when(writeAddr =/= readAddr){ Cat(writeFlag,writeAddr) := Cat(writeFlag,writeAddr) + 1.U
    }.elsewhen(writeFlag === readFlag){ Cat(writeFlag,writeAddr) := Cat(writeFlag,writeAddr) + 1.U }
  }
  when(readFire){
    when(writeAddr =/= readAddr){ Cat(readFlag,readAddr) := Cat(readFlag,readAddr) + 1.U
    }.elsewhen(writeFlag =/= readFlag){ Cat(readFlag,readAddr) := Cat(readFlag,readAddr) + 1.U }
  }
  //Flag Logic
  io.isFull := writeAddr === readAddr - 1.U && writeFlag =/= readFlag
  io.isEmpty := writeAddr === readAddr && writeFlag === readFlag

  //snapshot
  val snapshotReg = Reg(VecInit(Seq.fill(StoreBufferSize)(0.U.asTypeOf(new StoreBufferEntry))))
  when(io.snapshotena) { snapshotReg := StoreBuffer }
  //output
  io.writePtr := Cat(writeFlag,writeAddr)
  io.readPtr := Cat(readFlag,readAddr)

}