package SSDbackend

import chisel3._
import nutcore._
import chisel3.util._
import chiseltest.simulator.WriteFstAnnotation

trait HasStoreBufferConst{
  val StoreBufferSize = 8
}

class StoreBufferEntry extends NutCoreBundle{
  val valid    = Bool()
  val wmask    = UInt((XLEN/8).W)
  val paddr    = UInt(PAddrBits.W)
  val data     = UInt(XLEN.W)
  val isMMIO   = Bool()
}

class StoreBufferIO extends NutCoreBundle with HasStoreBufferConst {
  val in = Flipped(Decoupled(Output(new StoreBufferEntry)))
  val out = Decoupled(Output(new StoreBufferEntry))
  val isFull = Output(Bool())     // StoreBufferSize - 2, for store inst in pipeline stage 4, 5
  val isEmpty = Output(Bool())
  val snapshotena = Input(Bool())
  val snapshot = Output(Vec(StoreBufferSize, new StoreBufferEntry))
}

class StoreBuffer extends NutCoreModule{
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
  io.out.bits := Mux(!io.isEmpty && readFire,toreBuffer(readFire.asUInt),0.U.asTypeOf(new StoreBufferEntry))
  //Pointer Counter
  val writeAddr = RegInit(0.U(log2Up(StoreBufferSize).W))
  val writeFlag = RegInit(0.U(1.W))
  val readAddr = RegInit(0.U(log2Up(StoreBufferSize).W))
  val readFlag = RegInit(0.U(1.W))
  when(writeFire){
    when(writeAddr =/= readAddr){ Cat(writeFlag,writeAddr) := Cat(writeFlag,writeAddr) + 1
    }.elsewhen(writeFlag === readFlag){ Cat(writeFlag,writeAddr) := Cat(writeFlag,writeAddr) + 1 }
  }
  when(readFire){
    when(writeAddr =/= readAddr){ Cat(readFlag,readAddr) := Cat(readFlag,readAddr) + 1
    }.elsewhen(writeFlag =/= readFlag){ Cat(readFlag,readAddr) := Cat(readFlag,readAddr) + 1 }
  }
  //Flag Logic
  io.isFull := writeAddr === readAddr - 2 && writeFlag =/= readFlag
  io.isEmpty := writeAddr === readAddr && writeFlag === readFlag

  //snapshot
  val snapshotReg = Reg(VecInit(Seq.fill(StoreBufferSize)(0.U.asTypeOf(new StoreBufferEntry))))
  when(io.snapshotena) { snapshotReg := StoreBuffer }

}