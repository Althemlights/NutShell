package SSDbackend

import chisel3._
import chisel3.util._
import nutcore.NutCoreModule
import chisel3.util.experimental.BoringUtils

class RfReadPort extends Bundle{
  val addr = Input(UInt(5.W))
  val data = Output(UInt(64.W))
}

class RfWritePort extends Bundle{
  val wen = Input(Bool())
  val addr = Input(UInt(5.W))
  val data = Input(UInt(64.W))
}

class SSDRF extends Module{
  val io =IO(new Bundle() {
    val readPorts = Vec(4,new RfReadPort)
    val writePorts = Vec(2,new RfWritePort)
    val debugPorts = Output(Vec(32,UInt(64.W)))
    val mem = Output(Vec(32,UInt(63.W)))
  })

  val mem = Reg(Vec(32,UInt(64.W)))
  io.mem := mem

  for (r <- io.readPorts) {
    val rdata = Mux(r.addr === 0.U, 0.U, mem(r.addr))
    r.data := rdata
  }
  val writeHit = Wire(Bool())
  writeHit := io.writePorts(0).wen && io.writePorts(1).wen && io.writePorts(0).addr === io.writePorts(1).addr
  when(!writeHit) {
    for (w <- io.writePorts) {
      when(w.wen) {
        mem(w.addr) := w.data
      }
    }
  }.otherwise{
    mem(io.writePorts(0).addr) := io.writePorts(0).data
  }
  BoringUtils.addSource(mem(10), "rf_a0")
  for (i <- 0 to 31){
    io.debugPorts(i) := Mux(i.U === 0.U, 0.U, mem(i.U))
  }
}



/*class SSDRF extends NutCoreModule {
  val io = IO(new RfIO)

  val Regfile = Mem(32, UInt(64.W))
  def read(addr: UInt) : UInt = Mux(addr === 0.U, 0.U, Regfile(addr))
  //读写冲突
  val rwhit = Wire(Vec(4, UInt(2.W)))
  val rwhitData = Wire(Vec(4, UInt(64.W)))
  val rwhitTag = Wire(Vec(4, Bool()))
  for (i <- 0 to 3) rwhitTag(i) := rwhit(i).orR

  for (i <- 0 to 3) {
    rwhit(i) := {
      (io.raddr(i) == io.waddr(0), io.raddr(i) == io.waddr(1)) match {
        case (false, false) => "b00".U
        case (false, true) => "b01".U
        case (true, false) => "b10".U
        case (true, true) => "b11".U
      }
    }
  }
  val rwhitTable = Array(BitPat("b00".U) -> 0.U(64.W), BitPat("b01".U) -> io.wdata(1), BitPat("b10".U) -> io.wdata(0), BitPat("b11".U) -> io.wdata(1))

  for (i <- 0 to 3) rwhitData(i) := Lookup(rwhit(i), 0.U(64.W), rwhitTable)
  // read
  for(i <- 0 to 3) io.rdata(i) := Mux(rwhitTag(i),rwhitData(i),Regfile.read(io.raddr(i)))

  //写冲突
  val wwhit = Wire(Bool())
  wwhit := io.waddr(0) === io.waddr(1) && io.wen(0) && io.wen(1)
  //write
  Regfile.write(io.waddr(0),io.wen(0))
  Regfile.write(io.waddr(1),io.wen(1) && !wwhit)
}*/
