package SSDbackend

import chisel3._
import chisel3.util._
import nutcore._

class ModuleTest extends Module{
  val io = IO(new Bundle() {
    val out = Output(UInt(3.W))
  })
  io.out := DontCare

  //multiplier test
  val multplierTest = Module(new ArrayMultiplier(65))
  val cnt = Reg(UInt(3.W))
  cnt := cnt + 1.U
  multplierTest.io.in.valid := cnt === 5.U(3.W)
  multplierTest.io.in.bits.src(0) := 2.U(64.W)
  multplierTest.io.in.bits.src(1) := 2.U(64.W)
  multplierTest.io.in.bits.src(2) := DontCare
  multplierTest.io.out.ready := true.B
  multplierTest.ctrl.sign := true.B
  multplierTest.ctrl.isW := true.B
  multplierTest.ctrl.isHi := false.B
  dontTouch(multplierTest.io.in)
  dontTouch(multplierTest.io.out)

  //SSDMDU test
  val mdu = Module(new SSDMDU)
  val cnt1 = RegInit(0.U(2.W))
  cnt1 := cnt1 + 1.U
  mdu.io.out.ready := true.B
  val mduValid = true.B
  val mduSrc1 = cnt1
  val mduSrc2 = cnt1
  val mduFunc = 4.U
  mdu.access(mduValid,mduSrc1,mduSrc2,mduFunc)
  dontTouch(mdu.io.in)
  dontTouch(mdu.io.out)
}