package noop

import chisel3._
import chisel3.util._

import memory.MemIO

trait HasLSUOpType {
  val LsuOpTypeNum  = 10

  def LsuLb   = "b0000".U
  def LsuLh   = "b0001".U
  def LsuLw   = "b0010".U
  def LsuLbu  = "b0100".U
  def LsuLhu  = "b0101".U
  def LsuSb   = "b1000".U
  def LsuSh   = "b1001".U
  def LsuSw   = "b1010".U
}

object LSUInstr extends HasDecodeConst {
  def LB      = BitPat("b????????????_?????_000_?????_0000011")
  def LH      = BitPat("b????????????_?????_001_?????_0000011")
  def LW      = BitPat("b????????????_?????_010_?????_0000011")
  def LBU     = BitPat("b????????????_?????_100_?????_0000011")
  def LHU     = BitPat("b????????????_?????_101_?????_0000011")
  def SB      = BitPat("b???????_?????_?????_000_?????_0100011")
  def SH      = BitPat("b???????_?????_?????_001_?????_0100011")
  def SW      = BitPat("b???????_?????_?????_010_?????_0100011")

  val table = Array(
    LB             -> List(InstrI, FuLsu, LsuLb ),
    LH             -> List(InstrI, FuLsu, LsuLh ),
    LW             -> List(InstrI, FuLsu, LsuLw ),
    LBU            -> List(InstrI, FuLsu, LsuLbu),
    LHU            -> List(InstrI, FuLsu, LsuLhu),
    SB             -> List(InstrS, FuLsu, LsuSb ),
    SH             -> List(InstrS, FuLsu, LsuSh ),
    SW             -> List(InstrS, FuLsu, LsuSw)
  )
}

class LSU extends HasLSUOpType {
  def genWmask(addr: UInt, sizeEncode: UInt): UInt = {
    LookupTree(sizeEncode, List(
      "b00".U -> 0x1.U,
      "b01".U -> 0x3.U,
      "b10".U -> 0xf.U
    )) << addr(1, 0)
  }
  def genWdata(data: UInt, sizeEncode: UInt): UInt = {
    LookupTree(sizeEncode, List(
      "b00".U -> Fill(4, data(7, 0)),
      "b01".U -> Fill(2, data(15, 0)),
      "b10".U -> data
    ))
  }
  def access(isLsu: Bool, base: UInt, offset: UInt, func: UInt, wdata: UInt): (MemIO, Bool) = {
    val dmem = Wire(new MemIO)
    val s_idle :: s_wait_resp :: Nil = Enum(2)
    val state = RegInit(s_idle)

    switch (state) {
      is (s_idle) {
        when (dmem.a.fire()) { state := Mux(dmem.w.valid || dmem.r.fire(), s_idle, s_wait_resp) }
      }

      is (s_wait_resp) {
        when (dmem.r.fire()) { state := s_idle }
      }
    }

    dmem.a.bits.addr := base + offset
    dmem.a.bits.size := func(1, 0)
    dmem.a.valid := isLsu && (state === s_idle)
    dmem.w.valid := isLsu && func(3)
    dmem.w.bits.data := genWdata(wdata, func(1, 0))
    dmem.w.bits.mask := genWmask(base + offset, func(1, 0))
    dmem.r.ready := true.B

    (dmem, Mux(dmem.w.valid, dmem.a.fire(), dmem.r.fire()))
  }
  def rdataExt(rdataFromBus: UInt, addr: UInt, func: UInt): UInt = {
    val rdata = LookupTree(addr(1, 0), List(
      "b00".U -> rdataFromBus,
      "b01".U -> rdataFromBus(15, 8),
      "b10".U -> rdataFromBus(31, 16),
      "b11".U -> rdataFromBus(31, 24)
    ))
    LookupTree(func, List(
      LsuLb   -> Cat(Fill(24, rdata(7)), rdata(7, 0)),
      LsuLh   -> Cat(Fill(16, rdata(15)), rdata(15, 0)),
      LsuLw   -> rdata,
      LsuLbu  -> Cat(0.U(24.W), rdata(7, 0)),
      LsuLhu  -> Cat(0.U(16.W), rdata(15, 0))
    ))
  }
}
