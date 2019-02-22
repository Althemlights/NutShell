package noop

import chisel3._
import chisel3.util._

trait HasMDUOpType {
  val MduOpTypeNum  = 8

  def MduMul  = "b000".U
  def MduMulh = "b001".U
  def MduDiv  = "b100".U
  def MduDivu = "b101".U
  def MduRem  = "b110".U
  def MduRemu = "b111".U
}

object MDUInstr extends HasDecodeConst {
  def MUL     = BitPat("b0000001_?????_?????_000_?????_0110011")
  def MULH    = BitPat("b0000001_?????_?????_001_?????_0110011")
  def DIV     = BitPat("b0000001_?????_?????_100_?????_0110011")
  def DIVU    = BitPat("b0000001_?????_?????_101_?????_0110011")
  def REM     = BitPat("b0000001_?????_?????_110_?????_0110011")
  def REMU    = BitPat("b0000001_?????_?????_111_?????_0110011")

  val table = Array(
    MUL            -> List(InstrR, FuMdu, MduMul),
    MULH           -> List(InstrR, FuMdu, MduMulh),
    DIV            -> List(InstrR, FuMdu, MduDiv),
    DIVU           -> List(InstrR, FuMdu, MduDivu),
    REM            -> List(InstrR, FuMdu, MduRem),
    REMU           -> List(InstrR, FuMdu, MduRemu)
  )
}

class MDU extends HasMDUOpType {
  def access(src1: UInt, src2: UInt, func: UInt): UInt = {
    0.U
    /*
    val mulRes = (src1.asSInt * src2.asSInt).asUInt
    LookupTree(func, 0.U, List(
      MduMul  -> mulRes(31, 0),
      MduMulh -> mulRes(63, 32),
      MduDiv  -> (src1.asSInt  /  src2.asSInt).asUInt,
      MduDivu -> (src1  /  src2),
      MduRem  -> (src1.asSInt  %  src2.asSInt).asUInt,
      MduRemu -> (src1  %  src2)
    ))
    */
  }
}
