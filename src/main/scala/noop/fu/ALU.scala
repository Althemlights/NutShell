package noop

import chisel3._
import chisel3.util._

import utils._

trait HasALUOpType {
  val AluOpTypeNum  = 11

  def AluAdd  = "b0000".U
  def AluSll  = "b0001".U
  def AluSlt  = "b0010".U
  def AluSltu = "b0011".U
  def AluXor  = "b0100".U
  def AluSrl  = "b0101".U
  def AluOr   = "b0110".U
  def AluAnd  = "b0111".U
  def AluSub  = "b1000".U
  def AluSra  = "b1101".U
  def AluLui  = "b1111".U
}

object ALUInstr extends HasDecodeConst {
  def ADDI    = BitPat("b????????????_?????_000_?????_0010011")
  def SLLI    = BitPat("b0000000?????_?????_001_?????_0010011")
  def SLTI    = BitPat("b????????????_?????_010_?????_0010011")
  def SLTIU   = BitPat("b????????????_?????_011_?????_0010011")
  def XORI    = BitPat("b????????????_?????_100_?????_0010011")
  def SRLI    = BitPat("b0000000?????_?????_101_?????_0010011")
  def ORI     = BitPat("b????????????_?????_110_?????_0010011")
  def ANDI    = BitPat("b????????????_?????_111_?????_0010011")
  def SRAI    = BitPat("b0100000?????_?????_101_?????_0010011")

  def ADD     = BitPat("b0000000_?????_?????_000_?????_0110011")
  def SLL     = BitPat("b0000000_?????_?????_001_?????_0110011")
  def SLT     = BitPat("b0000000_?????_?????_010_?????_0110011")
  def SLTU    = BitPat("b0000000_?????_?????_011_?????_0110011")
  def XOR     = BitPat("b0000000_?????_?????_100_?????_0110011")
  def SRL     = BitPat("b0000000_?????_?????_101_?????_0110011")
  def OR      = BitPat("b0000000_?????_?????_110_?????_0110011")
  def AND     = BitPat("b0000000_?????_?????_111_?????_0110011")
  def SUB     = BitPat("b0100000_?????_?????_000_?????_0110011")
  def SRA     = BitPat("b0100000_?????_?????_101_?????_0110011")

  def AUIPC   = BitPat("b????????????????????_?????_0010111")
  def LUI     = BitPat("b????????????????????_?????_0110111")

  val table = Array(
    ADDI           -> List(InstrI, FuAlu, AluAdd),
    SLLI           -> List(InstrI, FuAlu, AluSll),
    SLTI           -> List(InstrI, FuAlu, AluSlt),
    SLTIU          -> List(InstrI, FuAlu, AluSltu),
    XORI           -> List(InstrI, FuAlu, AluXor),
    SRLI           -> List(InstrI, FuAlu, AluSrl),
    ORI            -> List(InstrI, FuAlu, AluOr ),
    ANDI           -> List(InstrI, FuAlu, AluAnd),
    SRAI           -> List(InstrI, FuAlu, AluSra),

    ADD            -> List(InstrR, FuAlu, AluAdd),
    SLL            -> List(InstrR, FuAlu, AluSll),
    SLT            -> List(InstrR, FuAlu, AluSlt),
    SLTU           -> List(InstrR, FuAlu, AluSltu),
    XOR            -> List(InstrR, FuAlu, AluXor),
    SRL            -> List(InstrR, FuAlu, AluSrl),
    OR             -> List(InstrR, FuAlu, AluOr ),
    AND            -> List(InstrR, FuAlu, AluAnd),
    SUB            -> List(InstrR, FuAlu, AluSub),
    SRA            -> List(InstrR, FuAlu, AluSra),

    AUIPC          -> List(InstrU, FuAlu, AluAdd),
    LUI            -> List(InstrU, FuAlu, AluLui)
  )
}

class ALU extends Module with HasALUOpType {
  val io = IO(new FunctionUnitIO)

  val (valid, src1, src2, func) = (io.in.valid, io.in.bits.src1, io.in.bits.src2, io.in.bits.func)
  def access(valid: Bool, src1: UInt, src2: UInt, func: UInt): UInt = {
    this.valid := valid
    this.src1 := src1
    this.src2 := src2
    this.func := func
    io.out.bits
  }

  val isAdderSub = (func =/= AluAdd)
  val adderRes = (src1 +& (src2 ^ Fill(32, isAdderSub))) + isAdderSub
  val xorRes = src1 ^ src2
  val sltu = !adderRes(32)
  val slt = xorRes(31) ^ sltu

  val shamt = src2(4, 0)
  io.out.bits := LookupTree(func, 0.U, List(
    AluAdd  -> adderRes,
    AluSll  -> ((src1  << shamt)(31, 0)),
    AluSlt  -> Cat(0.U(31.W), slt),
    AluSltu -> Cat(0.U(31.W), sltu),
    AluXor  -> xorRes,
    AluSrl  -> (src1  >> shamt),
    AluOr   -> (src1  |  src2),
    AluAnd  -> (src1  &  src2),
    AluSub  -> adderRes,
    AluLui  -> src2,
    AluSra  -> ((src1.asSInt >> shamt).asUInt)
  ))

  io.in.ready := true.B
  io.out.valid := valid
}
