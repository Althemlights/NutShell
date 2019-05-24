package noop

import chisel3._
import chisel3.util._

import utils._

class IDU extends Module with HasDecodeConst {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new PcInstrIO))
    val out = Decoupled(new PcCtrlDataIO)
  })

  val instr = io.in.bits.instr
  val instrType :: fuType :: fuOpType :: Nil =
    ListLookup(instr, Instructions.DecodeDefault, Instructions.DecodeTable)

  io.out.bits := DontCare

  io.out.bits.ctrl.fuType := fuType
  io.out.bits.ctrl.fuOpType := fuOpType

  val SrcTypeTable = List(
    InstrI -> (Src1Reg, Src2Imm),
    InstrR -> (Src1Reg, Src2Reg),
    InstrS -> (Src1Reg, Src2Reg),
    InstrB -> (Src1Reg, Src2Reg),
    InstrU -> (Src1Pc , Src2Imm),
    InstrJ -> (Src1Pc , Src2Imm),
    InstrN -> (Src1Pc , Src2Imm)
  )
  io.out.bits.ctrl.src1Type := LookupTree(instrType, SrcTypeTable.map(p => (p._1, p._2._1)))
  io.out.bits.ctrl.src2Type := LookupTree(instrType, SrcTypeTable.map(p => (p._1, p._2._2)))

  io.out.bits.ctrl.rfSrc1 := instr(19, 15)
  io.out.bits.ctrl.rfSrc2 := instr(24, 20)
  io.out.bits.ctrl.rfWen := isrfWen(instrType)
  io.out.bits.ctrl.rfDest := instr(11, 7)

  io.out.bits.data := DontCare
  io.out.bits.data.imm  := LookupTree(instrType, List(
    InstrI -> Cat(Fill(20, instr(31)), instr(31, 20)),
    InstrS -> Cat(Fill(20, instr(31)), instr(31, 25), instr(11, 7)),
    InstrB -> Cat(Fill(20, instr(31)), instr(7), instr(30, 25), instr(11, 8), 0.U(1.W)),
    InstrU -> Cat(instr(31, 12), 0.U(12.W)),
    InstrJ -> Cat(Fill(12, instr(31)), instr(19, 12), instr(20), instr(30, 21), 0.U(1.W))
  ))

  io.out.bits.pc := io.in.bits.pc

  io.out.bits.ctrl.isInvOpcode := (instrType === InstrN) && io.in.valid
  io.out.bits.ctrl.isNoopTrap := (instr === NOOPTrap.TRAP) && io.in.valid
  io.out.valid := io.in.valid

  io.in.ready := !io.in.valid || io.out.fire()
}
