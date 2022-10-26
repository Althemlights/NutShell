/**************************************************************************************
* Copyright (c) 2020 Institute of Computing Technology, CAS
* Copyright (c) 2020 University of Chinese Academy of Sciences
* 
* NutShell is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2. 
* You may obtain a copy of Mulan PSL v2 at:
*             http://license.coscl.org.cn/MulanPSL2 
* 
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND, EITHER 
* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT, MERCHANTABILITY OR 
* FIT FOR A PARTICULAR PURPOSE.  
*
* See the Mulan PSL v2 for more details.  
***************************************************************************************/

package nutcore

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._
//import difftest._

class Decoder(implicit val p: NutCoreConfig) extends NutCoreModule with HasInstrType {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new CtrlFlowIO))
    val out = Decoupled(new DecodeIO)
    val isWFI = Output(Bool()) // require NutCoreSim to advance mtime when wfi to reduce the idle time in Linux
    val isBranch = Output(Bool())
  })

  val hasIntr = Wire(Bool())
  val instr = io.in.bits.instr
  val decodeList = ListLookup(instr, Instructions.DecodeDefault, Instructions.DecodeTable)
  val instrType :: fuType :: fuOpType :: Nil = // insert Instructions.DecodeDefault when interrupt comes
    Instructions.DecodeDefault.zip(decodeList).map{case (intr, dec) => Mux(hasIntr || io.in.bits.exceptionVec(instrPageFault) || io.out.bits.cf.exceptionVec(instrAccessFault), intr, dec)}


  io.out.bits := DontCare

  io.out.bits.ctrl.fuType := fuType
  io.out.bits.ctrl.fuOpType := fuOpType

  val SrcTypeTable = List(
    InstrI -> (SrcType.reg, SrcType.imm),
    InstrR -> (SrcType.reg, SrcType.reg),
    InstrS -> (SrcType.reg, SrcType.reg),
    InstrSA-> (SrcType.reg, SrcType.reg),
    InstrB -> (SrcType.reg, SrcType.reg),
    InstrU -> (SrcType.pc , SrcType.imm),
    InstrJ -> (SrcType.pc , SrcType.imm),
    InstrN -> (SrcType.pc , SrcType.imm)
  )
  val src1Type = LookupTree(instrType, SrcTypeTable.map(p => (p._1, p._2._1)))
  val src2Type = LookupTree(instrType, SrcTypeTable.map(p => (p._1, p._2._2)))

  val (rs, rt, rd) = (instr(19, 15), instr(24, 20), instr(11, 7))
  // see riscv-spec vol1, Table 16.1: Compressed 16-bit RVC instruction formats.
  val rs1       = instr(11,7)
  val rs2       = instr(6,2)
  val rvc_shamt = Cat(instr(12),instr(6,2)) 
  // val rdp_rs1p = LookupTree(instr(9,7), RVCRegNumTable)
  // val rdp      = LookupTree(instr(4,2), RVCRegNumTable)

  val rfSrc1 = rs
  val rfSrc2 = rt
  val rfDest = rd
  // TODO: refactor decode logic
  // make non-register addressing to zero, since isu.sb.isBusy(0) === false.B
  io.out.bits.ctrl.rfSrc1 := Mux(src1Type === SrcType.pc, 0.U, rfSrc1)
  io.out.bits.ctrl.rfSrc2 := Mux(src2Type === SrcType.reg, rfSrc2, 0.U)
  io.out.bits.ctrl.rfWen  := isrfWen(instrType)
  io.out.bits.ctrl.rfDest := Mux(isrfWen(instrType), rfDest, 0.U)

  io.out.bits.data := DontCare
  val imm = LookupTree(instrType, List(
    InstrI  -> SignExt(instr(31, 20), XLEN),
    InstrS  -> SignExt(Cat(instr(31, 25), instr(11, 7)), XLEN),
    InstrSA -> SignExt(Cat(instr(31, 25), instr(11, 7)), XLEN),
    InstrB  -> SignExt(Cat(instr(31), instr(7), instr(30, 25), instr(11, 8), 0.U(1.W)), XLEN),
    InstrU  -> SignExt(Cat(instr(31, 12), 0.U(12.W)), XLEN),//fixed
    InstrJ  -> SignExt(Cat(instr(31), instr(19, 12), instr(20), instr(30, 21), 0.U(1.W)), XLEN)
  ))
  io.out.bits.data.imm  := imm

  when (fuType === FuType.bru) {
    def isLink(reg: UInt) = (reg === 1.U || reg === 5.U)
    when (isLink(rfDest) && fuOpType === ALUOpType.jal) { io.out.bits.ctrl.fuOpType := ALUOpType.call }
    when (fuOpType === ALUOpType.jalr) {
      when (isLink(rfSrc1)) { io.out.bits.ctrl.fuOpType := ALUOpType.ret }
      when (isLink(rfDest)) { io.out.bits.ctrl.fuOpType := ALUOpType.call }
    }
  }
  // fix LUI
  io.out.bits.ctrl.src1Type := Mux(instr(6,0) === "b0110111".U, SrcType.reg, src1Type)
  io.out.bits.ctrl.src2Type := src2Type

  val NoSpecList = Seq(
    FuType.csr
  )

  val BlockList = Seq(
    FuType.mou
  )

  io.out.bits.ctrl.isNutCoreTrap := (instr(31,0) === NutCoreTrap.TRAP) && io.in.valid
  io.out.bits.ctrl.noSpecExec := NoSpecList.map(j => io.out.bits.ctrl.fuType === j).reduce(_ || _)
  io.out.bits.ctrl.isBlocked :=
  (
    io.out.bits.ctrl.fuType === FuType.lsu && LSUOpType.isAtom(io.out.bits.ctrl.fuOpType) ||
    BlockList.map(j => io.out.bits.ctrl.fuType === j).reduce(_ || _)
  )

  //output signals
  io.out.valid := io.in.valid
  io.in.ready := io.out.ready
  io.out.bits.cf <> io.in.bits

  // fix c_break


  Debug(io.out.fire(), "issue: pc %x npc %x instr %x\n", io.out.bits.cf.pc, io.out.bits.cf.pnpc, io.out.bits.cf.instr)

  val intrVec = WireInit(0.U(12.W))
  BoringUtils.addSink(intrVec, "intrVecIDU")
  io.out.bits.cf.intrVec.zip(intrVec.asBools).map{ case(x, y) => x := y }
  hasIntr := intrVec.orR

  val vmEnable = WireInit(false.B)
  BoringUtils.addSink(vmEnable, "DTLBENABLE")

  io.out.bits.cf.exceptionVec.map(_ := false.B)
  io.out.bits.cf.exceptionVec(illegalInstr) := (instrType === InstrN && !hasIntr) && io.in.valid
  io.out.bits.cf.exceptionVec(instrPageFault) := io.in.bits.exceptionVec(instrPageFault)
  if (VAddrBits > PAddrBits) {
    io.out.bits.cf.exceptionVec(instrAccessFault) := io.in.bits.pc(VAddrBits - 1, PAddrBits).orR && !vmEnable
  } else {
    io.out.bits.cf.exceptionVec(instrAccessFault) := false.B
  }

  io.out.bits.ctrl.isNutCoreTrap := (instr === NutCoreTrap.TRAP) && io.in.valid
  io.isWFI := (instr === Priviledged.WFI) && io.in.valid
  io.isBranch := VecInit(RV32I_BRUInstr.table.map(i => i._2.tail(1) === fuOpType)).asUInt.orR && fuType === FuType.bru


  //new signals
  io.out.bits.ctrl.rs1Valid := ((!instr(14) && !instr(13) && !instr(2)) || (!instr(13)&&instr(11) && !instr(2)) || (instr(19)&&instr(13) && !instr(2)) ||
    (!instr(13) && instr(10) && !instr(2)) || (instr(18) && instr(13) && !instr(2)) || (!instr(13) && instr(9) && !instr(2)) || (
    instr(17) && instr(13) && !instr(2)) || (!instr(13) && instr(8) && !instr(2)) || (instr(16) && instr(13) && !instr(2)) || (
    !instr(13) && instr(7) && !instr(2)) || (instr(15) && instr(13) && !instr(2)) || (!instr(4) && !instr(3)) || (!instr(6)
     && !instr(2))) && rs1 =/= 0.U
  io.out.bits.ctrl.rs2Valid := ((instr(5) && !instr(4) && !instr(2)) || (!instr(6) && instr(5) && !instr(2))) && rs2 =/= 0.U
  io.out.bits.ctrl.rdValid := (!instr(5) && !instr(2)) || (instr(5) && instr(2)) || instr(4)

}

class IDU(implicit val p: NutCoreConfig) extends NutCoreModule with HasInstrType {
  val io = IO(new Bundle {
    val in = Vec(4, Flipped(Decoupled(new CtrlFlowIO)))
    val out = Vec(4, Decoupled(new DecodeIO))
  })
  val decoder1  = Module(new Decoder)
  val decoder2  = Module(new Decoder)
  val decoder3  = Module(new Decoder)
  val decoder4  = Module(new Decoder)
  io.in(0) <> decoder1.io.in
  io.in(1) <> decoder2.io.in
  io.out(0) <> decoder1.io.out
  io.out(1) <> decoder2.io.out
  //copy register for high fanout signal
  io.in(2) <> decoder3.io.in
  io.in(3) <> decoder4.io.in
  io.out(2) <> decoder3.io.out
  io.out(3) <> decoder4.io.out

  if(!EnableMultiIssue){
    io.in(1).ready := false.B
    decoder2.io.in.valid := false.B
  }
  val checkpoint_id = RegInit(0.U(64.W))

  // debug runahead
  /*val runahead = Module(new DifftestRunaheadEvent)
  runahead.io.clock         := clock
  runahead.io.coreid        := 0.U
  runahead.io.valid         := io.out(0).fire()
  runahead.io.branch        := decoder1.io.isBranch
  runahead.io.pc            := io.out(0).bits.cf.pc
  runahead.io.checkpoint_id := checkpoint_id*/
  /*when(runahead.io.valid && runahead.io.branch) {
    checkpoint_id := checkpoint_id + 1.U // allocate a new checkpoint_id
  }
  io.out(0).bits.cf.isBranch := decoder1.io.isBranch
  io.out(0).bits.cf.runahead_checkpoint_id := checkpoint_id*/
  // when(runahead.io.valid) {
  //   printf("fire pc %x branch %x inst %x\n", runahead.io.pc, runahead.io.branch, io.out(0).bits.cf.instr)
  // }

  if (!p.FPGAPlatform) {
    //BoringUtils.addSource(decoder1.io.isWFI | decoder2.io.isWFI, "isWFI")
  }
}
