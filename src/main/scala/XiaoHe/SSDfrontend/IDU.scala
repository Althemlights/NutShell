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

package XiaoHe.SSDfrontend

import XiaoHe.SSDbackend.fu.{ALUOpType, LSUOpType}
import XiaoHe.isa.{Priviledged, RV32I_BRUInstr, RVCInstr, MOUOpType}
import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import utils._
//import difftest._
import chipsalliance.rocketchip.config.Parameters
import system.HasNutCoreParameters
import XiaoHe._
import XiaoHe.SSDbackend._
import XiaoHe.SSDfrontend._
class Decoder(implicit p: Parameters) extends NutCoreModule with HasInstrType {
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

  val rvcImmType :: rvcSrc1Type :: rvcSrc2Type :: rvcDestType :: Nil =
    ListLookup(instr, CInstructions.DecodeDefault, CInstructions.CExtraDecodeTable)   

  val isRVC = io.in.bits.isRVC

  io.out.bits := DontCare
  io.out.bits.cf.triggeredFire := io.in.bits.triggeredFire
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
  val rs1  = instr(11,7)
  val rs2  = instr(6,2)
  val rs1p = LookupTree(instr(9,7), RVCInstr.RVCRegNumTable.map(p => (p._1, p._2)))
  val rs2p = LookupTree(instr(4,2), RVCInstr.RVCRegNumTable.map(p => (p._1, p._2)))
  val rvc_shamt = Cat(instr(12),instr(6,2)) 
  // val rdp_rs1p = LookupTree(instr(9,7), RVCRegNumTable)
  // val rdp      = LookupTree(instr(4,2), RVCRegNumTable)

  val RegLookUpTable = List(
    RVCInstr.DtCare   -> 0.U,
    RVCInstr.REGrs    -> rs,
    RVCInstr.REGrt    -> rt,
    RVCInstr.REGrd    -> rd,
    RVCInstr.REGrs1   -> rs1,
    RVCInstr.REGrs2   -> rs2,
    RVCInstr.REGrs1p  -> rs1p,
    RVCInstr.REGrs2p  -> rs2p,
    RVCInstr.REGx1    -> 1.U,
    RVCInstr.REGx2    -> 2.U
  )

  val rvc_src1 = LookupTree(rvcSrc1Type, RegLookUpTable.map(p => (p._1, p._2)))
  val rvc_src2 = LookupTree(rvcSrc2Type, RegLookUpTable.map(p => (p._1, p._2)))
  val rvc_dest =  LookupTree(rvcDestType, RegLookUpTable.map(p => (p._1, p._2)))


  val rfSrc1 = Mux(isRVC, rvc_src1, rs)
  val rfSrc2 = Mux(isRVC, rvc_src2, rt)
  val rfDest = Mux(isRVC, rvc_dest, rd)


  val imm = LookupTree(instrType, List(
    InstrI  -> SignExt(instr(31, 20), XLEN),
    InstrS  -> SignExt(Cat(instr(31, 25), instr(11, 7)), XLEN),
    InstrSA -> SignExt(Cat(instr(31, 25), instr(11, 7)), XLEN),
    InstrB  -> SignExt(Cat(instr(31), instr(7), instr(30, 25), instr(11, 8), 0.U(1.W)), XLEN),
    InstrU  -> SignExt(Cat(instr(31, 12), 0.U(12.W)), XLEN),//fixed
    InstrJ  -> SignExt(Cat(instr(31), instr(19, 12), instr(20), instr(30, 21), 0.U(1.W)), XLEN)
  ))
  val immrvc = LookupTree(rvcImmType, List(
    // InstrIW -> Cat(Fill(20+32, instr(31)), instr(31, 20)),//fixed
    RVCInstr.ImmNone  -> 0.U(XLEN.W),
    RVCInstr.ImmLWSP  -> ZeroExt(Cat(instr(3,2), instr(12), instr(6,4), 0.U(2.W)), XLEN),
    RVCInstr.ImmLDSP  -> ZeroExt(Cat(instr(4,2), instr(12), instr(6,5), 0.U(3.W)), XLEN),
    RVCInstr.ImmSWSP  -> ZeroExt(Cat(instr(8,7), instr(12,9), 0.U(2.W)), XLEN),
    RVCInstr.ImmSDSP  -> ZeroExt(Cat(instr(9,7), instr(12,10), 0.U(3.W)), XLEN),
    RVCInstr.ImmSW    -> ZeroExt(Cat(instr(5), instr(12,10), instr(6), 0.U(2.W)), XLEN),
    RVCInstr.ImmSD    -> ZeroExt(Cat(instr(6,5), instr(12,10), 0.U(3.W)), XLEN),
    RVCInstr.ImmLW    -> ZeroExt(Cat(instr(5), instr(12,10), instr(6), 0.U(2.W)), XLEN),
    RVCInstr.ImmLD    -> ZeroExt(Cat(instr(6,5), instr(12,10), 0.U(3.W)), XLEN),
    RVCInstr.ImmJ     -> SignExt(Cat(instr(12), instr(8), instr(10,9), instr(6), instr(7), instr(2), instr(11), instr(5,3), 0.U(1.W)), XLEN),
    RVCInstr.ImmB     -> SignExt(Cat(instr(12), instr(6,5), instr(2), instr(11,10), instr(4,3), 0.U(1.W)), XLEN),
    RVCInstr.ImmLI    -> SignExt(Cat(instr(12), instr(6,2)), XLEN),
    RVCInstr.ImmLUI   -> SignExt(Cat(instr(12), instr(6,2), 0.U(12.W)), XLEN),
    RVCInstr.ImmADDI  -> SignExt(Cat(instr(12), instr(6,2)), XLEN),
    RVCInstr.ImmADDI16SP-> SignExt(Cat(instr(12), instr(4,3), instr(5), instr(2), instr(6), 0.U(4.W)), XLEN),
    RVCInstr.ImmADD4SPN-> ZeroExt(Cat(instr(10,7), instr(12,11), instr(5), instr(6), 0.U(2.W)), XLEN),
    RVCInstr.ImmCBREAK -> 1.U(XLEN.W)
    // ImmFLWSP  -> 
    // ImmFLDSP  -> 
  ))



  // TODO: refactor decode logic
  // make non-register addressing to zero, since isu.sb.isBusy(0) === false.B
  io.out.bits.ctrl.rfSrc1 := Mux(src1Type === SrcType.pc, 0.U, rfSrc1)
  io.out.bits.ctrl.rfSrc2 := Mux(src2Type === SrcType.reg, rfSrc2, 0.U)
  io.out.bits.ctrl.rfWen  := isrfWen(instrType)
  io.out.bits.ctrl.rfDest := Mux(isrfWen(instrType), rfDest, 0.U)

  io.out.bits.data := DontCare
  io.out.bits.data.imm  := Mux(isRVC, immrvc, imm)

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
  io.out.bits.ctrl.rs1Valid := src1Type === SrcType.reg
  io.out.bits.ctrl.rs2Valid := src2Type === SrcType.reg
  io.out.bits.ctrl.rdValid := isrfWen(instrType)

}

class IDU(implicit val p: Parameters) extends NutCoreModule with HasInstrType with HasNutCoreParameters {
  val io = IO(new Bundle {
    val in = Vec(4, Flipped(Decoupled(new CtrlFlowIO)))
    val out = Vec(4, Decoupled(new DecodeIO))
    val pipelineEmpty = Input(Bool())
  })
  val decoder1  = Module(new Decoder)
  val decoder2  = Module(new Decoder)
  val decoder3  = Module(new Decoder)
  val decoder4  = Module(new Decoder)
  io.in(0) <> decoder1.io.in
  io.in(1) <> decoder2.io.in
  io.out(0) <> decoder1.io.out
  io.out(1) <> decoder2.io.out

  val mouvalid = ((io.out(0).bits.ctrl.fuType === "b100".U) && io.out(0).valid) || ((io.out(1).bits.ctrl.fuType === "b100".U) && io.out(1).valid)
//  dontTouch(mouvalid)
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

  //val backendEmpty = WireInit(false.B)
  //BoringUtils.addSink(backendEmpty,"backendEmpty")

  //s_idle(no fence) :: s_fence_stall(stall, wait for backend empty) :: s_issue_fence(issue the fence instr)
  val s_idle :: s_fence_stall :: s_issue_fence :: Nil = Enum(3)
  val state = RegInit(s_idle)
  val decode1_has_fence = decoder1.io.out.bits.ctrl.fuType === FuType.mou && decoder1.io.out.bits.ctrl.fuOpType === MOUOpType.fence && decoder1.io.out.valid && io.out(0).ready
  val decode2_has_fence = decoder2.io.out.bits.ctrl.fuType === FuType.mou && decoder2.io.out.bits.ctrl.fuOpType === MOUOpType.fence && decoder2.io.out.valid && io.out(1).ready
  val has_fence = decode1_has_fence || decode2_has_fence

  switch (state) {
    is (s_idle) {
      when (has_fence) {
        state := s_fence_stall
      }
    }
    is (s_fence_stall) {
      decoder1.io.in.valid := false.B
      decoder2.io.in.valid := false.B
      decoder3.io.in.valid := false.B
      decoder4.io.in.valid := false.B
      when (io.pipelineEmpty) {
        state := s_issue_fence
      }
    }
    is (s_issue_fence) {
      when (decode1_has_fence) {
        state := s_idle
      }
    }
  }

  //decode1 fence : stall decode1 and decode2
  //decode2 fence : stall decode2, release decode1
  when ((state === s_idle && has_fence) || state === s_fence_stall) {
    io.out(0).valid := false.B 
    io.out(1).valid := false.B 
    io.out(2).valid := false.B 
    io.out(3).valid := false.B 
    io.in(0).ready := false.B
    io.in(1).ready := false.B
    io.in(2).ready := false.B
    io.in(3).ready := false.B
  } 
  
  when (state === s_idle && decode2_has_fence) {
    io.out(0).valid := true.B 
    io.out(2).valid := true.B 
    //io.in(0).ready := true.B
    io.in(0).ready := decoder1.io.in.ready 
    io.in(2).ready := decoder3.io.in.ready
  } 
  // debug runahead
  /*val runahead = Module(new DifftestRunaheadEvent)
  runahead.io.clock         := clock
  runahead.io.coreid        := 0.U
  runahead.io.valid         := io.out(0).fire
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

  if (!FPGAPlatform) {
    //BoringUtils.addSource(decoder1.io.isWFI | decoder2.io.isWFI, "isWFI")
  }
}
