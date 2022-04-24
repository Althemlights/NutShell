package SSDbackend

import bus.simplebus.SimpleBusUC
import chisel3.{Mux, _}
import chisel3.util._
import difftest._
import utils.{PipelineConnect, SignExt}
import nutcore._
import chisel3.util.experimental.BoringUtils

import java.io.PipedOutputStream


class SSDbackend extends NutCoreModule with hasBypassConst {
  val io = IO(new Bundle{
    val in = Vec(2, Flipped(Decoupled(new DecodeIO)))
    val redirectOut = new RedirectIO
    val dmem = new SimpleBusUC(addrBits = VAddrBits) // without dtlb
    //val mmio = new SimpleBusUC
  })
  def BypassMux(sel:Bool,BypassCtl:Vec[Bool],BypassDataPort:Vec[UInt],rdata:UInt):UInt ={
    Mux(sel,PriorityMux(BypassCtl,BypassDataPort),rdata)
}

  //new
  val Bypass = Module(new Bypass)
  val regfile = Module(new SSDRF)

  //pipeline interface
  val pipeIn = Wire(Vec(10,Flipped(Decoupled(new FuPkt))))
  val pipeOut = Wire(Vec(10,Decoupled(new FuPkt)))
  val pipeFire = Wire(Vec(10,Bool()))
  val pipeFlush = Wire(Vec(10,Bool()))

  //e1 -e5 register
  val pipeRegStage0 = Module(new stallPointConnect(new FuPkt)).suggestName("pipeStage0")
  val pipeRegStage1 = Module(new stallPointConnect(new FuPkt)).suggestName("pipeStage1")
  val pipeRegStage2 = Module(new normalPipeConnect(new FuPkt)).suggestName("pipeStage2")
  val pipeRegStage3 = Module(new normalPipeConnect(new FuPkt)).suggestName("pipeStage3")
  val pipeRegStage4 = Module(new normalPipeConnect(new FuPkt)).suggestName("pipeStage4")
  val pipeRegStage5 = Module(new normalPipeConnect(new FuPkt)).suggestName("pipeStage5")
  val pipeRegStage6 = Module(new normalPipeConnect(new FuPkt)).suggestName("pipeStage6")
  val pipeRegStage7 = Module(new normalPipeConnect(new FuPkt)).suggestName("pipeStage7")
  val pipeRegStage8 = Module(new normalPipeConnect(new FuPkt)).suggestName("pipeStage8")
  val pipeRegStage9 = Module(new normalPipeConnect(new FuPkt)).suggestName("pipeStage9")

  pipeFlush := Bypass.io.pipeFlush
  for(i <- 0 to 3){
    pipeFire(2*i) := pipeOut(2*i).valid && pipeIn(2*i+2).ready
    pipeFire(2*i+1) := pipeOut(2*i+1).valid && pipeIn(2*i+3).ready
  }
  pipeFire(8) := true.B
  pipeFire(9) := true.B

  //Bypass
  val memStall = Wire(Bool())
  val mduStall = Wire(Bool())
  Bypass.io.in <> io.in
  Bypass.io.memStall := memStall
  Bypass.io.mduStall := mduStall
  val issueStall = VecInit(false.B,false.B)
  issueStall := Bypass.io.issueStall
  val BypassPkt = Wire(Vec(10,new BypassPkt))
  val BypassPktE0 = Wire(Vec(2,Decoupled(new BypassPkt)))
  dontTouch(BypassPktE0)
  val BypassPktValid = Wire(Vec(10,Bool()))
  BypassPkt := Bypass.io.BypassPkt
  BypassPktE0 := Bypass.io.decodeBypassPkt
  BypassPktValid := Bypass.io.BypassPktValid

  Bypass.io.decodeBypassPkt(0).ready := pipeIn(0).ready
  Bypass.io.decodeBypassPkt(1).ready := pipeIn(1).ready
  BypassPktE0(0).ready := pipeIn(0).ready
  BypassPktE0(1).ready := pipeIn(1).ready

  //decode & issue & e0bypass
  //ALU & SUB_ALU
  val ALU_0 = Module(new ALU).suggestName("ALU0")
  val ALU_1 = Module(new ALU).suggestName("ALU1")
  val ALU_6 = Module(new ALU).suggestName("ALU6")
  val ALU_7 = Module(new ALU).suggestName("ALU7")
  val Redirect0 = Wire(new RedirectIO)
  val Redirect1 = Wire(new RedirectIO)
  val Redirect6 = Wire(new RedirectIO)
  val Redirect7 = Wire(new RedirectIO)
  val ALUList = List(ALU_0,ALU_1,ALU_6,ALU_7)
  val pipeOut2ALUList = List(pipeOut(0),pipeOut(1),pipeOut(6),pipeOut(7))
  val ALURedirectList = List(Redirect0,Redirect1,Redirect6,Redirect7)
  ALURedirectList.foreach{case i => dontTouch(i)}
  
  //alu io
  ALU_0.io.cfIn := 0.U.asTypeOf(new CtrlFlowIO)
  ALU_1.io.cfIn := 0.U.asTypeOf(new CtrlFlowIO)
  ALU_6.io.cfIn := 0.U.asTypeOf(new CtrlFlowIO)
  ALU_7.io.cfIn := 0.U.asTypeOf(new CtrlFlowIO)

  (ALUList zip pipeOut2ALUList).foreach{ case(a,b) =>
    a.io.offset := b.bits.offset
    a.io.out.ready := true.B
    a.io.cfIn.pc := b.bits.pc
    a.io.cfIn.pnpc := b.bits.pnpc
    a.io.cfIn.instr := b.bits.instr
    a.io.cfIn.brIdx := b.bits.brIdx
    a.io.cfIn.isRVC := b.bits.isRVC
    a.io.cfIn.isBranch := b.bits.isBranch
  }
  (ALURedirectList zip ALUList).foreach{ case(a,b) =>
    a := b.io.redirect
  }
  val flushList = List(0,1,2,3)
  (flushList zip ALURedirectList).foreach{ case(a,b) =>
    Bypass.io.flush(a) := b.valid && pipeOut(a).ready
    dontTouch(b.valid)
  }

  io.redirectOut := Mux(Redirect7.valid,Redirect7,
                               Mux(Redirect6.valid,Redirect6,
                               Mux(Redirect1.valid,Redirect1,
                               Mux(Redirect0.valid,Redirect0,0.U.asTypeOf(new RedirectIO)))))



  val aluValid = VecInit(false.B,false.B,false.B,false.B)
  aluValid := Seq(
    pipeOut(0).valid && BypassPkt(0).decodePkt.alu && !BypassPkt(0).decodePkt.subalu,
    pipeOut(1).valid && BypassPkt(1).decodePkt.alu && !BypassPkt(1).decodePkt.subalu,
    pipeOut(6).valid && BypassPkt(6).decodePkt.alu && BypassPkt(6).decodePkt.subalu,
    pipeOut(7).valid && BypassPkt(7).decodePkt.alu && BypassPkt(7).decodePkt.subalu
  )

  ALU_0.access(aluValid(0),pipeOut(0).bits.rs1,pipeOut(0).bits.rs2,pipeOut(0).bits.fuOpType)
  ALU_1.access(aluValid(1),pipeOut(1).bits.rs1,pipeOut(1).bits.rs2,pipeOut(1).bits.fuOpType)
  ALU_6.access(aluValid(2),pipeOut(6).bits.rs1,pipeOut(6).bits.rs2,pipeOut(6).bits.fuOpType)
  ALU_7.access(aluValid(3),pipeOut(7).bits.rs1,pipeOut(7).bits.rs2,pipeOut(7).bits.fuOpType)

  //LSU
  val LSU = Module(new SSDLSU)
  io.dmem <> LSU.io.dmem
  dontTouch(io.dmem.resp.ready)
  LSU.io.out.ready := true.B//!(Redirect6.valid || Redirect7.valid)
  memStall := LSU.io.memStall
  LSU.io.flush := Redirect6.valid || Redirect7.valid
  val i0LSUValid = BypassPktValid(0) && (BypassPkt(0).decodePkt.load || BypassPkt(0).decodePkt.store)
  val i1LSUValid = BypassPktValid(1) && (BypassPkt(1).decodePkt.load || BypassPkt(1).decodePkt.store)
  dontTouch(i0LSUValid)
  dontTouch(i1LSUValid)
  val LSUValid = i0LSUValid || i1LSUValid
  val LSUfunc = Mux(i1LSUValid,pipeRegStage1.right.bits.fuOpType,pipeRegStage0.right.bits.fuOpType)
  val LSUsrc1 = Mux(i1LSUValid,pipeRegStage1.right.bits.rs1,pipeRegStage0.right.bits.rs1)
  val LSUsrc2 = Mux(i1LSUValid,pipeRegStage1.right.bits.rs2,pipeRegStage0.right.bits.rs2)
  val LSUoffset = Mux(i1LSUValid,pipeRegStage1.right.bits.offset,pipeRegStage0.right.bits.offset)
  LSU.access(LSUValid,LSUsrc1,LSUsrc2,LSUfunc,LSUoffset)
  //MDU
  val MDU = Module(new MDU)
  MDU.io.out.ready := true.B
  val i0MDUValid = BypassPktValid(0) && (BypassPkt(0).decodePkt.muldiv)
  val i1MDUValid = BypassPktValid(1) && (BypassPkt(1).decodePkt.muldiv)
  val MDUValid = i0MDUValid || i1MDUValid
  val MDUfunc = Mux(i1MDUValid,pipeRegStage1.right.bits.fuOpType,pipeRegStage0.right.bits.fuOpType)
  val MDUsrc1 = Mux(i1MDUValid,pipeRegStage1.right.bits.rs1,pipeRegStage0.right.bits.rs1)
  val MDUsrc2 = Mux(i1MDUValid,pipeRegStage1.right.bits.rs2,pipeRegStage0.right.bits.rs2)
  MDU.access(MDUValid,MDUsrc1,MDUsrc2,MDUfunc)
  mduStall := (BypassPkt(4).decodePkt.muldiv && pipeRegStage4.right.valid || BypassPkt(5).decodePkt.muldiv && pipeRegStage5.right.valid) && !MDU.io.out.valid ||
    MDUValid && !MDU.io.in.ready
  //Bypass signal and data port
  val ByPassEna = Wire(Vec(12,Bool()))
  ByPassEna := Seq(
    //e0
    BypassPktE0(0).bits.BypassCtl.rs1bypasse0.asUInt.orR,
    BypassPktE0(0).bits.BypassCtl.rs2bypasse0.asUInt.orR,
    BypassPktE0(1).bits.BypassCtl.rs1bypasse0.asUInt.orR,
    BypassPktE0(1).bits.BypassCtl.rs2bypasse0.asUInt.orR,
    //e2
    BypassPkt(2).BypassCtl.rs1bypasse2.asUInt.orR,
    BypassPkt(2).BypassCtl.rs2bypasse2.asUInt.orR,
    BypassPkt(3).BypassCtl.rs1bypasse2.asUInt.orR,
    BypassPkt(3).BypassCtl.rs2bypasse2.asUInt.orR,
    //e3
    BypassPkt(4).BypassCtl.rs1bypasse3.asUInt.orR,
    BypassPkt(4).BypassCtl.rs2bypasse3.asUInt.orR,
    BypassPkt(5).BypassCtl.rs1bypasse3.asUInt.orR,
    BypassPkt(5).BypassCtl.rs2bypasse3.asUInt.orR
  )

  val BypassPortE0 = Wire(Vec(E0BypassPort,UInt(64.W)))
  BypassPortE0 := Seq(pipeIn(2).bits.rd,pipeIn(3).bits.rd,pipeIn(4).bits.rd,pipeIn(5).bits.rd,
    pipeIn(6).bits.rd,pipeIn(7).bits.rd,LSU.io.out.bits,MDU.io.out.bits,
    pipeIn(8).bits.rd,pipeIn(9).bits.rd,pipeOut(8).bits.rd,pipeOut(9).bits.rd)

  val BypassPortE2 = Wire(Vec(E2BypassPort,UInt(64.W)))
  BypassPortE2 := Seq(pipeOut(8).bits.rd,pipeOut(9).bits.rd)
  val BypassPortE3 = Wire(Vec(E3BypassPort,UInt(64.W)))
  BypassPortE3 := Seq(pipeIn(7).bits.rd,pipeIn(8).bits.rd,pipeIn(9).bits.rd,pipeOut(8).bits.rd,pipeOut(9).bits.rd)
  dontTouch(BypassPortE0)
  dontTouch(BypassPortE2)
  dontTouch(BypassPortE3)
  //decode & issue
  //rs1 data type: pc, regfile or bypassa
  //rs2 data type: imm, regfilw or bypass
  val e0ByapssRs1 = VecInit(0.U(64.W),0.U(64.W))
  val e0ByapssRs2 = VecInit(0.U(64.W),0.U(64.W))
  e0ByapssRs1(0) := BypassMux(ByPassEna(0), BypassPktE0(0).bits.BypassCtl.rs1bypasse0,BypassPortE0, regfile.io.readPorts(0).data)
  e0ByapssRs1(1) := BypassMux(ByPassEna(2), BypassPktE0(1).bits.BypassCtl.rs1bypasse0,BypassPortE0, regfile.io.readPorts(2).data)
  e0ByapssRs2(0) := BypassMux(ByPassEna(1), BypassPktE0(0).bits.BypassCtl.rs2bypasse0,BypassPortE0, regfile.io.readPorts(1).data)
  e0ByapssRs2(1) := BypassMux(ByPassEna(3), BypassPktE0(1).bits.BypassCtl.rs2bypasse0,BypassPortE0, regfile.io.readPorts(3).data)
 //myDebug(pipeIn(0).bits.pc === "h8000003c".U,"pipeIn(0) pc: %x, rs1Bypasse0: %b,rs1Bypass data: %x",pipeIn(0).bits.pc,BypassPktE0(0).bits.BypassCtl.rs1bypasse0.asUInt,e0ByapssRs1(0))

  for(i <-0 to 1){
    pipeIn(i).valid := io.in(1-i).valid
    io.in(i).ready := pipeIn(1-i).ready
    pipeIn(i).bits.rd := 0.U(64.W)
    pipeIn(i).bits.rs1 := Mux(io.in(1-i).bits.ctrl.src1Type === SrcType.pc,
      SignExt(io.in(1-i).bits.cf.pc, 64),e0ByapssRs1(i))
    pipeIn(i).bits.rs2 := Mux(io.in(1-i).bits.ctrl.src2Type =/= SrcType.reg,
      io.in(1-i).bits.data.imm,e0ByapssRs2(i))
    pipeIn(i).bits.fuOpType := io.in(1-i).bits.ctrl.fuOpType
    pipeIn(i).bits.offset := io.in(1-i).bits.data.imm
    pipeIn(i).bits.instr := io.in(1-i).bits.cf.instr
    pipeIn(i).bits.pc := io.in(1-i).bits.cf.pc
    pipeIn(i).bits.pnpc := io.in(1-i).bits.cf.pnpc
    pipeIn(i).bits.isRVC := io.in(1-i).bits.cf.isRVC
    pipeIn(i).bits.brIdx := io.in(1-i).bits.cf.brIdx
    pipeIn(i).bits.isBranch := io.in(1-i).bits.cf.isBranch
    //for Debug
    pipeIn(i).bits.debugInfo.rs1 := io.in(1-i).bits.ctrl.rfSrc1
    pipeIn(i).bits.debugInfo.rs2 := io.in(1-i).bits.ctrl.rfSrc2
    pipeIn(i).bits.debugInfo.rd  := io.in(1-i).bits.ctrl.rfDest
    pipeIn(i).bits.debugInfo.rdValid  := io.in(1-i).bits.ctrl.rfWen
    pipeIn(i).bits.debugInfo.rs1Valid  := io.in(1-i).bits.ctrl.src1Type === SrcType.reg
    pipeIn(i).bits.debugInfo.rs2Valid  := io.in(1-i).bits.ctrl.src2Type === SrcType.reg
    pipeIn(i).bits.debugInfo.rs1Pc  := io.in(1-i).bits.ctrl.src1Type === SrcType.pc
    pipeIn(i).bits.debugInfo.rs2Imm  := io.in(1-i).bits.ctrl.src2Type === SrcType.imm
    //for csr inst
    pipeIn(i).bits.csrInst := io.in(1-i).bits.cf.instr(6,0) === "b1110011".U

  }

  for(i <- 2 to 9 ){
      pipeIn(i).bits := pipeOut(i - 2).bits
      pipeIn(i).valid := pipeOut(i - 2).valid
      pipeOut(i - 2).ready := pipeIn(i).ready
//    }
  }
  pipeOut(8).ready := true.B && !(memStall || mduStall)
  pipeOut(9).ready := true.B && !(memStall || mduStall)

  //e1
  pipeIn(2).bits.rd := Mux(aluValid(0),ALU_0.io.out.bits,0.U(64.W))
  pipeIn(3).bits.rd := Mux(aluValid(1),ALU_1.io.out.bits,0.U(64.W))

  //e2
  pipeIn(4).bits.rs1 := BypassMux(ByPassEna(4), BypassPkt(2).BypassCtl.rs1bypasse2,BypassPortE2, pipeOut(2).bits.rs1)
  pipeIn(4).bits.rs2 := BypassMux(ByPassEna(5), BypassPkt(2).BypassCtl.rs2bypasse2,BypassPortE2, pipeOut(2).bits.rs2)
  pipeIn(5).bits.rs1 := BypassMux(ByPassEna(6), BypassPkt(3).BypassCtl.rs1bypasse2,BypassPortE2, pipeOut(3).bits.rs1)
  pipeIn(5).bits.rs2 := BypassMux(ByPassEna(7), BypassPkt(3).BypassCtl.rs2bypasse2,BypassPortE2, pipeOut(3).bits.rs2)

  //e3
  pipeIn(6).bits.rd := Mux(BypassPkt(4).decodePkt.load,LSU.io.out.bits,Mux(BypassPkt(4).decodePkt.muldiv,MDU.io.out.bits,pipeOut(4).bits.rd)) //add other lsu result
  pipeIn(6).bits.rs1 := BypassMux(ByPassEna(8), BypassPkt(4).BypassCtl.rs1bypasse3,BypassPortE3, pipeOut(4).bits.rs1)
  pipeIn(6).bits.rs2 := BypassMux(ByPassEna(9), BypassPkt(4).BypassCtl.rs2bypasse3,BypassPortE3, pipeOut(4).bits.rs2)
  pipeIn(7).bits.rd := Mux(BypassPkt(5).decodePkt.load,LSU.io.out.bits,Mux(BypassPkt(5).decodePkt.muldiv,MDU.io.out.bits,pipeOut(5).bits.rd))
  pipeIn(7).bits.rs1 := BypassMux(ByPassEna(10), BypassPkt(5).BypassCtl.rs1bypasse3,BypassPortE3, pipeOut(5).bits.rs1)
  pipeIn(7).bits.rs2 := BypassMux(ByPassEna(11), BypassPkt(5).BypassCtl.rs2bypasse3,BypassPortE3, pipeOut(5).bits.rs2)

  //e4
  pipeIn(8).bits.rd := Mux(aluValid(2),ALU_6.io.out.bits,pipeOut(6).bits.rd)
  pipeIn(9).bits.rd := Mux(aluValid(3),ALU_7.io.out.bits,pipeOut(7).bits.rd)

  //e5 write back
  //regfile
  regfile.io.writePorts(0).wen := BypassPktValid(8) && BypassPkt(8).decodePkt.rdvalid
  regfile.io.writePorts(0).addr := BypassPkt(8).decodePkt.rd
  regfile.io.writePorts(0).data := pipeOut(8).bits.rd
  regfile.io.writePorts(1).wen := BypassPktValid(9) && BypassPkt(9).decodePkt.rdvalid
  regfile.io.writePorts(1).addr := BypassPkt(9).decodePkt.rd
  regfile.io.writePorts(1).data := pipeOut(9).bits.rd


  //i1rs1,i1rs2,i0rs1,i0rs2
  regfile.io.readPorts(0).addr := io.in(1).bits.ctrl.rfSrc1
  regfile.io.readPorts(1).addr := io.in(1).bits.ctrl.rfSrc2
  regfile.io.readPorts(2).addr := io.in(0).bits.ctrl.rfSrc1
  regfile.io.readPorts(3).addr := io.in(0).bits.ctrl.rfSrc2

  // for debug
  val lsuPC =WireInit(0.U(VAddrBits.W))
  lsuPC := Mux(BypassPkt(1).decodePkt.load || BypassPkt(1).decodePkt.store, pipeOut(1).bits.pc, pipeOut(0).bits.pc)
  BoringUtils.addSource(lsuPC,"lsuPC")

  // pipe connect

  val stallStageList = List(pipeRegStage0,pipeRegStage1)
  val stallIndexList = List(0,1)
  (stallStageList zip stallIndexList).foreach{case (a,b) =>
    a.io.left <> pipeIn(b)
    a.io.right <> pipeOut(b)
    a.io.rightOutFire <> pipeFire(b)
    a.io.isFlush <> pipeFlush(b)
  }
  pipeRegStage0.io.isStall := issueStall(0)
  pipeRegStage1.io.isStall := issueStall(1)
//  pipeRegStage2.io.isStall := LSU.io.cacheS2NotReady
//  pipeRegStage3.io.isStall := LSU.io.cacheS2NotReady
//  pipeRegStage4.io.isStall := LSU.io.cacheS3NotReady
//  pipeRegStage5.io.isStall := LSU.io.cacheS3NotReady
//  pipeRegStage6.io.isStall := memStall
//  pipeRegStage7.io.isStall := memStall


  val normalStageList = List(pipeRegStage2,pipeRegStage3,pipeRegStage4,pipeRegStage5,pipeRegStage6,pipeRegStage7,pipeRegStage8,pipeRegStage9)
  val normalIndexList = List(2,3,4,5,6,7,8,9)

  (normalStageList zip normalIndexList).foreach{case (a,b) =>
    a.io.left <> pipeIn(b)
    a.io.right <> pipeOut(b)
    a.io.rightOutFire <> pipeFire(b)
    a.io.isFlush <> pipeFlush(b)
  }

  //Pipeline Debug
  //Pipeline basic information
  def instTypePrint(valid:Bool, BypassPkt: BypassPkt)={
    val aluCond = BypassPkt.decodePkt.alu && !BypassPkt.decodePkt.subalu
    val subaluCond = BypassPkt.decodePkt.alu &&  BypassPkt.decodePkt.subalu
    val loadCond = BypassPkt.decodePkt.load
    val storeCond = BypassPkt.decodePkt.store
    val elseCond = !aluCond && !subaluCond && !loadCond && !storeCond && valid || ! valid
    myDebug(valid && aluCond,   " ALU   ")
    myDebug(valid && subaluCond," SubALU")
    myDebug(valid && loadCond,  " Load  ")
    myDebug(valid && storeCond, " Store ")
    myDebug(elseCond,           "       ")
  }
  def rsrdPrintf (valid:Bool, pipeinfo:FuPkt )={
    myDebug(valid && pipeinfo.debugInfo.rs1Valid,"rs1[%x]: %x ;",pipeinfo.debugInfo.rs1,pipeinfo.rs1)
    myDebug(valid && pipeinfo.debugInfo.rs1Pc,   "rs1[pc ]: %x ;",pipeinfo.pc)
    myDebug(valid && pipeinfo.debugInfo.rs2Valid,"rs2[%x]: %x ;",pipeinfo.debugInfo.rs2,pipeinfo.rs2)
    myDebug(valid && pipeinfo.debugInfo.rs2Imm,  "rs2[imm]: %x ;",pipeinfo.offset)
    myDebug(valid && pipeinfo.debugInfo.rdValid, "rd [%x]: %x ;",pipeinfo.debugInfo.rd,pipeinfo.rd)
    myDebug(valid && !pipeinfo.debugInfo.rdValid,"             \n")
  }
  def pipeInPrintf (valid:Bool, pipeIn:FuPkt )={
    myDebug(valid,"rs1:%x, rs2:%x, rd:%x",pipeIn.rs1,pipeIn.rs2,pipeIn.rd)
  }
  val tag = pipeIn(0).bits.pc === "h800000d8".U || pipeIn(1).bits.pc === "h800000d8".U
  dontTouch(tag)
  val pipeDebug = false
  if(pipeDebug){
  printf("=========================================================\n")
  printf("--------------------- Pipeline state --------------------\n")
  printf("=========================================================\n")
  for(i <- 0 to 4){
    myDebug(pipeOut(2*i).valid,"| %x | %x ",(2*i).U,pipeOut(2*i).bits.pc)
    myDebug(!pipeOut(2*i).valid,"| %x |            ",(2*i).U)
    instTypePrint(Bypass.io.BypassPktValid(2*i),Bypass.io.BypassPkt(2*i))
    myDebug(pipeOut(2*i+1).valid,"| %x | %x ",(2*i+1).U,pipeOut(2*i+1).bits.pc)
    myDebug(!pipeOut(2*i+1).valid,"| %x |            ",(2*i+1).U)
    instTypePrint(Bypass.io.BypassPktValid(2*i+1),Bypass.io.BypassPkt(2*i+1))
    printf("|\n")
    }
  printf("=========================================================\n")
  printf("---------------------- rd / rs info ---------------------\n")
  printf("=========================================================\n")
  for(i <- 0 to 9){
    printf("Pipe%x: ",i.U)
    rsrdPrintf(pipeOut(i).valid,pipeOut(i).bits)
    printf("\n")
  }
  printf("=========================================================\n")
  printf("--------------------- Pipeline Input --------------------\n")
  printf("=========================================================\n")
  for(i <- 0 to 9){
    printf("Pipe%x: ",i.U)
    pipeInPrintf(pipeIn(i).valid,pipeIn(i).bits)
    printf("\n")
  }
  printf("=========================================================\n")
} //SSDCore Performance Counter
  val SSDCorePerfCntList = Map(
    "i0Issue"   -> (0x0, "perfCntI0Issue"      ),
    "i1Issue"   -> (0x1, "perfCntI1Issue"      ),
    "i0Stall"   -> (0x2, "perfCntI0Stall"      ),
    "i1Stall"   -> (0x3, "perfCntI1Stall"      ),
    "e0Bypass"  -> (0x4, "perfCntE0Bypass"     ),
    "e2Bypass"  -> (0x5, "perfCntE2Bypass"     ),
    "e3Bypass"  -> (0x6, "perfCntE3Bypass"     )
  )

  val perfCntNum = if (SSDCoreConfig().EnablePerfCnt) 7 else 0
  val perfCnts = List.fill(perfCntNum)(RegInit(0.U(64.W)))
  val perfCntCond = List.fill(perfCntNum)(WireInit(false.B))
  (perfCnts zip perfCntCond).map { case (c, e) => { when (e) { c := c + 1.U } } }
  when(perfCntCond(0x4)){ perfCnts(0x4) := perfCnts(0x4) +
    BypassPkt(8).BypassCtl.rs1bypasse0.asUInt.orR.asUInt + BypassPkt(8).BypassCtl.rs2bypasse0.asUInt.orR.asUInt +
    BypassPkt(9).BypassCtl.rs1bypasse0.asUInt.orR.asUInt + BypassPkt(9).BypassCtl.rs2bypasse0.asUInt.orR.asUInt
  }
  when(perfCntCond(0x5)){ perfCnts(0x5) := perfCnts(0x5) +
    BypassPkt(8).BypassCtl.rs1bypasse2.asUInt.orR.asUInt + BypassPkt(8).BypassCtl.rs2bypasse2.asUInt.orR.asUInt +
    BypassPkt(9).BypassCtl.rs1bypasse2.asUInt.orR.asUInt + BypassPkt(9).BypassCtl.rs2bypasse2.asUInt.orR.asUInt
  }
  when(perfCntCond(0x6)){ perfCnts(0x6) := perfCnts(0x6) +
    BypassPkt(8).BypassCtl.rs1bypasse3.asUInt.orR.asUInt + BypassPkt(8).BypassCtl.rs2bypasse3.asUInt.orR.asUInt +
    BypassPkt(9).BypassCtl.rs1bypasse3.asUInt.orR.asUInt + BypassPkt(9).BypassCtl.rs2bypasse3.asUInt.orR.asUInt
  }

  //BoringUtils.addSource(io.in(1).valid && io.in(1).ready,"perfCntI0Issue")
  //BoringUtils.addSource(io.in(0).valid && io.in(0).ready,"perfCntI1Issue")
  //BoringUtils.addSource(!io.in(1).ready,"perfCntI0Stall")
  //BoringUtils.addSource(!io.in(0).ready,"perfCntI1Stall")
  //BoringUtils.addSource(BypassPkt(8).BypassCtl.rs1bypasse0.asUInt.orR || BypassPkt(8).BypassCtl.rs2bypasse0.asUInt.orR ||
    //BypassPkt(9).BypassCtl.rs1bypasse0.asUInt.orR || BypassPkt(9).BypassCtl.rs2bypasse0.asUInt.orR,"perfCntE0Bypass")
  //BoringUtils.addSource(BypassPkt(8).BypassCtl.rs1bypasse2.asUInt.orR || BypassPkt(8).BypassCtl.rs2bypasse2.asUInt.orR ||
    //BypassPkt(9).BypassCtl.rs1bypasse2.asUInt.orR || BypassPkt(9).BypassCtl.rs2bypasse2.asUInt.orR,"perfCntE2Bypass")
  //BoringUtils.addSource(BypassPkt(8).BypassCtl.rs1bypasse3.asUInt.orR || BypassPkt(8).BypassCtl.rs2bypasse3.asUInt.orR ||
    //BypassPkt(9).BypassCtl.rs1bypasse3.asUInt.orR || BypassPkt(9).BypassCtl.rs2bypasse3.asUInt.orR,"perfCntE3Bypass")

  val SSDcoretrap = WireInit(false.B)
  BoringUtils.addSource((pipeOut(8).bits.instr === "h0000006b".U || pipeOut(8).bits.instr === "h0005006b".U) && pipeOut(8).fire() ||
    (pipeOut(9).bits.instr === "h0000006b".U || pipeOut(9).bits.instr === "h0005006b".U) && pipeOut(9).fire(),"SSDcoretrap")
  BoringUtils.addSink(SSDcoretrap,"SSDcoretrap")

//  SSDCorePerfCntList.map { case (name, (addr, boringId)) =>
//    BoringUtils.addSink(perfCntCond(addr), boringId)}
//
//  if (SSDCoreConfig().EnablePerfCnt) {
//    when(RegNext(RegNext(SSDcoretrap))) {
//      printf("======== SSDCorePerfCnt =========\n")
//      SSDCorePerfCntList.map { case (name, (addr, boringId)) =>
//        printf("%d <- " + name + "\n", perfCnts(addr))
//      }
//      printf("=================================\n")
//    }
//  }
  /* ----- Difftest ----- */

  val dt_ic1 = Module(new DifftestInstrCommit)
  dt_ic1.io.clock    := clock
  dt_ic1.io.coreid   := 0.U
  dt_ic1.io.index    := 0.U
  dt_ic1.io.valid    := RegNext(pipeOut(9).valid && pipeOut(9).bits.pc =/= 0.U)
  dt_ic1.io.pc       := RegNext(Cat(0.U((64-VAddrBits).W),pipeOut(9).bits.pc))
  dt_ic1.io.instr    := RegNext(pipeOut(9).bits.instr)
  dt_ic1.io.special  := 0.U
  dt_ic1.io.skip     := RegNext(pipeOut(9).valid && pipeOut(9).bits.csrInst)
  dt_ic1.io.isRVC    := false.B
  dt_ic1.io.scFailed := false.B
  dt_ic1.io.wen      := RegNext(regfile.io.writePorts(1).wen)
  dt_ic1.io.wpdest   := RegNext(Cat(0.U(3.W),regfile.io.writePorts(1).addr))
  dt_ic1.io.wdest    := RegNext(Cat(0.U(3.W),regfile.io.writePorts(1).addr))

  val dt_ic0 = Module(new DifftestInstrCommit)
  dt_ic0.io.clock    := clock
  dt_ic0.io.coreid   := 0.U
  dt_ic0.io.index    := 1.U

  dt_ic0.io.valid    := RegNext(pipeOut(8).valid && pipeOut(8).bits.pc =/= 0.U)
  dt_ic0.io.pc       := RegNext(Cat(0.U((64-VAddrBits).W),pipeOut(8).bits.pc))
  dt_ic0.io.instr    := RegNext(pipeOut(8).bits.instr)
  dt_ic0.io.special  := 0.U
  dt_ic0.io.skip     := RegNext(pipeOut(8).valid && pipeOut(8).bits.csrInst)
  dt_ic0.io.isRVC    := false.B
  dt_ic0.io.scFailed := false.B
  dt_ic0.io.wen      := RegNext(regfile.io.writePorts(0).wen)
  dt_ic0.io.wpdest   := RegNext(Cat(0.U(3.W),regfile.io.writePorts(0).addr))
  dt_ic0.io.wdest    := RegNext(Cat(0.U(3.W),regfile.io.writePorts(0).addr))

  val dt_iw0 = Module(new DifftestIntWriteback)
  dt_iw0.io.clock    := clock
  dt_iw0.io.coreid   := 0.U
  dt_iw0.io.valid    := RegNext(regfile.io.writePorts(1).wen)
  dt_iw0.io.dest     := RegNext(regfile.io.writePorts(1).addr)
  dt_iw0.io.data     := RegNext(regfile.io.writePorts(1).data)



  val dt_iw1 = Module(new DifftestIntWriteback)
  dt_iw1.io.clock    := clock
  dt_iw1.io.coreid   := 0.U
  dt_iw1.io.valid    := RegNext(regfile.io.writePorts(0).wen)
  dt_iw1.io.dest     := RegNext(regfile.io.writePorts(0).addr)
  dt_iw1.io.data     := RegNext(regfile.io.writePorts(0).data)




  val dt_ae = Module(new DifftestArchEvent)
  dt_ae.io.clock        := clock
  dt_ae.io.coreid       := 0.U
  dt_ae.io.intrNO       := 0.U
  dt_ae.io.cause        := 0.U
  dt_ae.io.exceptionPC  := 0.U

  val cycle_cnt = RegInit(0.U(64.W))
  val instr_cnt = RegInit(0.U(64.W))

  cycle_cnt := cycle_cnt + 1.U
  instr_cnt := instr_cnt + RegNext(pipeOut(8).valid).asUInt + RegNext(pipeOut(9).valid).asUInt

  val rf_a0 = WireInit(0.U(64.W))
  BoringUtils.addSink(rf_a0, "rf_a0")

  val dt_te = Module(new DifftestTrapEvent)
  dt_te.io.clock    := clock
  dt_te.io.coreid   := 0.U
  dt_te.io.valid    := RegNext(SSDcoretrap)
  dt_te.io.code     := rf_a0(2, 0)
  dt_te.io.pc       := Mux(RegNext(pipeOut(8).bits.instr === "h0000006b".U),RegNext(Cat(0.U((64-VAddrBits).W),pipeOut(8).bits.pc)),RegNext(Cat(0.U((64-VAddrBits).W),pipeOut(9).bits.pc)))
  dt_te.io.cycleCnt := cycle_cnt
  dt_te.io.instrCnt := instr_cnt

  val dt_cs = Module(new DifftestCSRState)
  dt_cs.io.clock          := clock
  dt_cs.io.coreid         := 0.U
  dt_cs.io.priviledgeMode := 3.U  // Machine mode
  dt_cs.io.mstatus        := 0.U
  dt_cs.io.sstatus        := 0.U
  dt_cs.io.mepc           := 0.U
  dt_cs.io.sepc           := 0.U
  dt_cs.io.mtval          := 0.U
  dt_cs.io.stval          := 0.U
  dt_cs.io.mtvec          := 0.U
  dt_cs.io.stvec          := 0.U
  dt_cs.io.mcause         := 0.U
  dt_cs.io.scause         := 0.U
  dt_cs.io.satp           := 0.U
  dt_cs.io.mip            := 0.U
  dt_cs.io.mie            := 0.U
  dt_cs.io.mscratch       := 0.U
  dt_cs.io.sscratch       := 0.U
  dt_cs.io.mideleg        := 0.U
  dt_cs.io.medeleg        := 0.U

  val dt_irs = Module(new DifftestArchIntRegState)
  dt_irs.io.clock  := clock
  dt_irs.io.coreid := 0.U
  dt_irs.io.gpr := regfile.io.debugPorts




}
