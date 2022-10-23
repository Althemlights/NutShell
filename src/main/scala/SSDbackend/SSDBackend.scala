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
    val in = Vec(4, Flipped(Decoupled(new DecodeIO)))
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
  val PMU = Module(new PMU)
  val SSDcoretrap = WireInit(false.B)

  //pipeline interface
  val pipeIn = Wire(Vec(10,Flipped(Decoupled(new FuPkt))))
  val pipeOut = Wire(Vec(10,Decoupled(new FuPkt)))
  val pipeFire = Wire(Vec(10,Bool()))
  val pipeFlush = Wire(Vec(10,Bool()))
  val pipeInvalid = Wire(Vec(12,Bool()))

  val coupledPipeIn = Wire(Vec(4,Decoupled(new FuPkt)))
  val coupledPipeOut = Wire(Vec(4,Decoupled(new FuPkt)))


  //e1 -e5 register
  val pipeRegStage0 = Module(new stallPointConnect(new FuPkt)).suggestName("pipeStage0")
  val pipeRegStage1 = Module(new stallPointConnect(new FuPkt)).suggestName("pipeStage1")
  val pipeRegStage2 = Module(new normalPipeConnect(new FuPkt)).suggestName("pipeStage2")
  val pipeRegStage3 = Module(new normalPipeConnect(new FuPkt)).suggestName("pipeStage3")
  val pipeRegStage4 = Module(new normalPipeConnect(new FuPkt)).suggestName("pipeStage4")
  val pipeRegStage5 = Module(new normalPipeConnect(new FuPkt)).suggestName("pipeStage5")
  val pipeRegStage6 = Module(new stallPointConnect(new FuPkt)).suggestName("pipeStage6")
  val pipeRegStage7 = Module(new stallPointConnect(new FuPkt)).suggestName("pipeStage7")
  val pipeRegStage8 = Module(new normalPipeConnect(new FuPkt)).suggestName("pipeStage8")
  val pipeRegStage9 = Module(new normalPipeConnect(new FuPkt)).suggestName("pipeStage9")

  val coupledPipeRegStage6 = Module(new normalPipeConnect(new FuPkt)).suggestName("coupledPipeStage6")
  val coupledPipeRegStage7 = Module(new normalPipeConnect(new FuPkt)).suggestName("coupledPipeStage7")
  val coupledPipeRegStage8 = Module(new normalPipeConnect(new FuPkt)).suggestName("coupledPipeStage8")
  val coupledPipeRegStage9 = Module(new normalPipeConnect(new FuPkt)).suggestName("coupledPipeStage9")

  pipeFlush := Bypass.io.pipeFlush
  pipeInvalid := Bypass.io.pipeInvalid
  for(i <- 0 to 3){
    pipeFire(2*i) := pipeOut(2*i).valid && pipeIn(2*i+2).ready
    pipeFire(2*i+1) := pipeOut(2*i+1).valid && pipeIn(2*i+3).ready
  }
  pipeFire(8) := pipeOut(8).valid && pipeOut(8).ready
  pipeFire(9) := pipeOut(9).valid && pipeOut(9).ready

  //Bypass
  val memStall = Wire(Bool())
  val mduStall = Wire(Bool())
  Bypass.io.in(0) <> io.in(2)
  Bypass.io.in(1) <> io.in(3)
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
  //PMU
  PMU.io.in0 <> Bypass.io.pmuio
  PMU.io.coreTrap := SSDcoretrap

//  val Redirect2_csr = Wire(new RedirectIO)
//  val Redirect3_csr = Wire(new RedirectIO)
//  Redirect2_csr := 0.U.asTypeOf(new RedirectIO)
//  Redirect3_csr := 0.U.asTypeOf(new RedirectIO)
  val SSDCSR = Module(new SSDCSR)
  SSDCSR.io.out.ready := true.B
  val i0CSRValid = BypassPktValid(0) && (BypassPkt(0).decodePkt.csr) && (BypassPkt(0).decodePkt.skip)
  val i1CSRValid = BypassPktValid(1) && (BypassPkt(1).decodePkt.csr) && (BypassPkt(1).decodePkt.skip)
  val CSRValid = i0CSRValid || i1CSRValid
  val CSRfunc = Mux(i1CSRValid,pipeRegStage1.right.bits.fuOpType,pipeRegStage0.right.bits.fuOpType)
  val CSRsrc1 = Mux(i1CSRValid,pipeRegStage1.right.bits.rs1,pipeRegStage0.right.bits.rs1)
  val CSRsrc2 = Mux(i1CSRValid,pipeRegStage1.right.bits.rs2,pipeRegStage0.right.bits.rs2)
  SSDCSR.access(CSRValid,CSRsrc1,CSRsrc2,CSRfunc)
  SSDCSR.io.cfIn := 0.U.asTypeOf(new CtrlFlowIO)
  SSDCSR.io.isBackendException := false.B
  SSDCSR.io.instrValid := CSRValid
  when(i0CSRValid) {
    SSDCSR.io.cfIn.pc                   := pipeOut(0).bits.pc
    SSDCSR.io.cfIn.pnpc                 := pipeOut(0).bits.pnpc
    SSDCSR.io.cfIn.instr                := pipeOut(0).bits.instr
    SSDCSR.io.cfIn.brIdx                := pipeOut(0).bits.brIdx
    SSDCSR.io.cfIn.isRVC                := pipeOut(0).bits.isRVC
    SSDCSR.io.cfIn.isBranch             := pipeOut(0).bits.isBranch
    SSDCSR.io.cfIn.redirect.btbIsBranch := pipeOut(0).bits.btbIsBranch
    SSDCSR.io.cfIn.redirect.ghr         := pipeOut(0).bits.ghr
  }.elsewhen(i1CSRValid) {
    SSDCSR.io.cfIn.pc                   := pipeOut(1).bits.pc
    SSDCSR.io.cfIn.pnpc                 := pipeOut(1).bits.pnpc
    SSDCSR.io.cfIn.instr                := pipeOut(1).bits.instr
    SSDCSR.io.cfIn.brIdx                := pipeOut(1).bits.brIdx
    SSDCSR.io.cfIn.isRVC                := pipeOut(1).bits.isRVC
    SSDCSR.io.cfIn.isBranch             := pipeOut(1).bits.isBranch
    SSDCSR.io.cfIn.redirect.btbIsBranch := pipeOut(1).bits.btbIsBranch
    SSDCSR.io.cfIn.redirect.ghr         := pipeOut(1).bits.ghr
  }
//  when(RegNext(i0CSRValid)) {
//    Redirect2_csr := RegNext(RegNext(SSDCSR.io.redirect))
//    Redirect3_csr := 0.U.asTypeOf(new RedirectIO)
//  }.elsewhen(RegNext(i1CSRValid)) {
//    Redirect2_csr := 0.U.asTypeOf(new RedirectIO)
//    Redirect3_csr := RegNext(SSDCSR.io.redirect)
//  }.otherwise {
//    Redirect2_csr := 0.U.asTypeOf(new RedirectIO)
//    Redirect3_csr := 0.U.asTypeOf(new RedirectIO)
//  }
  //decode & issue & e0bypass
  //ALU & SUB_ALU
  val ALU_0 = Module(new ALU).suggestName("ALU0")
  val ALU_1 = Module(new ALU).suggestName("ALU1")
  val ALU_6 = Module(new ALU).suggestName("ALU6")
  val ALU_7 = Module(new ALU).suggestName("ALU7")
  val Redirect2 = Wire(new RedirectIO)
  val Redirect3 = Wire(new RedirectIO)
  val Redirect8 = Wire(new RedirectIO)
  val Redirect9 = Wire(new RedirectIO)
  val bpuUpdataReq0 = Wire(new BPUUpdateReq)
  val bpuUpdataReq1 = Wire(new BPUUpdateReq)
  val bpuUpdataReq6 = Wire(new BPUUpdateReq)
  val bpuUpdataReq7 = Wire(new BPUUpdateReq)
  val alu2pmu0 = Wire(new ALU2PMUIO)
  val alu2pmu1 = Wire(new ALU2PMUIO)
  val alu2pmu6 = Wire(new ALU2PMUIO)
  val alu2pmu7 = Wire(new ALU2PMUIO)
  val finalBpuUpdateReq = Wire(new BPUUpdateReq)
  val ALUList = List(ALU_0,ALU_1,ALU_6,ALU_7)
  val pipeOut2ALUList = List(pipeOut(0),pipeOut(1),pipeOut(6),pipeOut(7))
  val pipeOut2Redirect = List(pipeOut(2),pipeOut(3),pipeOut(8),pipeOut(9))
  val ALURedirectList = List(Redirect2,Redirect3,Redirect8,Redirect9)
  val bpuUpdateReqList = List(bpuUpdataReq0,bpuUpdataReq1,bpuUpdataReq6,bpuUpdataReq7)
  val alu2pmuList = List(alu2pmu0,alu2pmu1,alu2pmu6,alu2pmu7)
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
    a.io.cfIn.redirect.btbIsBranch := b.bits.btbIsBranch
    a.io.cfIn.redirect.ghr := b.bits.ghr
  }
  (ALURedirectList zip pipeOut2Redirect).foreach{ case(a,b) => a := b.bits.redirect}
  (bpuUpdateReqList zip ALUList).foreach{ case(a,b) => a := b.io.bpuUpdateReq}
  (alu2pmuList zip ALUList).foreach{ case(a,b) => a := b.io.alu2pmu}
  Bypass.io.flush(0) := (Redirect2.valid) && pipeOut(2).valid
  Bypass.io.flush(1) := (Redirect3.valid) && pipeOut(3).valid
  Bypass.io.flush(2) := Redirect8.valid && pipeOut(8).valid
  Bypass.io.flush(3) := Redirect9.valid && pipeOut(9).valid




  io.redirectOut := Mux(SSDCSR.io.redirect.valid,SSDCSR.io.redirect,
    Mux(Redirect9.valid &&  pipeOut(9).valid && !pipeInvalid(11),Redirect9,
      Mux(Redirect8.valid &&  pipeOut(8).valid && !pipeInvalid(10),Redirect8,
        Mux(Redirect3.valid && pipeOut(3).valid,Redirect3,
          Mux(Redirect2.valid && pipeOut(2).valid,Redirect2,0.U.asTypeOf(new RedirectIO))))))
  finalBpuUpdateReq := Mux(pipeOut(9).bits.bpuUpdateReq.valid && pipeOut(9).fire() && !pipeInvalid(11),pipeOut(9).bits.bpuUpdateReq,
    Mux(pipeOut(8).bits.bpuUpdateReq.valid && pipeOut(8).fire() && !pipeInvalid(10),pipeOut(8).bits.bpuUpdateReq,0.U.asTypeOf(new BPUUpdateReq)))
  BoringUtils.addSource(finalBpuUpdateReq, "bpuUpdateReq")
  //  BoringUtils.addSource(finalBpuUpdateReq, "ghrUpdateReq")
  BoringUtils.addSource(finalBpuUpdateReq.valid,"pmuUpdateCnt")
  dontTouch(finalBpuUpdateReq)

  val aluValid = VecInit(false.B,false.B,false.B,false.B)
  aluValid := Seq(
    pipeOut(0).valid && BypassPkt(0).decodePkt.alu && !BypassPkt(0).decodePkt.subalu,
    pipeOut(1).valid && BypassPkt(1).decodePkt.alu && !BypassPkt(1).decodePkt.subalu,
    pipeOut(6).valid && BypassPkt(6).decodePkt.alu && BypassPkt(6).decodePkt.subalu,
    pipeOut(7).valid && BypassPkt(7).decodePkt.alu && BypassPkt(7).decodePkt.subalu
  )

  ALU_0.access(pipeOut(0).valid && !BypassPkt(0).decodePkt.alu,aluValid(0),pipeOut(0).bits.rs1,pipeOut(0).bits.rs2,pipeOut(0).bits.fuOpType)
  ALU_1.access(pipeOut(1).valid && !BypassPkt(1).decodePkt.alu,aluValid(1),pipeOut(1).bits.rs1,pipeOut(1).bits.rs2,pipeOut(1).bits.fuOpType)
  ALU_6.access(false.B,
    aluValid(2),
    Mux(pipeOut(6).valid, pipeOut(6).bits.rs1,coupledPipeRegStage6.io.right.bits.rs1),
    Mux(pipeOut(6).valid, pipeOut(6).bits.rs2,coupledPipeRegStage6.io.right.bits.rs2),
    Mux(pipeOut(6).valid, pipeOut(6).bits.fuOpType,coupledPipeRegStage6.io.right.bits.fuOpType))
  ALU_7.access(false.B,
    aluValid(3),
    Mux(pipeOut(7).valid, pipeOut(7).bits.rs1,coupledPipeRegStage7.io.right.bits.rs1),
    Mux(pipeOut(7).valid, pipeOut(7).bits.rs2,coupledPipeRegStage7.io.right.bits.rs2),
    Mux(pipeOut(7).valid, pipeOut(7).bits.fuOpType,coupledPipeRegStage7.io.right.bits.fuOpType))

  //LSU
  val LSU = Module(new SSDLSU)
  io.dmem <> LSU.io.dmem
  dontTouch(io.dmem.resp.ready)
  LSU.io.out.ready := true.B//!(Redirect6.valid || Redirect7.valid)
  memStall := LSU.io.memStall
  LSU.io.storeBypassCtrl <> Bypass.io.LSUBypassCtrl.storeBypassCtrlE2
  val i0LSUValid = BypassPktValid(2) && (BypassPkt(2).decodePkt.load || BypassPkt(2).decodePkt.store)
  val i1LSUValid = BypassPktValid(3) && (BypassPkt(3).decodePkt.load || BypassPkt(3).decodePkt.store)
  //LSU flush
//  LSU.io.flush(0) := pipeOut(2).bits.redirect.valid && pipeOut(2).valid || pipeOut(3).bits.redirect.valid && pipeOut(3).valid ||
//    ALU_6.io.redirect.valid || ALU_7.io.redirect.valid
//  LSU.io.flush(1) := ALU_6.io.redirect.valid || ALU_7.io.redirect.valid
//  LSU.io.flush(2) := ALU_6.io.redirect.valid || ALU_7.io.redirect.valid
//  LSU.io.flush(3) := false.B
//  LSU.io.invalid(0) := false.B
//  LSU.io.invalid(1) := pipeOut(3).bits.redirect.valid && pipeOut(3).valid && (BypassPkt(2).decodePkt.load || BypassPkt(2).decodePkt.store)
//  LSU.io.invalid(2) := false.B
//  LSU.io.invalid(3) := ALU_7.io.redirect.valid && BypassPktValid(6) && BypassPkt(6).decodePkt.store
    LSU.io.flush(0) := ALU_6.io.redirect.valid || ALU_7.io.redirect.valid
    LSU.io.flush(1) := ALU_6.io.redirect.valid || ALU_7.io.redirect.valid
    LSU.io.flush(2) := false.B
    LSU.io.invalid(0) := pipeOut(3).bits.redirect.valid && pipeOut(3).valid && (BypassPkt(2).decodePkt.load || BypassPkt(2).decodePkt.store)
    LSU.io.invalid(1) := false.B
    LSU.io.invalid(2) := ALU_7.io.redirect.valid && BypassPktValid(6) && BypassPkt(6).decodePkt.store

  dontTouch(i0LSUValid)
  dontTouch(i1LSUValid)
  val LSUValid = i0LSUValid || i1LSUValid
  val LSUfunc = Mux(i1LSUValid,pipeRegStage3.right.bits.fuOpType,pipeRegStage2.right.bits.fuOpType)
  val LSUsrc1 = Mux(i1LSUValid,pipeRegStage3.right.bits.rs1,pipeRegStage2.right.bits.rs1)
  val LSUsrc2 = Mux(i1LSUValid,pipeRegStage3.right.bits.rs2,pipeRegStage2.right.bits.rs2)
  val LSUoffset = Mux(i1LSUValid,pipeRegStage3.right.bits.offset,pipeRegStage2.right.bits.offset)
  LSU.access(LSUValid,LSUsrc1,LSUsrc2,LSUfunc,LSUoffset)
  //MDU
  val MDU = Module(new SSDMDU)
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
  val ByPassEna = Wire(Vec(14,Bool()))
  ByPassEna := Seq(
    //e0
    BypassPktE0(0).bits.BypassCtl.rs1bypasse0.asUInt.orR,
    BypassPktE0(0).bits.BypassCtl.rs2bypasse0.asUInt.orR,
    BypassPktE0(1).bits.BypassCtl.rs1bypasse0.asUInt.orR,
    BypassPktE0(1).bits.BypassCtl.rs2bypasse0.asUInt.orR,
    //e1 
    //for src1 of load/store inst
    Bypass.io.LSUBypassCtrl.lsBypassCtrli0E1.asUInt.orR,
    Bypass.io.LSUBypassCtrl.lsBypassCtrli1E1.asUInt.orR,
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
  val BypassPortE2 = Wire(Vec(E2BypassPort,UInt(64.W)))
  val BypassPortE3 = Wire(Vec(E3BypassPort,UInt(64.W)))
  val lsuBypassPortE1 = Wire(Vec(E1StoreBypassPort,UInt(64.W)))
  val StoreBypassPortE2 = Wire(Vec(E2StoreBypassPort,UInt(64.W)))
  BypassPortE0 := Seq(pipeIn(2).bits.rd,pipeIn(3).bits.rd,pipeIn(4).bits.rd,pipeIn(5).bits.rd,
    pipeIn(6).bits.rd,pipeIn(7).bits.rd,LSU.io.out.bits,MDU.io.out.bits,
    pipeIn(8).bits.rd,pipeIn(9).bits.rd,pipeOut(8).bits.rd,pipeOut(9).bits.rd)
  BypassPortE2 := Seq(coupledPipeOut(2).bits.rd,coupledPipeOut(3).bits.rd)
  BypassPortE3 := Seq(coupledPipeIn(1).bits.rd,coupledPipeIn(2).bits.rd,coupledPipeIn(3).bits.rd,coupledPipeOut(2).bits.rd,coupledPipeOut(3).bits.rd)

  lsuBypassPortE1 := Seq(coupledPipeIn(0).bits.rd,coupledPipeIn(1).bits.rd,coupledPipeIn(2).bits.rd,coupledPipeIn(3).bits.rd)
  StoreBypassPortE2 := Seq(coupledPipeIn(0).bits.rd,coupledPipeIn(1).bits.rd,coupledPipeIn(2).bits.rd,coupledPipeIn(3).bits.rd,coupledPipeOut(2).bits.rd,coupledPipeOut(3).bits.rd)
  
  LSU.io.storeBypassPort <> StoreBypassPortE2
  dontTouch(BypassPortE0)
  dontTouch(BypassPortE2)
  dontTouch(BypassPortE3)
  dontTouch(lsuBypassPortE1)
  dontTouch(StoreBypassPortE2)
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
    pipeIn(i).bits.isBranch := ALUOpType.isBru(io.in(1-i).bits.ctrl.fuOpType)
    pipeIn(i).bits.bpuUpdateReq := 0.U.asTypeOf(new BPUUpdateReq)
    pipeIn(i).bits.alu2pmu := 0.U.asTypeOf(new ALU2PMUIO)
    pipeIn(i).bits.redirect := 0.U.asTypeOf(new RedirectIO)
    //for sub ALU
    pipeIn(i).bits.isSubALU := Bypass.io.decodeBypassPkt(i).bits.decodePkt.subalu
    //for MMIO
    pipeIn(i).bits.isMMIO := DontCare
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
    // for update ghr
    pipeIn(i).bits.ghr := io.in(1-i).bits.cf.redirect.ghr
    pipeIn(i).bits.btbIsBranch := io.in(1-i).bits.cf.redirect.btbIsBranch
    pipeIn(i).bits.branchTaken := DontCare
    //for difftest
    pipeIn(i).bits.CSRregfile := DontCare
    pipeIn(i).bits.ArchEvent := DontCare
  }

  for(i <- 2 to 9 ){
    pipeIn(i).bits := pipeOut(i - 2).bits
    pipeIn(i).valid := pipeOut(i - 2).valid
    pipeOut(i - 2).ready := pipeIn(i).ready
    //    }
  }

  coupledPipeIn(0).valid := pipeOut(4).valid
  coupledPipeIn(1).valid := pipeOut(5).valid
  coupledPipeIn(2).valid := coupledPipeOut(0).valid
  coupledPipeIn(3).valid := coupledPipeOut(1).valid
  coupledPipeIn(0).bits := pipeOut(4).bits
  coupledPipeIn(1).bits := pipeOut(5).bits
  coupledPipeIn(2).bits := coupledPipeOut(0).bits
  coupledPipeIn(3).bits := coupledPipeOut(1).bits
  coupledPipeOut(0).ready := coupledPipeIn(2).ready
  coupledPipeOut(1).ready := coupledPipeIn(3).ready
  coupledPipeOut(2).ready := !(memStall || mduStall)
  coupledPipeOut(3).ready := !(memStall || mduStall)

  pipeOut(8).ready := true.B
  pipeOut(9).ready := true.B


  //e1
  pipeIn(2).bits.rd := Mux(aluValid(0),ALU_0.io.out.bits,Mux(CSRValid,SSDCSR.io.out.bits,0.U(64.W)))
  pipeIn(3).bits.rd := Mux(aluValid(1),ALU_1.io.out.bits,Mux(CSRValid,SSDCSR.io.out.bits,0.U(64.W)))
//  pipeIn(2).bits.rd := Mux(CSRValid,SSDCSR.io.out.bits,Mux(aluValid(0),ALU_0.io.out.bits,0.U(64.W)))
//  pipeIn(3).bits.rd :=  Mux(CSRValid,SSDCSR.io.out.bits,Mux(aluValid(1),ALU_1.io.out.bits,0.U(64.W)))
  pipeIn(2).bits.rs1 := BypassMux(ByPassEna(4), BypassPkt(0).lsuCtrl.lsBypassCtrlE1,lsuBypassPortE1, pipeOut(0).bits.rs1)
  pipeIn(3).bits.rs1 := BypassMux(ByPassEna(5), BypassPkt(1).lsuCtrl.lsBypassCtrlE1,lsuBypassPortE1, pipeOut(1).bits.rs1)
  pipeIn(2).bits.branchTaken := Mux(aluValid(0),ALU_0.io.branchTaken,0.U(64.W))
  pipeIn(3).bits.branchTaken := Mux(aluValid(1),ALU_1.io.branchTaken,0.U(64.W))
  pipeIn(2).bits.bpuUpdateReq := Mux(bpuUpdataReq0.valid && pipeOut(0).valid,bpuUpdataReq0,0.U.asTypeOf(new BPUUpdateReq))
  pipeIn(3).bits.bpuUpdateReq := Mux(bpuUpdataReq1.valid && pipeOut(1).valid,bpuUpdataReq1,0.U.asTypeOf(new BPUUpdateReq))
  pipeIn(2).bits.redirect := Mux(SSDCSR.io.redirect.valid && i0CSRValid ,SSDCSR.io.redirect,Mux(ALU_0.io.redirect.valid && pipeOut(0).valid,ALU_0.io.redirect,0.U.asTypeOf(new RedirectIO)))
  pipeIn(3).bits.redirect := Mux(SSDCSR.io.redirect.valid && i1CSRValid ,SSDCSR.io.redirect,Mux(ALU_1.io.redirect.valid && pipeOut(1).valid,ALU_1.io.redirect,0.U.asTypeOf(new RedirectIO)))
  pipeIn(2).bits.alu2pmu := Mux(bpuUpdataReq0.valid && pipeOut(0).valid,alu2pmu0,0.U.asTypeOf(new ALU2PMUIO))
  pipeIn(3).bits.alu2pmu := Mux(bpuUpdataReq1.valid && pipeOut(1).valid,alu2pmu1,0.U.asTypeOf(new ALU2PMUIO))
  val mtvec_wire = WireInit(UInt(XLEN.W),0.U)
  val mcause_wire = WireInit(UInt(XLEN.W),0.U)
  val mepc_wire = WireInit(UInt(XLEN.W),0.U)
  val mstatus_wire = WireInit(UInt(XLEN.W),0.U)
  val mie_wire = WireInit(UInt(XLEN.W),0.U)
  val mtval_wire = WireInit(UInt(XLEN.W),0.U)
  val mscratch_wire = WireInit(UInt(XLEN.W),0.U)
  val mideleg_wire = WireInit(UInt(XLEN.W),0.U)
  val medeleg_wire = WireInit(UInt(XLEN.W),0.U)
  BoringUtils.addSink(mtvec_wire,"mtvec_wire")
  BoringUtils.addSink(mcause_wire,"mcause_wire")
  BoringUtils.addSink(mepc_wire,"mepc_wire")
  BoringUtils.addSink(mstatus_wire,"mstatus_wire")
  BoringUtils.addSink(mie_wire,"mie_wire")
  BoringUtils.addSink(mtval_wire,"mtval_wire")
  BoringUtils.addSink(mscratch_wire,"mscratch_wire")
  BoringUtils.addSink(mideleg_wire,"mideleg_wire")
  BoringUtils.addSink(medeleg_wire,"medeleg_wire")

  pipeIn(2).bits.CSRregfile :=  SSDCSR.io.CSRregfile
  pipeIn(3).bits.CSRregfile :=  SSDCSR.io.CSRregfile
  pipeIn(2).bits.CSRregfile.mtvec := mtvec_wire
  pipeIn(3).bits.CSRregfile.mtvec := mtvec_wire
  pipeIn(2).bits.CSRregfile.mcause := mcause_wire
  pipeIn(3).bits.CSRregfile.mcause := mcause_wire
  pipeIn(2).bits.CSRregfile.mepc := mepc_wire
  pipeIn(3).bits.CSRregfile.mepc := mepc_wire
  pipeIn(2).bits.CSRregfile.mstatus := mstatus_wire
  pipeIn(3).bits.CSRregfile.mstatus := mstatus_wire
  pipeIn(2).bits.CSRregfile.mie := mie_wire
  pipeIn(3).bits.CSRregfile.mie := mie_wire
  pipeIn(2).bits.CSRregfile.mtval := mtval_wire
  pipeIn(3).bits.CSRregfile.mtval := mtval_wire
  pipeIn(2).bits.CSRregfile.mscratch := mscratch_wire
  pipeIn(3).bits.CSRregfile.mscratch := mscratch_wire
  pipeIn(2).bits.CSRregfile.mideleg := mideleg_wire
  pipeIn(3).bits.CSRregfile.mideleg := mideleg_wire
  pipeIn(2).bits.CSRregfile.medeleg := medeleg_wire
  pipeIn(3).bits.CSRregfile.medeleg := medeleg_wire

  pipeIn(2).bits.ArchEvent :=  Mux(RegNext(CSRValid),Mux(RegNext(i0CSRValid),SSDCSR.io.ArchEvent,0.U.asTypeOf(new ArchEvent)),0.U.asTypeOf(new ArchEvent))
  pipeIn(3).bits.ArchEvent :=  Mux(RegNext(CSRValid),Mux(RegNext(i1CSRValid),SSDCSR.io.ArchEvent,0.U.asTypeOf(new ArchEvent)),0.U.asTypeOf(new ArchEvent))
  //e2
  pipeIn(4).bits.rs1 := BypassMux(ByPassEna(6), BypassPkt(2).BypassCtl.rs1bypasse2,BypassPortE2, pipeOut(2).bits.rs1)
  pipeIn(4).bits.rs2 := BypassMux(ByPassEna(7), BypassPkt(2).BypassCtl.rs2bypasse2,BypassPortE2, pipeOut(2).bits.rs2)
  pipeIn(5).bits.rs1 := BypassMux(ByPassEna(8), BypassPkt(3).BypassCtl.rs1bypasse2,BypassPortE2, pipeOut(3).bits.rs1)
  pipeIn(5).bits.rs2 := BypassMux(ByPassEna(9), BypassPkt(3).BypassCtl.rs2bypasse2,BypassPortE2, pipeOut(3).bits.rs2)

  //e3
  pipeIn(6).bits.rd := Mux(BypassPkt(4).decodePkt.load,LSU.io.out.bits,Mux(BypassPkt(4).decodePkt.muldiv,MDU.io.out.bits,pipeOut(4).bits.rd))
  pipeIn(6).bits.rs1 := BypassMux(ByPassEna(10), BypassPkt(4).BypassCtl.rs1bypasse3,BypassPortE3, pipeOut(4).bits.rs1)
  pipeIn(6).bits.rs2 := BypassMux(ByPassEna(11), BypassPkt(4).BypassCtl.rs2bypasse3,BypassPortE3, pipeOut(4).bits.rs2)
  pipeIn(6).bits.isMMIO := Mux(BypassPkt(4).decodePkt.load || BypassPkt(4).decodePkt.store,LSU.io.isMMIO,false.B)
  pipeIn(7).bits.rd := Mux(BypassPkt(5).decodePkt.load,LSU.io.out.bits,Mux(BypassPkt(5).decodePkt.muldiv,MDU.io.out.bits,pipeOut(5).bits.rd))
  pipeIn(7).bits.rs1 := BypassMux(ByPassEna(12), BypassPkt(5).BypassCtl.rs1bypasse3,BypassPortE3, pipeOut(5).bits.rs1)
  pipeIn(7).bits.rs2 := BypassMux(ByPassEna(13), BypassPkt(5).BypassCtl.rs2bypasse3,BypassPortE3, pipeOut(5).bits.rs2)
  pipeIn(7).bits.isMMIO := Mux(BypassPkt(5).decodePkt.load || BypassPkt(5).decodePkt.store,LSU.io.isMMIO,false.B)

  coupledPipeIn(0).bits.rd := Mux(BypassPkt(4).decodePkt.load,LSU.io.out.bits,Mux(BypassPkt(4).decodePkt.muldiv,MDU.io.out.bits,pipeOut(4).bits.rd))
  coupledPipeIn(0).bits.rs1 := BypassMux(ByPassEna(10), BypassPkt(4).BypassCtl.rs1bypasse3,BypassPortE3, pipeOut(4).bits.rs1)
  coupledPipeIn(0).bits.rs2 := BypassMux(ByPassEna(11), BypassPkt(4).BypassCtl.rs2bypasse3,BypassPortE3, pipeOut(4).bits.rs2)
  coupledPipeIn(1).bits.rd := Mux(BypassPkt(5).decodePkt.load,LSU.io.out.bits,Mux(BypassPkt(5).decodePkt.muldiv,MDU.io.out.bits,pipeOut(5).bits.rd))
  coupledPipeIn(1).bits.rs1 := BypassMux(ByPassEna(12), BypassPkt(5).BypassCtl.rs1bypasse3,BypassPortE3, pipeOut(5).bits.rs1)
  coupledPipeIn(1).bits.rs2 := BypassMux(ByPassEna(13), BypassPkt(5).BypassCtl.rs2bypasse3,BypassPortE3, pipeOut(5).bits.rs2)

  //e4
  pipeIn(8).bits.rd := Mux(aluValid(2),ALU_6.io.out.bits,pipeOut(6).bits.rd)
  pipeIn(9).bits.rd := Mux(aluValid(3),ALU_7.io.out.bits,pipeOut(7).bits.rd)
  pipeIn(8).bits.branchTaken := Mux(aluValid(2),ALU_6.io.branchTaken,pipeOut(6).bits.branchTaken)
  pipeIn(9).bits.branchTaken := Mux(aluValid(3),ALU_7.io.branchTaken,pipeOut(7).bits.branchTaken)
  pipeIn(8).bits.redirect := Mux(ALU_6.io.redirect.valid && pipeOut(6).valid,ALU_6.io.redirect,0.U.asTypeOf(new RedirectIO))
  pipeIn(9).bits.redirect := Mux(ALU_7.io.redirect.valid && pipeOut(7).valid,ALU_7.io.redirect,0.U.asTypeOf(new RedirectIO))
  pipeIn(8).bits.bpuUpdateReq := Mux(bpuUpdataReq6.valid && pipeOut(6).valid, bpuUpdataReq6, pipeOut(6).bits.bpuUpdateReq)
  pipeIn(9).bits.bpuUpdateReq := Mux(bpuUpdataReq7.valid && pipeOut(7).valid, bpuUpdataReq7, pipeOut(7).bits.bpuUpdateReq)
  pipeIn(8).bits.alu2pmu := Mux(bpuUpdataReq6.valid && pipeOut(6).valid, alu2pmu6, pipeOut(6).bits.alu2pmu)
  pipeIn(9).bits.alu2pmu := Mux(bpuUpdataReq7.valid && pipeOut(7).valid, alu2pmu7, pipeOut(7).bits.alu2pmu)

  coupledPipeIn(2).bits.rd := Mux(coupledPipeOut(0).bits.isSubALU,ALU_6.io.out.bits,pipeOut(6).bits.rd)
  coupledPipeIn(3).bits.rd := Mux(coupledPipeOut(1).bits.isSubALU,ALU_7.io.out.bits,pipeOut(7).bits.rd)

  //e5 write back
  //e5 write back
  //regfile
  regfile.io.writePorts(0).wen := BypassPktValid(8) && BypassPkt(8).decodePkt.rdvalid && !pipeInvalid(10)
  regfile.io.writePorts(0).addr := BypassPkt(8).decodePkt.rd
  regfile.io.writePorts(0).data := pipeOut(8).bits.rd
  regfile.io.writePorts(1).wen := BypassPktValid(9) && BypassPkt(9).decodePkt.rdvalid && !pipeInvalid(11)
  regfile.io.writePorts(1).addr := BypassPkt(9).decodePkt.rd
  regfile.io.writePorts(1).data := pipeOut(9).bits.rd


  //i1rs1,i1rs2,i0rs1,i0rs2
  regfile.io.readPorts(0).addr := io.in(1).bits.ctrl.rfSrc1
  regfile.io.readPorts(1).addr := io.in(1).bits.ctrl.rfSrc2
  regfile.io.readPorts(2).addr := io.in(0).bits.ctrl.rfSrc1
  regfile.io.readPorts(3).addr := io.in(0).bits.ctrl.rfSrc2

  // for debug
  val lsuPC =WireInit(0.U(VAddrBits.W))
  lsuPC := Mux(BypassPkt(3).decodePkt.load || BypassPkt(3).decodePkt.store, pipeOut(3).bits.pc, pipeOut(2).bits.pc)
  BoringUtils.addSource(lsuPC,"lsuPC")

  //moduleTest
  //  val moduleTest = Module(new ModuleTest)
  //pipe connect

  val stallStageList = List(pipeRegStage0,pipeRegStage1,pipeRegStage6,pipeRegStage7)
  val stallIndexList = List(0,1,6,7)
  (stallStageList zip stallIndexList).foreach{case (a,b) =>
    a.io.left <> pipeIn(b)
    a.io.right <> pipeOut(b)
    a.io.rightOutFire <> pipeFire(b)
    a.io.isFlush <> pipeFlush(b)
    a.io.inValid <> pipeInvalid(b)
  }
  pipeRegStage0.io.isStall := issueStall(0)
  pipeRegStage1.io.isStall := issueStall(1)
  pipeRegStage6.io.isStall := memStall || mduStall
  pipeRegStage7.io.isStall := memStall || mduStall

  val normalStageList = List(pipeRegStage2,pipeRegStage3,pipeRegStage4,pipeRegStage5,pipeRegStage8,pipeRegStage9)
  val normalIndexList = List(2,3,4,5,8,9)

  (normalStageList zip normalIndexList).foreach{case (a,b) =>
    a.io.left <> pipeIn(b)
    a.io.right <> pipeOut(b)
    a.io.rightOutFire <> pipeFire(b)
    a.io.isFlush <> pipeFlush(b)
    a.io.inValid <> pipeInvalid(b)
  }

  val coupledStageList = List(coupledPipeRegStage6,coupledPipeRegStage7,coupledPipeRegStage8,coupledPipeRegStage9)
  val coupledIndexList = List(0,1,2,3)

  (coupledStageList zip coupledIndexList).foreach{case (a,b) =>
    a.io.left <> coupledPipeIn(b)
    a.io.right <> coupledPipeOut(b)
    a.io.rightOutFire <> coupledPipeOut(b).fire()
    a.io.isFlush <> false.B
    a.io.inValid <> false.B
  }

  //GHR commit
  val GHRCommit = RegInit(0.U(GhrLength.W))
  val i0IsBranch = pipeOut(8).fire() && !pipeInvalid(10) && pipeOut(8).bits.isBranch && ALUOpType.isBranch(pipeOut(8).bits.fuOpType)
  val i1IsBranch = pipeOut(9).fire() && !pipeInvalid(11) && pipeOut(9).bits.isBranch && ALUOpType.isBranch(pipeOut(9).bits.fuOpType)
  val GHRCommitUpdate = i0IsBranch || i1IsBranch
  val nextGHRCommit = Cat(GHRCommit(GhrLength-2,0),Mux(i0IsBranch,pipeOut(8).bits.branchTaken,pipeOut(9).bits.branchTaken))
  when(GHRCommitUpdate){ GHRCommit := nextGHRCommit}
  val branchInstPC = Mux(GHRCommitUpdate,Mux(i0IsBranch,pipeOut(8).bits.pc,pipeOut(9).bits.pc),0.U)
  val ghrCompared = Mux(i0IsBranch,pipeOut(8).bits.bpuUpdateReq.ghrNotUpdated,Mux(i1IsBranch,pipeOut(9).bits.bpuUpdateReq.ghrNotUpdated, 0.U))
  val ghrTag = Mux(i0IsBranch,pipeOut(8).bits.bpuUpdateReq.ghrNotUpdated =/= GHRCommit,
    Mux(i1IsBranch,pipeOut(9).bits.bpuUpdateReq.ghrNotUpdated =/= GHRCommit,false.B))
  //assert(RegNext(!ghrTag))
  dontTouch(branchInstPC)
  dontTouch(nextGHRCommit)
  dontTouch(GHRCommit)
  if(SSDCoreConfig().EnableGHRDebug){
    myDebug(GHRCommitUpdate,"pc = %x, taken: %b, GHR now: %b, GHR compared %b\n",
      branchInstPC,Mux(i0IsBranch,pipeOut(8).bits.branchTaken,pipeOut(9).bits.branchTaken).asUInt,GHRCommit,ghrCompared)
  }
  dontTouch(ghrTag)
  dontTouch(ghrCompared)
  //Call/Ret Debug
  val i0Call = pipeOut(8).fire() && !pipeInvalid(10) && ALUOpType.call === pipeOut(8).bits.fuOpType
  val i1Call = pipeOut(9).fire() && !pipeInvalid(11) && ALUOpType.call === pipeOut(9).bits.fuOpType
  val i0Ret = pipeOut(8).fire() && !pipeInvalid(10) && ALUOpType.ret === pipeOut(8).bits.fuOpType
  val i1Ret = pipeOut(9).fire() && !pipeInvalid(11) && ALUOpType.ret === pipeOut(9).bits.fuOpType
  val CallCond = i0Call || i1Call
  val RetCond = i0Ret || i1Ret
  dontTouch(CallCond)
  dontTouch(RetCond)
  val CallPC = Mux(CallCond,Mux(i0Call,pipeOut(8).bits.pc,pipeOut(9).bits.pc),0.U)
  val RetPC = Mux(RetCond,Mux(i0Ret,pipeOut(8).bits.pc,pipeOut(9).bits.pc),0.U)
  val RetWrong = RetCond &&  Mux(i0Ret,pipeOut(8).bits.alu2pmu.retWrong,pipeOut(9).bits.alu2pmu.retWrong)
  dontTouch(RetWrong)
  if(SSDCoreConfig().EnableRetDebug){
    myDebug(CallCond,"Call: pc = %x, Targe = %x \n",
      CallPC,Mux(i0Call,pipeOut(8).bits.bpuUpdateReq.actualTarget,pipeOut(9).bits.bpuUpdateReq.actualTarget).asUInt)
  }
  if(SSDCoreConfig().EnableRetDebug){
    myDebug(RetCond,"Ret : pc = %x, Targe = %x, isMisPredict = %b \n",
      RetPC,Mux(i0Ret,pipeOut(8).bits.bpuUpdateReq.actualTarget,pipeOut(9).bits.bpuUpdateReq.actualTarget).asUInt,
      Mux(i0Ret,pipeOut(8).bits.alu2pmu.retWrong,pipeOut(9).bits.alu2pmu.retWrong).asUInt)
  }
  //PMU perfCnt signal
  val perfCntIO = Wire(new PMUIO1)
  PMU.io.in1 <> perfCntIO

  perfCntIO.branchRight := ( pipeOut(8).fire() && !pipeInvalid(10) && pipeOut(8).bits.alu2pmu.branchRight).asUInt + ( pipeOut(9).fire() && !pipeInvalid(11) && pipeOut(9).bits.alu2pmu.branchRight).asUInt
  perfCntIO.branchWrong := ( pipeOut(8).fire() && !pipeInvalid(10) && pipeOut(8).bits.alu2pmu.branchWrong).asUInt + ( pipeOut(9).fire() && !pipeInvalid(11) && pipeOut(9).bits.alu2pmu.branchWrong).asUInt
  perfCntIO.jalRight    := ( pipeOut(8).fire() && !pipeInvalid(10) && pipeOut(8).bits.alu2pmu.jalRight).asUInt    + ( pipeOut(9).fire() && !pipeInvalid(11) && pipeOut(9).bits.alu2pmu.jalRight).asUInt
  perfCntIO.jalWrong    := ( pipeOut(8).fire() && !pipeInvalid(10) && pipeOut(8).bits.alu2pmu.jalWrong).asUInt    + ( pipeOut(9).fire() && !pipeInvalid(11) && pipeOut(9).bits.alu2pmu.jalWrong).asUInt
  perfCntIO.jalrRight   := ( pipeOut(8).fire() && !pipeInvalid(10) && pipeOut(8).bits.alu2pmu.jalrRight).asUInt   + ( pipeOut(9).fire() && !pipeInvalid(11) && pipeOut(9).bits.alu2pmu.jalrRight).asUInt
  perfCntIO.jalrWrong   := ( pipeOut(8).fire() && !pipeInvalid(10) && pipeOut(8).bits.alu2pmu.jalrWrong).asUInt   + ( pipeOut(9).fire() && !pipeInvalid(11) && pipeOut(9).bits.alu2pmu.jalrWrong).asUInt
  perfCntIO.retRight    := ( pipeOut(8).fire() && !pipeInvalid(10) && pipeOut(8).bits.alu2pmu.retRight).asUInt    + ( pipeOut(9).fire() && !pipeInvalid(11) && pipeOut(9).bits.alu2pmu.retRight).asUInt
  perfCntIO.retWrong    := ( pipeOut(8).fire() && !pipeInvalid(10) && pipeOut(8).bits.alu2pmu.retWrong).asUInt    + ( pipeOut(9).fire() && !pipeInvalid(11) && pipeOut(9).bits.alu2pmu.retWrong).asUInt
  perfCntIO.branchTargetWrong    := ( pipeOut(8).fire() && !pipeInvalid(10) && pipeOut(8).bits.alu2pmu.branchTargetWrong).asUInt    + ( pipeOut(9).fire() && !pipeInvalid(11) && pipeOut(9).bits.alu2pmu.branchTargetWrong).asUInt
  perfCntIO.branchDirectionWrong := ( pipeOut(8).fire() && !pipeInvalid(10) && pipeOut(8).bits.alu2pmu.branchDirectionWrong).asUInt + ( pipeOut(9).fire() && !pipeInvalid(11) && pipeOut(9).bits.alu2pmu.branchDirectionWrong).asUInt
  //PMU instCnt signal
  val instIssueCntIO = Wire(new PMUIO2)
  val instCommitCntIO = Wire(new PMUIO2)
  PMU.io.in2Issue <> instIssueCntIO
  PMU.io.in2Commit <> instCommitCntIO
  instIssueCntIO.branchInst := (pipeOut(0).fire() && pipeOut(0).bits.isBranch && ALUOpType.isBranch(pipeOut(0).bits.fuOpType)).asUInt +
    (pipeOut(1).fire() && pipeOut(1).bits.isBranch && ALUOpType.isBranch(pipeOut(1).bits.fuOpType)).asUInt
  instIssueCntIO.jalInst := (pipeOut(0).fire() && pipeOut(0).bits.isBranch && (ALUOpType.jal === pipeOut(0).bits.fuOpType || ALUOpType.call === pipeOut(0).bits.fuOpType)).asUInt +
    (pipeOut(1).fire() && pipeOut(1).bits.isBranch && (ALUOpType.jal === pipeOut(1).bits.fuOpType || ALUOpType.call === pipeOut(1).bits.fuOpType)).asUInt
  instIssueCntIO.jalrInst := (pipeOut(0).fire() && pipeOut(0).bits.isBranch && ALUOpType.jalr === pipeOut(0).bits.fuOpType).asUInt +
    (pipeOut(1).fire() && pipeOut(1).bits.isBranch && ALUOpType.jalr === pipeOut(1).bits.fuOpType).asUInt
  instIssueCntIO.retInst := (pipeOut(0).fire() && pipeOut(0).bits.isBranch && ALUOpType.ret === pipeOut(0).bits.fuOpType).asUInt +
    (pipeOut(1).fire() && pipeOut(1).bits.isBranch && ALUOpType.ret === pipeOut(1).bits.fuOpType).asUInt
  instIssueCntIO.loadInst := (pipeOut(0).fire() && BypassPktValid(0) && BypassPkt(0).decodePkt.load).asUInt +
    (pipeOut(1).fire() && BypassPktValid(1) && BypassPkt(1).decodePkt.load).asUInt
  instIssueCntIO.storeInst := (pipeOut(0).fire() && BypassPktValid(0) && BypassPkt(0).decodePkt.store).asUInt +
    (pipeOut(1).fire() && BypassPktValid(1) && BypassPkt(1).decodePkt.store).asUInt
  instIssueCntIO.mulInst := (pipeOut(0).fire()  && !MDUOpType.isDiv(pipeOut(0).bits.fuOpType) && BypassPktValid(0) && BypassPkt(0).decodePkt.muldiv).asUInt +
    (pipeOut(1).fire() && !MDUOpType.isDiv(pipeOut(1).bits.fuOpType) && BypassPktValid(1) && BypassPkt(1).decodePkt.muldiv).asUInt
  instIssueCntIO.divInst := (pipeOut(0).fire()  && MDUOpType.isDiv(pipeOut(0).bits.fuOpType) && BypassPktValid(0) && BypassPkt(0).decodePkt.muldiv).asUInt +
    (pipeOut(1).fire() && MDUOpType.isDiv(pipeOut(1).bits.fuOpType) && BypassPktValid(1) && BypassPkt(1).decodePkt.muldiv).asUInt

  instCommitCntIO.branchInst := ( pipeOut(8).fire() && !pipeInvalid(10) && pipeOut(8).bits.isBranch && ALUOpType.isBranch(pipeOut(8).bits.fuOpType)).asUInt +
    ( pipeOut(9).fire() && !pipeInvalid(11) && pipeOut(9).bits.isBranch && ALUOpType.isBranch(pipeOut(9).bits.fuOpType)).asUInt
  instCommitCntIO.jalInst := ( pipeOut(8).fire() && !pipeInvalid(10) && pipeOut(8).bits.isBranch && (ALUOpType.jal === pipeOut(8).bits.fuOpType || ALUOpType.call === pipeOut(8).bits.fuOpType)).asUInt +
    ( pipeOut(9).fire() && !pipeInvalid(11) && pipeOut(9).bits.isBranch && (ALUOpType.jal === pipeOut(9).bits.fuOpType || ALUOpType.call === pipeOut(9).bits.fuOpType)).asUInt
  instCommitCntIO.jalrInst := ( pipeOut(8).fire() && !pipeInvalid(10) && pipeOut(8).bits.isBranch && ALUOpType.jalr === pipeOut(8).bits.fuOpType).asUInt +
    ( pipeOut(9).fire() && !pipeInvalid(11) && pipeOut(9).bits.isBranch && ALUOpType.jalr === pipeOut(9).bits.fuOpType).asUInt
  instCommitCntIO.retInst := ( pipeOut(8).fire() && !pipeInvalid(10) && pipeOut(8).bits.isBranch && ALUOpType.ret === pipeOut(8).bits.fuOpType).asUInt +
    ( pipeOut(9).fire() && !pipeInvalid(11) && pipeOut(9).bits.isBranch && ALUOpType.ret === pipeOut(9).bits.fuOpType).asUInt
  instCommitCntIO.loadInst := ( pipeOut(8).fire() && !pipeInvalid(10) && BypassPktValid(8) && BypassPkt(8).decodePkt.load).asUInt +
    ( pipeOut(9).fire() && !pipeInvalid(11) && BypassPktValid(9) && BypassPkt(9).decodePkt.load).asUInt
  instCommitCntIO.storeInst := ( pipeOut(8).fire() && !pipeInvalid(10) && BypassPktValid(8) && BypassPkt(8).decodePkt.store).asUInt +
    ( pipeOut(9).fire() && !pipeInvalid(11) && BypassPktValid(9) && BypassPkt(9).decodePkt.store).asUInt
  instCommitCntIO.mulInst := ( pipeOut(8).fire() && !pipeInvalid(10)  && !MDUOpType.isDiv(pipeOut(8).bits.fuOpType) && BypassPktValid(8) && BypassPkt(8).decodePkt.muldiv).asUInt +
    ( pipeOut(9).fire() && !pipeInvalid(11) && !MDUOpType.isDiv(pipeOut(9).bits.fuOpType) && BypassPktValid(9) && BypassPkt(9).decodePkt.muldiv).asUInt
  instCommitCntIO.divInst := ( pipeOut(8).fire() && !pipeInvalid(10)  && MDUOpType.isDiv(pipeOut(8).bits.fuOpType) && BypassPktValid(8) && BypassPkt(8).decodePkt.muldiv).asUInt +
    ( pipeOut(9).fire() && !pipeInvalid(11) && MDUOpType.isDiv(pipeOut(9).bits.fuOpType) && BypassPktValid(9) && BypassPkt(9).decodePkt.muldiv).asUInt



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
  if(SSDCoreConfig().EnablePipestageDebug){
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

  BoringUtils.addSource((pipeOut(8).bits.instr === "h0000006b".U || pipeOut(8).bits.instr === "h0005006b".U) &&  pipeOut(8).fire() && !pipeInvalid(10) ||
    (pipeOut(9).bits.instr === "h0000006b".U || pipeOut(9).bits.instr === "h0005006b".U) &&  pipeOut(9).fire() && !pipeInvalid(11),"SSDcoretrap")
  BoringUtils.addSink(SSDcoretrap,"SSDcoretrap")
  //inst flag for mtvec
  val mtvecFlag = Reg(Bool())
  dontTouch(mtvecFlag)
  when((pipeOut(8).bits.instr === "h30571073".U) &&  pipeOut(8).fire() && !pipeInvalid(10) ||
    (pipeOut(9).bits.instr === "h30571073".U) &&  pipeOut(9).fire() && !pipeInvalid(11)){
    mtvecFlag := true.B
  }.otherwise{
    mtvecFlag := false.B
  }

  //putch for ysyx SoC
  val a0 = WireInit(0.U(64.W))
  val a0Wen0 = (pipeOut(8).bits.instr === 0x7b.U) && pipeOut(8).fire() && !pipeInvalid(10) && BypassPkt(8).decodePkt.rdvalid &&
    BypassPkt(8).decodePkt.rd === "ha".U && regfile.io.writePorts(0).wen
  val a0Wen1 = (pipeOut(9).bits.instr === 0x7b.U) && pipeOut(9).fire() && !pipeInvalid(11) && BypassPkt(9).decodePkt.rdvalid &&
    BypassPkt(9).decodePkt.rd  === "ha".U && regfile.io.writePorts(1).wen
  a0 := Mux(a0Wen0,pipeOut(8).bits.rd,Mux(a0Wen1,pipeOut(9).bits.rd,regfile.io.mem(10)))

  when((pipeOut(8).bits.instr === 0x7b.U) &&  pipeOut(8).fire() && !pipeInvalid(10) ||
    (pipeOut(9).bits.instr === 0x7b.U) &&  pipeOut(9).fire() && !pipeInvalid(11)){
    printf("%c",a0.asUInt)
  }
//  when(RegNext((pipeOut(0).bits.instr === 0x7b.U) || (pipeOut(1).bits.instr === 0x7b.U))) {
//    printf("y")
//  }

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
  val cycle_cnt = RegInit(0.U(64.W))
  val instr_cnt = RegInit(0.U(64.W))

  cycle_cnt := cycle_cnt + 1.U
  instr_cnt := instr_cnt + RegNext(pipeOut(8).fire() && !pipeInvalid(10)).asUInt + RegNext(pipeOut(9).fire() && !pipeInvalid(11)).asUInt
  PMU.io.cycleCnt := cycle_cnt

  val rf_a0 = WireInit(0.U(64.W))
  BoringUtils.addSink(rf_a0, "rf_a0")

  if(SSDCoreConfig().EnableDifftest) {
    val dt_ic1 = Module(new DifftestInstrCommit)
    dt_ic1.io.clock := clock
    dt_ic1.io.coreid := 0.U
    dt_ic1.io.index := 0.U
    dt_ic1.io.valid := RegNext(pipeOut(9).fire() && !pipeInvalid(11) && pipeOut(9).bits.pc =/= 0.U) && !RegNext(SSDcoretrap)
    dt_ic1.io.pc := RegNext(Cat(0.U((64 - VAddrBits).W), pipeOut(9).bits.pc))
    dt_ic1.io.instr := RegNext(pipeOut(9).bits.instr)
    dt_ic1.io.special := 0.U
//    dt_ic1.io.skip := (RegNext(pipeOut(9).fire() && !pipeInvalid(11) && ((pipeOut(9).bits.csrInst && pipeOut(8).bits.instr=/=0x73.U && pipeOut(9).bits.instr=/=0x30571073.U && pipeOut(9).bits.instr=/=0x30031073.U && pipeOut(9).bits.instr =/=0x34139073.U && pipeOut(9).bits.instr =/= 0x30200073.U) || pipeOut(9).bits.isMMIO))) || RegNext(pipeOut(9).bits.instr === 0x7b.U)
    dt_ic1.io.skip := (RegNext(pipeOut(9).fire() && !pipeInvalid(11) && (pipeOut(9).bits.isMMIO))) || RegNext(pipeOut(9).bits.instr === 0x7b.U) ||
      RegNext(pipeOut(9).bits.instr(6,0) === "hb0002973".U(6,0) && pipeOut(9).bits.instr(31,12) === "hb0002973".U(31,12)) //trap & csrr mcycle
    dt_ic1.io.isRVC := false.B
    dt_ic1.io.scFailed := false.B
    dt_ic1.io.wen := RegNext(regfile.io.writePorts(1).wen)
    dt_ic1.io.wpdest := RegNext(Cat(0.U(3.W), regfile.io.writePorts(1).addr))
    dt_ic1.io.wdest := RegNext(Cat(0.U(3.W), regfile.io.writePorts(1).addr))

    val dt_ic0 = Module(new DifftestInstrCommit)
    dt_ic0.io.clock := clock
    dt_ic0.io.coreid := 0.U
    dt_ic0.io.index := 1.U

    dt_ic0.io.valid := RegNext(pipeOut(8).fire() && !pipeInvalid(10) && pipeOut(8).bits.pc =/= 0.U)
    dt_ic0.io.pc := RegNext(Cat(0.U((64 - VAddrBits).W), pipeOut(8).bits.pc))
    dt_ic0.io.instr := RegNext(pipeOut(8).bits.instr)
    dt_ic0.io.special := 0.U
//    dt_ic0.io.skip := (RegNext(pipeOut(8).fire() && !pipeInvalid(10) && ((pipeOut(8).bits.csrInst && pipeOut(8).bits.instr=/=0x73.U && pipeOut(8).bits.instr=/=0x30571073.U && pipeOut(8).bits.instr=/=0x30031073.U && pipeOut(8).bits.instr =/=0x34139073.U && pipeOut(8).bits.instr =/= 0x30200073.U) || pipeOut(8).bits.isMMIO))) || RegNext(pipeOut(8).bits.instr === 0x7b.U)
    dt_ic0.io.skip := (RegNext(pipeOut(8).fire() && !pipeInvalid(10) && (pipeOut(8).bits.isMMIO))) || RegNext(pipeOut(8).bits.instr === 0x7b.U) ||
      RegNext(pipeOut(8).bits.instr(6,0) === "hb0002973".U(6,0) && pipeOut(8).bits.instr(31,12) === "hb0002973".U(31,12))
    dt_ic0.io.isRVC := false.B
    dt_ic0.io.scFailed := false.B
    dt_ic0.io.wen := RegNext(regfile.io.writePorts(0).wen)
    dt_ic0.io.wpdest := RegNext(Cat(0.U(3.W), regfile.io.writePorts(0).addr))
    dt_ic0.io.wdest := RegNext(Cat(0.U(3.W), regfile.io.writePorts(0).addr))


    val dt_iw0 = Module(new DifftestIntWriteback)
    dt_iw0.io.clock := clock
    dt_iw0.io.coreid := 0.U
    dt_iw0.io.valid := RegNext(regfile.io.writePorts(1).wen)
    dt_iw0.io.dest := RegNext(regfile.io.writePorts(1).addr)
    dt_iw0.io.data := RegNext(regfile.io.writePorts(1).data)


    val dt_iw1 = Module(new DifftestIntWriteback)
    dt_iw1.io.clock := clock
    dt_iw1.io.coreid := 0.U
    dt_iw1.io.valid := RegNext(regfile.io.writePorts(0).wen)
    dt_iw1.io.dest := RegNext(regfile.io.writePorts(0).addr)
    dt_iw1.io.data := RegNext(regfile.io.writePorts(0).data)


    val dt_ae = Module(new DifftestArchEvent)
    dt_ae.io.clock := clock
    dt_ae.io.coreid :=      0.U
    dt_ae.io.intrNO :=      RegNext(Mux(pipeOut(8).bits.csrInst,pipeOut(8).bits.ArchEvent.intrNO,pipeOut(9).bits.ArchEvent.intrNO))
    dt_ae.io.cause :=       RegNext(Mux(pipeOut(8).bits.csrInst,pipeOut(8).bits.ArchEvent.cause,pipeOut(9).bits.ArchEvent.cause))
    dt_ae.io.exceptionPC := RegNext(Mux(pipeOut(8).bits.csrInst,pipeOut(8).bits.ArchEvent.exceptionPC,pipeOut(9).bits.ArchEvent.exceptionPC))

    val dt_te = Module(new DifftestTrapEvent)
    dt_te.io.clock := clock
    dt_te.io.coreid := 0.U
    dt_te.io.valid := RegNext(SSDcoretrap)
    dt_te.io.code := rf_a0(2, 0)
    dt_te.io.pc := Mux(RegNext(pipeOut(8).bits.instr === "h0000006b".U), RegNext(Cat(0.U((64 - VAddrBits).W), pipeOut(8).bits.pc)), RegNext(Cat(0.U((64 - VAddrBits).W), pipeOut(9).bits.pc)))
    dt_te.io.cycleCnt := cycle_cnt
    dt_te.io.instrCnt := instr_cnt

    val dt_cs = Module(new DifftestCSRState)
    dt_cs.io.clock := clock
    dt_cs.io.coreid := 0.U
    dt_cs.io.priviledgeMode := 3.U // Machine mode
    dt_cs.io.mstatus :=  RegNext(Mux(pipeOut(8).bits.csrInst,pipeOut(8).bits.CSRregfile.mstatus ,pipeOut(9).bits.CSRregfile.mstatus ))
    dt_cs.io.sstatus :=  RegNext(Mux(pipeOut(8).bits.csrInst,pipeOut(8).bits.CSRregfile.sstatus ,pipeOut(9).bits.CSRregfile.sstatus ))
    dt_cs.io.mepc :=     RegNext(Mux(pipeOut(8).bits.csrInst,pipeOut(8).bits.CSRregfile.mepc    ,pipeOut(9).bits.CSRregfile.mepc    ))
    dt_cs.io.sepc :=     RegNext(Mux(pipeOut(8).bits.csrInst,pipeOut(8).bits.CSRregfile.sepc    ,pipeOut(9).bits.CSRregfile.sepc    ))
    dt_cs.io.mtval :=    RegNext(Mux(pipeOut(8).bits.csrInst,pipeOut(8).bits.CSRregfile.mtval   ,pipeOut(9).bits.CSRregfile.mtval   ))
    dt_cs.io.stval :=    RegNext(Mux(pipeOut(8).bits.csrInst,pipeOut(8).bits.CSRregfile.stval   ,pipeOut(9).bits.CSRregfile.stval   ))
    dt_cs.io.mtvec :=    RegNext(Mux(pipeOut(8).bits.csrInst,pipeOut(8).bits.CSRregfile.mtvec   ,pipeOut(9).bits.CSRregfile.mtvec   ))
    dt_cs.io.stvec :=    RegNext(Mux(pipeOut(8).bits.csrInst,pipeOut(8).bits.CSRregfile.stvec   ,pipeOut(9).bits.CSRregfile.stvec   ))
    dt_cs.io.mcause :=   RegNext(Mux(pipeOut(8).bits.csrInst,pipeOut(8).bits.CSRregfile.mcause  ,pipeOut(9).bits.CSRregfile.mcause  ))
    dt_cs.io.scause :=   RegNext(Mux(pipeOut(8).bits.csrInst,pipeOut(8).bits.CSRregfile.scause  ,pipeOut(9).bits.CSRregfile.scause  ))
    dt_cs.io.satp :=     RegNext(Mux(pipeOut(8).bits.csrInst,pipeOut(8).bits.CSRregfile.satp    ,pipeOut(9).bits.CSRregfile.satp    ))
    dt_cs.io.mip :=      RegNext(Mux(pipeOut(8).bits.csrInst,pipeOut(8).bits.CSRregfile.mip     ,pipeOut(9).bits.CSRregfile.mip     ))
    dt_cs.io.mie :=      RegNext(Mux(pipeOut(8).bits.csrInst,pipeOut(8).bits.CSRregfile.mie     ,pipeOut(9).bits.CSRregfile.mie     ))
    dt_cs.io.mscratch := RegNext(Mux(pipeOut(8).bits.csrInst,pipeOut(8).bits.CSRregfile.mscratch,pipeOut(9).bits.CSRregfile.mscratch))
    dt_cs.io.sscratch := RegNext(Mux(pipeOut(8).bits.csrInst,pipeOut(8).bits.CSRregfile.sscratch,pipeOut(9).bits.CSRregfile.sscratch))
    dt_cs.io.mideleg :=  RegNext(Mux(pipeOut(8).bits.csrInst,pipeOut(8).bits.CSRregfile.mideleg ,pipeOut(9).bits.CSRregfile.mideleg ))
    dt_cs.io.medeleg :=  RegNext(Mux(pipeOut(8).bits.csrInst,pipeOut(8).bits.CSRregfile.medeleg ,pipeOut(9).bits.CSRregfile.medeleg ))

    val dt_irs = Module(new DifftestArchIntRegState)
    dt_irs.io.clock := clock
    dt_irs.io.coreid := 0.U
    dt_irs.io.gpr := regfile.io.debugPorts
  }



}
