package SSDbackend

import chisel3._
import chisel3.util._
import difftest._
import utils.{PipelineConnect, SignExt}
import nutcore._
import chisel3.util.experimental.BoringUtils


class SSDbackend extends Module with hasBypassConst {
  val io = IO(new Bundle{
    val in = Vec(2, Flipped(Decoupled(new DecodeIO)))
  })
  def BypassMux(sel:Bool,BypassCtl:Vec[Bool],BypassDataPort:Vec[UInt],rdata:UInt):UInt ={
    Mux(sel,Mux1H(BypassCtl,BypassDataPort),rdata)
}

  //new
  val Bypass = Module(new Bypass)
  val regfile = Module(new SSDRF)

  //pipeline interface
  val pipeIn = Wire(Vec(10,Decoupled(new FuPkt)))
  val pipeOut = Wire(Vec(10,Decoupled(new FuPkt)))
  val pipeFire = Wire(Vec(10,Bool()))
  val pipeFlush = Wire(Vec(10,Bool()))

  pipeFlush := Bypass.io.pipeFlush
  for(i <- 0 to 3){
    pipeFire(2*i) := pipeOut(2*i).valid && pipeIn(2*i+2).ready
    pipeFire(2*i+1) := pipeOut(2*i+1).valid && pipeIn(2*i+3).ready
  }
  pipeFire(8) := true.B
  pipeFire(9) := true.B

  //Bypass
  val memStall = Wire(Bool())
  memStall := false.B
  Bypass.io.in <> io.in
  Bypass.io.memStall := memStall
  Bypass.io.flush := VecInit(Seq.fill(4)(false.B))
  val issueStall = VecInit(false.B,false.B)
  issueStall :=Bypass.io.issueStall
  val BypassPkt = Wire(Vec(10,new BypassPkt))
  val BypassPktE0 = Wire(Vec(2,Decoupled(new BypassPkt)))
  val BypassPktValid = Wire(Vec(10,Bool()))
  BypassPkt := Bypass.io.BypassPkt
  BypassPktE0 := Bypass.io.decodeBypassPkt
  BypassPktValid := Bypass.io.BypassPktValid

  Bypass.io.decodeBypassPkt(0).ready := pipeIn(0).ready
  Bypass.io.decodeBypassPkt(1).ready := pipeIn(1).ready
  BypassPktE0(0).ready := pipeIn(0).ready
  BypassPktE0(1).ready := pipeIn(1).ready

  //decode & issue & e0bypass
  //FuDataOut
  val LSUData = 0.U(64.W)
  val MDUData = 0.U(64.W)
  //ALU & SUB_ALU
  val ALU_0 = Module(new ALU)
  val ALU_1 = Module(new ALU)
  val ALU_4 = Module(new ALU)
  val ALU_5 = Module(new ALU)
  //alu io
  ALU_0.io.cfIn := 0.U.asTypeOf(new CtrlFlowIO)
  ALU_1.io.cfIn := 0.U.asTypeOf(new CtrlFlowIO)
  ALU_4.io.cfIn := 0.U.asTypeOf(new CtrlFlowIO)
  ALU_5.io.cfIn := 0.U.asTypeOf(new CtrlFlowIO)
  ALU_0.io.offset := pipeOut(0).bits.offset
  ALU_1.io.offset := pipeOut(1).bits.offset
  ALU_4.io.offset := pipeOut(4).bits.offset
  ALU_5.io.offset := pipeOut(5).bits.offset
  ALU_0.io.out.ready := true.B
  ALU_1.io.out.ready := true.B
  ALU_4.io.out.ready := true.B
  ALU_5.io.out.ready := true.B

  val aluResult = Wire(Vec(4,UInt(64.W)))
  val aluValid = VecInit(false.B,false.B,false.B,false.B)
  aluValid := Seq(
    pipeOut(0).valid && BypassPkt(0).decodePkt.alu && !BypassPkt(0).decodePkt.subalu,
    pipeOut(1).valid && BypassPkt(1).decodePkt.alu && !BypassPkt(1).decodePkt.subalu,
    pipeOut(6).valid && BypassPkt(6).decodePkt.alu && BypassPkt(6).decodePkt.subalu,
    pipeOut(7).valid && BypassPkt(7).decodePkt.alu && BypassPkt(7).decodePkt.subalu
  )

  aluResult(0) := ALU_0.access(aluValid(0),pipeOut(0).bits.rs1,pipeOut(0).bits.rs2,pipeOut(0).bits.fuOpType)
  aluResult(1) := ALU_1.access(aluValid(1),pipeOut(1).bits.rs1,pipeOut(1).bits.rs2,pipeOut(1).bits.fuOpType)
  aluResult(2) := ALU_4.access(aluValid(2),pipeOut(6).bits.rs1,pipeOut(6).bits.rs2,pipeOut(6).bits.fuOpType)
  aluResult(3) := ALU_5.access(aluValid(3),pipeOut(7).bits.rs1,pipeOut(7).bits.rs2,pipeOut(7).bits.fuOpType)


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

  val ByapssPortE0 = Wire(Vec(E0BypaaPort,UInt(64.W)))
  ByapssPortE0 := Seq(aluResult(0),aluResult(1),pipeOut(2).bits.rd,pipeOut(3).bits.rd,
    pipeOut(4).bits.rd,pipeOut(5).bits.rd,0.U,0.U,
    aluResult(2),aluResult(3),pipeOut(8).bits.rd,pipeOut(9).bits.rd)

  val ByapssPortE2 = Wire(Vec(E2BypaaPort,UInt(64.W)))
  ByapssPortE2 := Seq(pipeOut(8).bits.rd,pipeOut(9).bits.rd)
  val ByapssPortE3 = Wire(Vec(E3BypaaPort,UInt(64.W)))
  ByapssPortE3 := Seq(pipeOut(5).bits.rd,pipeIn(8).bits.rd,pipeIn(9).bits.rd,pipeOut(8).bits.rd,pipeOut(9).bits.rd)

  //decode & issue
  //rs1 data type: pc, regfile or bypass
  //rs2 data type: imm, regfilw or bypass
  val e0ByapssRs1 = VecInit(0.U(64.W),0.U(64.W))
  val e0ByapssRs2 = VecInit(0.U(64.W),0.U(64.W))
  e0ByapssRs1(0) := BypassMux(ByPassEna(0), BypassPktE0(0).bits.BypassCtl.rs1bypasse0,ByapssPortE0, regfile.io.readPorts(0).data)
  e0ByapssRs1(1) := BypassMux(ByPassEna(2), BypassPktE0(1).bits.BypassCtl.rs1bypasse0,ByapssPortE0, regfile.io.readPorts(2).data)
  e0ByapssRs2(0) := BypassMux(ByPassEna(1), BypassPktE0(0).bits.BypassCtl.rs2bypasse0,ByapssPortE0, regfile.io.readPorts(1).data)
  e0ByapssRs2(1) := BypassMux(ByPassEna(3), BypassPktE0(1).bits.BypassCtl.rs2bypasse0,ByapssPortE0, regfile.io.readPorts(3).data)
 //myDebug(pipeIn(0).bits.pc === "h8000003c".U,"pipeIn(0) pc: %x, rs1Bypasse0: %b,rs1Bypass data: %x",pipeIn(0).bits.pc,BypassPktE0(0).bits.BypassCtl.rs1bypasse0.asUInt,e0ByapssRs1(0))

  pipeIn(0).valid := io.in(1).valid
  pipeIn(1).valid := io.in(0).valid
  pipeIn(0).ready := pipeOut(0).ready
  pipeIn(1).ready := pipeOut(1).ready

  pipeIn(0).bits.rd := io.in(1).bits.ctrl.rfDest
  pipeIn(1).bits.rd := io.in(0).bits.ctrl.rfDest
  pipeIn(0).bits.rs1 := Mux(io.in(1).bits.ctrl.src1Type === SrcType.pc,
    SignExt(io.in(1).bits.cf.pc, 64),e0ByapssRs1(0))
  pipeIn(0).bits.rs2 := Mux(io.in(1).bits.ctrl.src2Type =/= SrcType.reg,
    io.in(1).bits.data.imm,e0ByapssRs2(0))
  pipeIn(1).bits.rs1 := Mux(io.in(0).bits.ctrl.src1Type === SrcType.pc,
    SignExt(io.in(0).bits.cf.pc, 64),e0ByapssRs1(1))
  pipeIn(1).bits.rs2 := Mux(io.in(0).bits.ctrl.src2Type =/= SrcType.reg,
    io.in(0).bits.data.imm,e0ByapssRs2(1))

  //myDebug(true.B,"debugTest %b\n",BypassPktE0(1).bits.BypassCtl.rs1bypasse0.asUInt)

  pipeIn(0).bits.fuOpType := io.in(1).bits.ctrl.fuOpType
  pipeIn(1).bits.fuOpType := io.in(0).bits.ctrl.fuOpType
  pipeIn(0).bits.offset := io.in(1).bits.data.imm
  pipeIn(1).bits.offset := io.in(0).bits.data.imm
  pipeIn(0).bits.pc := io.in(1).bits.cf.pc
  pipeIn(1).bits.pc := io.in(0).bits.cf.pc
  pipeIn(0).bits.instr := io.in(1).bits.cf.instr
  pipeIn(1).bits.instr := io.in(0).bits.cf.instr
  for(i <- 2 to 9 ){
    pipeIn(i).bits := pipeOut(i - 2).bits
    pipeIn(i).valid := pipeOut(i - 2).valid
    pipeOut(i - 2).ready := pipeIn(i).ready
  }
  pipeOut(8).ready := true.B
  pipeOut(9).ready := true.B

  //e1_reg
//  StallPointConnect(pipeIn(0),pipeOut(0),pipeFire(0),pipeFlush(0),issueStall(0))
//  StallPointConnect(pipeIn(1),pipeOut(1),pipeFire(1),pipeFlush(1),issueStall(1))
  //e1
  pipeIn(2).bits.rd := Mux(aluValid(0),aluResult(0),0.U(64.W))
  pipeIn(3).bits.rd := Mux(aluValid(1),aluResult(1),0.U(64.W))
  //e2_reg
//  PipelineConnect(pipeIn(2),pipeOut(2),pipeFire(2),pipeFlush(2))
//  PipelineConnect(pipeIn(3),pipeOut(3),pipeFire(3),pipeFlush(3))
  //e2
  pipeIn(4).bits.rs1 := BypassMux(ByPassEna(4), BypassPkt(2).BypassCtl.rs1bypasse2,ByapssPortE2, pipeOut(2).bits.rs1)
  pipeIn(4).bits.rs2 := BypassMux(ByPassEna(5), BypassPkt(2).BypassCtl.rs2bypasse2,ByapssPortE2, pipeOut(2).bits.rs2)
  pipeIn(5).bits.rs1 := BypassMux(ByPassEna(6), BypassPkt(3).BypassCtl.rs1bypasse2,ByapssPortE2, pipeOut(3).bits.rs1)
  pipeIn(5).bits.rs2 := BypassMux(ByPassEna(7), BypassPkt(3).BypassCtl.rs2bypasse2,ByapssPortE2, pipeOut(3).bits.rs2)
  //e3_reg
//  StallPointConnect(pipeIn(4),pipeOut(4),pipeFire(4),pipeFlush(4),memStall)
//  StallPointConnect(pipeIn(5),pipeOut(5),pipeFire(5),pipeFlush(5),memStall)
  //e3
  pipeIn(6).bits.rd := pipeOut(4).bits.rd  //加入其他Fu后要修改
  pipeIn(6).bits.rs1 := BypassMux(ByPassEna(8), BypassPkt(4).BypassCtl.rs1bypasse3,ByapssPortE3, pipeOut(4).bits.rs1)
  pipeIn(6).bits.rs2 := BypassMux(ByPassEna(9), BypassPkt(4).BypassCtl.rs2bypasse3,ByapssPortE3, pipeOut(4).bits.rs2)
  pipeIn(7).bits.rd := pipeOut(5).bits.rd
  pipeIn(7).bits.rs1 := BypassMux(ByPassEna(10), BypassPkt(5).BypassCtl.rs1bypasse3,ByapssPortE3, pipeOut(5).bits.rs1)
  pipeIn(7).bits.rs2 := BypassMux(ByPassEna(11), BypassPkt(5).BypassCtl.rs2bypasse3,ByapssPortE3, pipeOut(5).bits.rs2)
  //e4_reg
//  PipelineConnect(pipeIn(6),pipeOut(6),pipeFire(6),pipeFlush(6))
//  PipelineConnect(pipeIn(7),pipeOut(7),pipeFire(7),pipeFlush(7))
  //e4
  pipeIn(8).bits.rd := Mux(aluValid(2),aluResult(2),pipeOut(6).bits.rd)
  pipeIn(9).bits.rd := Mux(aluValid(3),aluResult(3),pipeOut(7).bits.rd)
  val cond = Wire(Bool())
  cond := pipeOut(6).bits.pc === "h8000008c".U
  myDebug(cond,"aluValid id:%b\n",aluValid(2).asUInt)
  //e5_reg
//  PipelineConnect(pipeIn(8),pipeOut(8),pipeFire(8),pipeFlush(8))
//  PipelineConnect(pipeIn(9),pipeOut(9),pipeFire(9),pipeFlush(9))
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

  //e1 -e5 register
  val pipeRegStage0 = Module(new stallPointConnect(new FuPkt))
  val pipeRegStage1 = Module(new stallPointConnect(new FuPkt))
  val pipeRegStage4 = Module(new stallPointConnect(new FuPkt))
  val pipeRegStage5 = Module(new stallPointConnect(new FuPkt))
  val pipeRegStage2 = Module(new normalPipeConnect(new FuPkt))
  val pipeRegStage3 = Module(new normalPipeConnect(new FuPkt))
  val pipeRegStage6 = Module(new normalPipeConnect(new FuPkt))
  val pipeRegStage7 = Module(new normalPipeConnect(new FuPkt))
  val pipeRegStage8 = Module(new normalPipeConnect(new FuPkt))
  val pipeRegStage9 = Module(new normalPipeConnect(new FuPkt))

  val stallStageList = List(pipeRegStage0,pipeRegStage1,pipeRegStage4,pipeRegStage5)
  val stallList = List(0,1,4,5)
  (stallStageList zip stallList).foreach{case (a,b) =>
    a.io.left <> pipeIn(b)
    a.io.right <> pipeOut(b)
    a.io.rightOutFire <> pipeFire(b)
    a.io.isFlush <> pipeFlush(b)
  }
  pipeRegStage0.io.isStall := issueStall(0)
  pipeRegStage1.io.isStall := issueStall(1)
  pipeRegStage4.io.isStall := memStall
  pipeRegStage5.io.isStall := memStall


  val normalStageList = List(pipeRegStage2,pipeRegStage3,pipeRegStage6,pipeRegStage7,pipeRegStage8,pipeRegStage9)
  val normalList = List(2,3,6,7,8,9)

  (normalStageList zip normalList).foreach{case (a,b) =>
    a.io.left <> pipeIn(b)
    a.io.right <> pipeOut(b)
    a.io.rightOutFire <> pipeFire(b)
    a.io.isFlush <> pipeFlush(b)
  }

  /* ----- Difftest ----- */

  val dt_ic1 = Module(new DifftestInstrCommit)
  dt_ic1.io.clock    := clock
  dt_ic1.io.coreid   := 0.U
  dt_ic1.io.index    := 0.U
  dt_ic1.io.valid    := RegNext(pipeOut(9).valid)
  dt_ic1.io.pc       := RegNext(pipeOut(9).bits.pc)
  dt_ic1.io.instr    := RegNext(pipeOut(9).bits.instr)
  dt_ic1.io.special  := 0.U
  dt_ic1.io.skip     := false.B
  dt_ic1.io.isRVC    := false.B
  dt_ic1.io.scFailed := false.B
  dt_ic1.io.wen      := RegNext(regfile.io.writePorts(1).wen)
  dt_ic1.io.wpdest   := RegNext(Cat(0.U(3.W),regfile.io.writePorts(1).addr))
  dt_ic1.io.wdest    := RegNext(Cat(0.U(3.W),regfile.io.writePorts(1).addr))

  val dt_ic0 = Module(new DifftestInstrCommit)
  dt_ic0.io.clock    := clock
  dt_ic0.io.coreid   := 0.U
  dt_ic0.io.index    := 1.U

  dt_ic0.io.valid    := RegNext(pipeOut(8).valid)
  dt_ic0.io.pc       := RegNext(pipeOut(8).bits.pc)
  dt_ic0.io.instr    := RegNext(pipeOut(8).bits.instr)
  dt_ic0.io.special  := 0.U
  dt_ic0.io.skip     := false.B
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
  dt_te.io.valid    := RegNext(pipeOut(8).bits.instr === "h0000006b".U) || RegNext(pipeOut(9).bits.instr === "h0000006b".U)
  dt_te.io.code     := rf_a0(2, 0)
  dt_te.io.pc       := Mux(RegNext(pipeOut(8).bits.instr === "h0000006b".U),RegNext(pipeOut(8).bits.pc),RegNext(pipeOut(9).bits.pc))
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
