package SSDbackend

import chisel3._
import chisel3.util._
import difftest._
import utils.PipelineConnect
import nutcore._


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
    pipeFire(2*i) := pipeOut(2*i).valid && pipeIn(2*i).ready
    pipeFire(2*i+1) := pipeOut(2*i+1).valid && pipeIn(2*i+1).ready
  }
  pipeFire(8) := pipeOut(6).valid
  pipeFire(9) := pipeOut(7).valid

  //Bypass
  val memStall = Wire(Bool())
  memStall := false.B
  Bypass.io.in := io.in
  Bypass.io.memStall := memStall
  Bypass.io.flush := VecInit(Seq.fill(4)(false.B))
  val issueStall = VecInit(false.B,false.B)
  issueStall :=Bypass.io.issueStall
  val BypassPkt = Wire(Vec(10,Decoupled(new BypassPkt)))
  val BypassPktE0 = Wire(Vec(2,Decoupled(new BypassPkt)))
  BypassPkt := Bypass.io.BypassPkt
  BypassPktE0 := Bypass.io.decodeBypassPkt

  //decode & issue & e0bypass
  //FuDataOut
  val LSUData = 0.U(64.W)
  val MDUData = 0.U(64.W)
  //ALU & SUB_ALU
  val ALU_0 = Module(new ALU)
  val ALU_1 = Module(new ALU)
  val ALU_4 = Module(new ALU)
  val ALU_5 = Module(new ALU)
  val aluResult = Wire(Vec(4,UInt(64.W)))
  val aluValid = VecInit(false.B,false.B,false.B,false.B)
  aluValid := Seq(
    pipeOut(0).valid && BypassPkt(0).bits.decodePkt.alu && !BypassPkt(0).bits.decodePkt.subalu,
    pipeOut(1).valid && BypassPkt(1).bits.decodePkt.alu && !BypassPkt(1).bits.decodePkt.subalu,
    pipeOut(6).valid && BypassPkt(6).bits.decodePkt.alu && BypassPkt(6).bits.decodePkt.subalu,
    pipeOut(7).valid && BypassPkt(7).bits.decodePkt.alu && BypassPkt(7).bits.decodePkt.subalu
  )

  aluResult(0) := ALU_0.access(aluValid(0),pipeOut(0).bits.rs1,pipeOut(0).bits.rs2,pipeOut(0).bits.fuOpType)
  aluResult(1) := ALU_0.access(aluValid(1),pipeOut(1).bits.rs1,pipeOut(1).bits.rs2,pipeOut(1).bits.fuOpType)
  aluResult(2) := ALU_0.access(aluValid(2),pipeOut(6).bits.rs1,pipeOut(6).bits.rs2,pipeOut(6).bits.fuOpType)
  aluResult(3) := ALU_0.access(aluValid(3),pipeOut(7).bits.rs1,pipeOut(7).bits.rs2,pipeOut(7).bits.fuOpType)


  //Bypass signal and data port
  val ByPassEna = Wire(Vec(12,Bool()))
  ByPassEna := Seq(
    //e0
    BypassPktE0(0).bits.BypassCtl.rs1bypasse0.asUInt.orR,
    BypassPktE0(0).bits.BypassCtl.rs2bypasse0.asUInt.orR,
    BypassPktE0(1).bits.BypassCtl.rs1bypasse0.asUInt.orR,
    BypassPktE0(1).bits.BypassCtl.rs2bypasse0.asUInt.orR,
    //e2
    BypassPkt(2).bits.BypassCtl.rs1bypasse2.asUInt.orR,
    BypassPkt(2).bits.BypassCtl.rs2bypasse2.asUInt.orR,
    BypassPkt(3).bits.BypassCtl.rs1bypasse2.asUInt.orR,
    BypassPkt(3).bits.BypassCtl.rs2bypasse2.asUInt.orR,
    //e3
    BypassPkt(4).bits.BypassCtl.rs1bypasse3.asUInt.orR,
    BypassPkt(4).bits.BypassCtl.rs2bypasse3.asUInt.orR,
    BypassPkt(5).bits.BypassCtl.rs1bypasse3.asUInt.orR,
    BypassPkt(5).bits.BypassCtl.rs2bypasse3.asUInt.orR

  )

  val ByapssPortE0 = Wire(Vec(E0BypaaPort,UInt(64.W)))
  ByapssPortE0 := Seq(aluResult(0),aluResult(1),pipeOut(2).bits.rd,pipeOut(3).bits.rd,
    pipeOut(4).bits.rd,pipeOut(5).bits.rd,0.U,0.U,
    aluResult(2),aluResult(3),pipeOut(8).bits.rd,pipeOut(9).bits.rd)

  val ByapssPortE2 = Wire(Vec(E2BypaaPort,UInt(64.W)))
  ByapssPortE2 := Seq(pipeOut(8).bits.rd,pipeOut(9).bits.rd)
  val ByapssPortE3 = Wire(Vec(E3BypaaPort,UInt(64.W)))
  ByapssPortE3 := Seq(pipeOut(5).bits.rd,pipeOut(6).bits.rd,pipeOut(7).bits.rd,pipeOut(8).bits.rd,pipeOut(9).bits.rd)

  //decode & issue

  pipeIn(0).valid := io.in(0).valid
  pipeIn(1).valid := io.in(1).valid
  pipeIn(0).ready := pipeOut(0).ready
  pipeIn(1).ready := pipeOut(1).ready

  pipeIn(0).bits.rd := 0.U(64.W)
  pipeIn(0).bits.rs1 := BypassMux(ByPassEna(0), BypassPktE0(0).bits.BypassCtl.rs1bypasse0,ByapssPortE0, regfile.io.rdata(0))
  pipeIn(0).bits.rs2 := BypassMux(ByPassEna(1), BypassPktE0(0).bits.BypassCtl.rs2bypasse0,ByapssPortE0, regfile.io.rdata(1))
  pipeIn(1).bits.rd := 0.U(64.W)
  pipeIn(1).bits.rs1 := BypassMux(ByPassEna(2), BypassPktE0(1).bits.BypassCtl.rs1bypasse0,ByapssPortE0, regfile.io.rdata(2))
  pipeIn(1).bits.rs2 := BypassMux(ByPassEna(3), BypassPktE0(1).bits.BypassCtl.rs2bypasse0,ByapssPortE0, regfile.io.rdata(3))


  pipeIn(0).bits.fuOpType := io.in(0).bits.ctrl.fuOpType
  pipeIn(1).bits.fuOpType := io.in(1).bits.ctrl.fuOpType
  pipeIn(0).bits.offset := io.in(0).bits.data.imm
  pipeIn(1).bits.offset := io.in(1).bits.data.imm
  pipeIn(0).bits.pc := io.in(0).bits.cf.pc
  pipeIn(1).bits.pc := io.in(1).bits.cf.pc
  pipeIn(0).bits.instr := io.in(0).bits.cf.instr
  pipeIn(1).bits.instr := io.in(1).bits.cf.instr

  //e1_reg
  StallPointConnect(pipeIn(0),pipeOut(0),pipeFire(0),pipeFlush(0),issueStall(0))
  StallPointConnect(pipeIn(1),pipeOut(1),pipeFire(1),pipeFlush(1),issueStall(1))
  //e1
  pipeIn(2).bits.rd := Mux(aluValid(0),aluResult(0),0.U(64.W))
  pipeIn(3).bits.rd := Mux(aluValid(1),aluResult(1),0.U(64.W))
  //e2_reg
  PipelineConnect(pipeIn(2),pipeOut(2),pipeFire(2),pipeFlush(2))
  PipelineConnect(pipeIn(3),pipeOut(3),pipeFire(3),pipeFlush(3))
  //e2
  pipeIn(4).bits.rd := pipeIn(2).bits.rd
  pipeIn(4).bits.rs1 := BypassMux(ByPassEna(4), BypassPkt(2).bits.BypassCtl.rs1bypasse2,ByapssPortE2, pipeOut(2).bits.rs1)
  pipeIn(4).bits.rs2 := BypassMux(ByPassEna(5), BypassPkt(2).bits.BypassCtl.rs1bypasse2,ByapssPortE2, pipeOut(2).bits.rs1)
  pipeIn(5).bits.rd := pipeIn(3).bits.rd
  pipeIn(5).bits.rs1 := BypassMux(ByPassEna(6), BypassPkt(3).bits.BypassCtl.rs1bypasse2,ByapssPortE2, pipeOut(3).bits.rs1)
  pipeIn(5).bits.rs2 := BypassMux(ByPassEna(7), BypassPkt(3).bits.BypassCtl.rs1bypasse2,ByapssPortE2, pipeOut(3).bits.rs1)
  //e3_reg
  StallPointConnect(pipeIn(4),pipeOut(4),pipeFire(4),pipeFlush(4),memStall)
  StallPointConnect(pipeIn(5),pipeOut(5),pipeFire(5),pipeFlush(5),memStall)
  //e3
  pipeIn(6).bits.rd := pipeIn(4).bits.rd  //加入其他Fu后要修改
  pipeIn(6).bits.rs1 := BypassMux(ByPassEna(8), BypassPkt(4).bits.BypassCtl.rs1bypasse2,ByapssPortE2, pipeOut(4).bits.rs1)
  pipeIn(6).bits.rs2 := BypassMux(ByPassEna(9), BypassPkt(4).bits.BypassCtl.rs1bypasse2,ByapssPortE2, pipeOut(4).bits.rs1)
  pipeIn(7).bits.rd := pipeIn(5).bits.rd
  pipeIn(7).bits.rs1 := BypassMux(ByPassEna(10), BypassPkt(5).bits.BypassCtl.rs1bypasse2,ByapssPortE2, pipeOut(5).bits.rs1)
  pipeIn(7).bits.rs2 := BypassMux(ByPassEna(11), BypassPkt(5).bits.BypassCtl.rs1bypasse2,ByapssPortE2, pipeOut(5).bits.rs1)
  //e4_reg
  PipelineConnect(pipeIn(6),pipeOut(6),pipeFire(6),pipeFlush(6))
  PipelineConnect(pipeIn(7),pipeOut(7),pipeFire(7),pipeFlush(7))
  //e4
  pipeIn(8).bits.rd := Mux(aluValid(2),aluResult(2),0.U(64.W))
  pipeIn(9).bits.rd := Mux(aluValid(3),aluResult(3),0.U(64.W))
  //e5_reg
  PipelineConnect(pipeIn(8),pipeOut(8),pipeFire(8),pipeFlush(8))
  PipelineConnect(pipeIn(9),pipeOut(9),pipeFire(9),pipeFlush(9))
  //e5 write back
  //regfile
  regfile.io.wen := Seq(BypassPkt(8).valid && BypassPkt(8).bits.decodePkt.rdvalid,
    BypassPkt(9).valid && BypassPkt(9).bits.decodePkt.rdvalid)
  regfile.io.waddr := Seq(BypassPkt(8).bits.decodePkt.rd, BypassPkt(9).bits.decodePkt.rd)
  regfile.io.wdata := Seq(pipeOut(8).bits.rd,pipeOut(9).bits.rd)
  //i1rs1,i1rs2,i0rs1,i0rs2
  regfile.io.raddr := Seq(io.in(0).bits.ctrl.rfSrc1,io.in(0).bits.ctrl.rfSrc2,io.in(1).bits.ctrl.rfSrc1,io.in(1).bits.ctrl.rfSrc2)

  /* ----- Difftest ------------------------------ */

  val dt_ic0 = Module(new DifftestInstrCommit)
  dt_ic0.io.clock    := clock
  dt_ic0.io.coreid   := 0.U
  dt_ic0.io.index    := 0.U

  dt_ic0.io.valid    := RegNext(pipeOut(8).valid)
  dt_ic0.io.pc       := RegNext(pipeOut(8).bits.pc)
  dt_ic0.io.instr    := RegNext(pipeOut(8).bits.instr)
  dt_ic0.io.special  := 0.U
  dt_ic0.io.skip     := false.B
  dt_ic0.io.isRVC    := false.B
  dt_ic0.io.scFailed := false.B
  dt_ic0.io.wen      := RegNext(regfile.io.wen(0))
  dt_ic0.io.wpdest   := RegNext(Cat(0.U(3.W),regfile.io.waddr(0)))
  dt_ic0.io.wdest    := RegNext(Cat(0.U(3.W),regfile.io.waddr(0)))

  val dt_ic1 = Module(new DifftestInstrCommit)
  dt_ic1.io.clock    := clock
  dt_ic1.io.coreid   := 0.U
  dt_ic1.io.index    := 0.U
  dt_ic1.io.valid    := RegNext(pipeOut(8).valid)
  dt_ic1.io.pc       := RegNext(pipeOut(8).bits.pc)
  dt_ic1.io.instr    := RegNext(pipeOut(8).bits.instr)
  dt_ic1.io.special  := 0.U
  dt_ic1.io.skip     := false.B
  dt_ic1.io.isRVC    := false.B
  dt_ic1.io.scFailed := false.B
  dt_ic1.io.wen      := RegNext(regfile.io.wen(1))
  dt_ic1.io.wpdest   := RegNext(Cat(0.U(3.W),regfile.io.waddr(1)))
  dt_ic1.io.wdest    := RegNext(Cat(0.U(3.W),regfile.io.waddr(1)))

  val dt_iw0 = Module(new DifftestIntWriteback)
  dt_iw0.io.clock    := clock
  dt_iw0.io.coreid   := 0.U
  dt_iw0.io.valid    := RegNext(regfile.io.wen(0))
  dt_iw0.io.dest     := RegNext(Cat(0.U(3.W),regfile.io.waddr(0)))
  dt_iw0.io.data     := RegNext(Cat(0.U(3.W),regfile.io.wdata(0)))

  val dt_iw1 = Module(new DifftestIntWriteback)
  dt_iw1.io.clock    := clock
  dt_iw1.io.coreid   := 0.U
  dt_iw1.io.valid    := RegNext(regfile.io.wen(0))
  dt_iw1.io.dest     := RegNext(Cat(0.U(3.W),regfile.io.waddr(0)))
  dt_iw1.io.data     := RegNext(Cat(0.U(3.W),regfile.io.wdata(0)))




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


  val dt_te = Module(new DifftestTrapEvent)
  dt_te.io.clock    := clock
  dt_te.io.coreid   := 0.U
  dt_te.io.valid    := false.B//(fetch.io.inst === "h0000006b".U)
  dt_te.io.code     := 0.U//rf_a0(2, 0)
  dt_te.io.pc       := 0.U//fetch.io.pc
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
  dt_irs.io.gpr    := VecInit((0 to 31).map(i => regfile.read(i.U)))


}
