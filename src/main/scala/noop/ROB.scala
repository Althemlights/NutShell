package noop

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._

trait HasROBConst{
  // val multiIssue = true
  val robSize = 16
  val robWidth = 2
  val rmqSize = 4 // register map queue size
  val prfAddrWidth = log2Up(robSize) + log2Up(robWidth) // physical rf addr width
}

object physicalRFTools{
  def getPRFAddr(robIndex: UInt, bank: UInt): UInt = {
    Cat(robIndex, bank(0))
  }
}

class ROB(implicit val p: NOOPConfig) extends NOOPModule with HasInstrType with HasROBConst with HasRegFileParameter{
  val io = IO(new Bundle {
    val in = Vec(robWidth, Flipped(Decoupled(new DecodeIO)))
    // val out = Vec(robWidth, Decoupled(new DecodeIO))
    val commit = Vec(robWidth, Flipped(Decoupled(new OOCommitIO)))
    val wb = Vec(robWidth, new WriteBackIO)
    val redirect = new RedirectIO
    val flush = Input(Bool())
    val empty = Output(Bool())
    val index = Output(UInt(log2Up(robSize).W))

    // to CSR
    // val lsexc = Decoupled(new DecodeIO) // for l/s exception

    // to LSU
    val scommit = Output(Bool())

    // PRF
    // val arprf = Input(Vec(robWidth, UInt(prfAddrWidth.W))) // read from decode
    val rprf = Output(Vec(robWidth * 2, UInt(XLEN.W)))
    val rvalid = Output(Vec(robWidth * 2, Bool()))
    val rcommited = Output(Vec(robWidth * 2, Bool()))

  })

  val decode = Reg(Vec(robSize, Vec(robWidth, new DecodeIO)))
  // val valid = RegInit(VecInit(List.fill(robSize)(VecInit(List.fill(robWidth)(false.B)))))
  val valid = List.fill(robWidth)(Mem(robSize, Bool()))
  // val validReg = RegInit(0.U.asTypeOf(Vec(robSize * robWidth, Bool())))
  val commited = Reg(Vec(robSize, Vec(robWidth, Bool())))
  val redirect = Reg(Vec(robSize, Vec(robWidth, new RedirectIO)))
  val isMMIO = Reg(Vec(robSize, Vec(robWidth, Bool())))
  val intrNO = Reg(Vec(robSize, Vec(robWidth, UInt(XLEN.W))))
  val prf = Mem(robSize * robWidth, UInt(XLEN.W))

  // def valid(i: UInt, j: UInt){
  //   validReg(Cat(i.UInt(log2Up(robSize)), j.UInt(log2Up(robWidth))))
  // }

  // Almost all int/exceptions can be detected at decode stage, excepting for load/store related exception.
  // In NOOP-Argo's backend, non-l/s int/exc will be sent dircetly to CSR when dispatch,
  // while l/s exc will be sent to CSR by LSU.

  val ringBufferHead = RegInit(0.U(log2Up(robSize).W))
  val ringBufferTail = RegInit(0.U(log2Up(robSize).W))
  val ringBufferEmpty = ringBufferHead === ringBufferTail && !valid(0)(ringBufferHead) && !valid(1)(ringBufferHead)
  val ringBufferFull = ringBufferTail === ringBufferHead && (valid(0)(ringBufferHead) || valid(1)(ringBufferHead))
  val ringBufferAllowin = !ringBufferFull 

  def forAllROBBanks(func: Int => _) = {
    List.tabulate(robWidth)(func)
  }

  io.index := ringBufferHead
  io.empty := ringBufferEmpty

  // Register Map  
  val rmtMap = Mem(NRReg, UInt(prfAddrWidth.W))
  val rmtValid = RegInit(VecInit(Seq.fill(NRReg)(false.B)))
  val rmtCommited = RegInit(VecInit(Seq.fill(NRReg)(false.B)))

  forAllROBBanks((i: Int) => {
    io.rprf(2*i)        := rmtMap(io.in(i).bits.ctrl.rfSrc1)
    io.rprf(2*i+1)      := rmtMap(io.in(i).bits.ctrl.rfSrc2)
    io.rvalid(2*i)      := rmtValid(io.in(i).bits.ctrl.rfSrc1)
    io.rvalid(2*i+1)    := rmtValid(io.in(i).bits.ctrl.rfSrc2)
    io.rcommited(2*i)   := rmtCommited(io.in(i).bits.ctrl.rfSrc1)
    io.rcommited(2*i+1) := rmtCommited(io.in(i).bits.ctrl.rfSrc2)
    io.commit(i).ready  := true.B //!ringBufferEmpty
  })

  // Dispatch logic
  // ROB enqueue
  val validEnqueueRequest = List.tabulate(robWidth)(i => io.in(i).valid).foldRight(false.B)((sum,i)=>sum|i) //io.in(0).valid || io.in(1).valid
  when(validEnqueueRequest && ringBufferAllowin){
    ringBufferHead := ringBufferHead + 1.U
    forAllROBBanks((i: Int) => decode(ringBufferHead)(i) := io.in(i).bits)
    forAllROBBanks((i: Int) => valid(i)(ringBufferHead) := io.in(i).valid)
    forAllROBBanks((i: Int) => commited(ringBufferHead)(i) := false.B)
    forAllROBBanks((i: Int) => redirect(ringBufferHead)(i).valid := false.B)
    forAllROBBanks((i: Int) => 
      when(io.in(i).valid && io.in(i).bits.ctrl.rfWen){
        rmtMap(io.in(i).bits.ctrl.rfDest) := Cat(ringBufferHead, i.U)
        rmtValid(io.in(i).bits.ctrl.rfDest) := true.B
        rmtCommited(io.in(i).bits.ctrl.rfDest) := false.B
      }
    )
  }
  forAllROBBanks((i: Int) => io.in(i).ready := ringBufferAllowin)

  // Commit logic
  // commit to ROB
  // ROB listens to CDB (common data bus), i.e. CommitIO
  // An ROB term will be marked as commited after that inst was commited to CDB
  // This will always success

  // if ROB index == commit index && bank index == commit bank index
  for(i <- (0 to robSize - 1)){
    for(j <- (0 to robWidth - 1)){
      val robIdx = Cat(i.asUInt(log2Up(robSize).W), j.asUInt(log2Up(robWidth).W))
      for(k <- (0 to robWidth - 1)){
        when(valid(j)(i) && io.commit(k).bits.prfidx === robIdx && io.commit(k).valid){
        // when(true.B){
          assert(!commited(i)(j))
          // Mark an ROB term as commited
          commited(i)(j) := true.B
          // Write result to ROB-PRF
          prf(robIdx) := io.commit(k).bits.commits
          // Write other info which could be generated by function units
          isMMIO(i)(j) := io.commit(k).bits.isMMIO
          intrNO(i)(j) := io.commit(k).bits.intrNO
          // Write redirect info
          redirect(i)(j) := io.commit(k).bits.decode.cf.redirect
          // Update rmt
          // TODO: merge RMT with ROB
          when(io.commit(k).bits.decode.ctrl.rfWen){
            rmtCommited(Cat(i.U, j.U)) := true.B
          }
        }
      }
    }
  }

  // Retire logic

  // ROB Retire
  // We write back at most #bank reg results back to arch-rf.
  // Then we mark those ROB terms as finished, i.e. `!valid`
  // No more than robWidth insts can retire from ROB in a single cycle.
  val tailBankNotUsed = List.tabulate(robWidth)(i => !valid(i)(ringBufferTail) || valid(i)(ringBufferTail) && commited(ringBufferTail)(i))
  val tailTermEmpty = List.tabulate(robWidth)(i => !valid(i)(ringBufferTail)).foldRight(true.B)((sum, i) => sum & i)
  // val tailTermEmpty = !valid(ringBufferTail).asUInt.orR
  val retireATerm = tailBankNotUsed.foldRight(true.B)((sum, i) => sum & i) && !tailTermEmpty
  when(retireATerm){
    forAllROBBanks((i: Int) =>{
      valid(i)(ringBufferTail) := false.B
      // free prf (update RMT)
      when(decode(ringBufferTail)(i).ctrl.rfWen){
        rmtValid(Cat(ringBufferTail, i.U)) := false.B
      }
    })
  }

  // speculative execution

  // register map queue
  val rmq = Module(new Queue(UInt(VAddrBits.W), rmqSize)) //TODO
  
  // when speculated branch enters ROB, add current reg map into rmq
  // when a speculated branch inst retires, delete it from rmq
  // TODO
  rmq.io.enq.valid := List.tabulate(robWidth)(i => io.in(i).valid && io.in(i).bits.ctrl.isSpecExec).foldRight(false.B)((sum, i) => sum | i) // when(io.in(0).ctrl.isSpecExec)
  rmq.io.enq.bits := DontCare //TODO
  rmq.io.deq.ready := List.tabulate(robWidth)(i => 
    valid(i)(ringBufferTail) && decode(ringBufferTail)(i).ctrl.isSpecExec
  ).foldRight(false.B)((sum, i) => sum || i)

  // rmap recover := rmq.io.deq.bits

  // retire: trigger redirect
  // exception/interrupt/branch mispredict redirect is raised by ROB
  val redirectBank = Mux(redirect(ringBufferTail)(0).valid, 0.U, 1.U) // TODO: Fix it for robWidth > 2
  io.redirect := redirect(ringBufferTail)(redirectBank)
  io.redirect.valid := List.tabulate(robWidth)(i => 
    redirect(ringBufferTail)(i).valid && valid(i)(ringBufferTail)
  ).foldRight(false.B)((sum, i) => sum || i)

  // retire: trigger exception
  // Only l/s exception will trigger ROB flush
  // Other exception/interrupt will be stalled by Dispatch Unit until ROB is empty

  // Currently, agu(lsu) will send exception directly to CSR,
  // After sending exception signal, LSU will not commit this inst.
  // The inst which caused exception will be commited by CSR.

  // Raise decoupled store request
  // If l/s are decoupled, store request is sent to store buffer here.
  // Note: only # of safe store ops is sent to LSU
  io.scommit := List.tabulate(robWidth)(i => 
    valid(i)(ringBufferTail) && 
    decode(ringBufferTail)(i).ctrl.fuType === FuType.lsu && 
    LSUOpType.isStore(decode(ringBufferTail)(i).ctrl.fuOpType)
  ).foldRight(false.B)((sum, i) => sum || i)
  // In current version, only one l/s inst can be sent to agu in a cycle
  // therefore, in all banks, there is no more than 1 store insts

  // Arch-RF write back
  for(i <- (0 to robWidth - 1)){
    val haveRedirect = List.tabulate(i + 1)(j => redirect(ringBufferTail)(j).valid && valid(j)(ringBufferTail)).foldRight(false.B)((sum, i) => sum|i)
    io.wb(i).rfWen := retireATerm && decode(ringBufferTail)(i).ctrl.rfWen && valid(i)(ringBufferTail) && !haveRedirect
    io.wb(i).rfDest := decode(ringBufferTail)(i).ctrl.rfDest
    io.wb(i).rfData := prf(Cat(ringBufferTail, i.U))
  }

  // Generate Debug Trace
  Debug(){
    when (retireATerm && valid(0)(ringBufferTail)) { printf("[COMMIT1] TIMER: %d WBU: pc = 0x%x inst %x wen %x wdst %x wdata %x mmio %x intrNO %x\n", GTimer(), decode(ringBufferTail)(0).cf.pc, decode(ringBufferTail)(0).cf.instr, io.wb(0).rfWen, io.wb(0).rfDest, io.wb(0).rfData, isMMIO(ringBufferTail)(0), intrNO(ringBufferTail)(0)) }
    when (retireATerm && valid(1)(ringBufferTail)) { printf("[COMMIT2] TIMER: %d WBU: pc = 0x%x inst %x wen %x wdst %x wdata %x mmio %x intrNO %x\n", GTimer(), decode(ringBufferTail)(1).cf.pc, decode(ringBufferTail)(1).cf.instr, io.wb(1).rfWen, io.wb(1).rfDest, io.wb(1).rfData, isMMIO(ringBufferTail)(1), intrNO(ringBufferTail)(1)) }
  }

  val retireMultiTerms = retireATerm && valid(0)(ringBufferTail) && valid(1)(ringBufferTail)
  val firstValidInst = Mux(valid(0)(ringBufferTail), 0.U, 1.U)
  BoringUtils.addSource(retireATerm, "perfCntCondMinstret")
  BoringUtils.addSource(retireMultiTerms, "perfCntCondMultiCommit")
  
  if (!p.FPGAPlatform) {
    // TODO: fix debug
    BoringUtils.addSource(RegNext(retireATerm), "difftestCommit")
    BoringUtils.addSource(RegNext(retireMultiTerms), "difftestMultiCommit")
    BoringUtils.addSource(RegNext(SignExt(decode(ringBufferTail)(firstValidInst).cf.pc, AddrBits)), "difftestThisPC")
    // BoringUtils.addSource(RegNext(SignExt(decode(ringBufferTail)(1).cf.pc, AddrBits)), "difftestThisPC2")
    BoringUtils.addSource(RegNext(decode(ringBufferTail)(firstValidInst).cf.instr), "difftestThisINST")
    // BoringUtils.addSource(RegNext(decode(ringBufferTail)(1).cf.instr), "difftestThisINST2")
    BoringUtils.addSource(RegNext(isMMIO(ringBufferTail)(0)), "difftestIsMMIO") //TODO
    // BoringUtils.addSource(RegNext(isMMIO(ringBufferTail)(1)), "difftestIsMMIO2")
    BoringUtils.addSource(RegNext(decode(ringBufferTail)(firstValidInst).cf.instr(1,0)=/="b11".U), "difftestIsRVC")
    BoringUtils.addSource(RegNext(decode(ringBufferTail)(1).cf.instr(1,0)=/="b11".U), "difftestIsRVC2")
    BoringUtils.addSource(RegNext(intrNO(ringBufferTail)(firstValidInst)), "difftestIntrNO")
    // BoringUtils.addSource(RegNext(intrNO(ringBufferTail)(1)), "difftestIntrNO2")
  } else {
    BoringUtils.addSource(DontCare, "ilaWBUvalid")
    BoringUtils.addSource(DontCare, "ilaWBUpc")
    BoringUtils.addSource(DontCare, "ilaWBUrfWen")
    BoringUtils.addSource(DontCare, "ilaWBUrfDest")
    BoringUtils.addSource(DontCare, "ilaWBUrfData")
  }

  // flush control
  when(io.flush){
    ringBufferHead := 0.U
    ringBufferTail := 0.U
    List.tabulate(robWidth)(i => valid(i)(0) := 0.U) // set valid to 0
    List.tabulate(NRReg)(i => rmtValid(i) := false.B) // flush rmt
  }

}
