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

package XiaoHe.SSDbackend.fu

import chisel3.{UInt, _}
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import top.Settings
import utils._
import utils.MaskedRegMap.WritableMask
//import difftest._
import XiaoHe.SSDbackend._
import XiaoHe._

object SSDCSROpType {
  def jmp  = "b000".U
  def wrt  = "b001".U
  def set  = "b010".U
  def clr  = "b011".U
  def wrti = "b101".U
  def seti = "b110".U
  def clri = "b111".U
}

trait SSDHasCSRConst {
  // User Trap Setup
  val Ustatus       = 0x000
  val Uie           = 0x004
  val Utvec         = 0x005

  // User Trap Handling
  val Uscratch      = 0x040
  val Uepc          = 0x041
  val Ucause        = 0x042
  val Utval         = 0x043
  val Uip           = 0x044

  // User Floating-Point CSRs (not implemented)
  val Fflags        = 0x001
  val Frm           = 0x002
  val Fcsr          = 0x003

  // User Counter/Timers
  val Cycle         = 0xC00
  val Time          = 0xC01
  val Instret       = 0xC02

  // Supervisor Trap Setup
  val Sstatus       = 0x100
  val Sedeleg       = 0x102
  val Sideleg       = 0x103
  val Sie           = 0x104
  val Stvec         = 0x105
  val Scounteren    = 0x106

  // Supervisor Trap Handling
  val Sscratch      = 0x140
  val Sepc          = 0x141
  val Scause        = 0x142
  val Stval         = 0x143
  val Sip           = 0x144

  // Supervisor Protection and Translation
  val Satp          = 0x180

  // Machine Information Registers
  val Mvendorid     = 0xF11
  val Marchid       = 0xF12
  val Mimpid        = 0xF13
  val Mhartid       = 0xF14

  // Machine Trap Setup
  val Mstatus       = 0x300
  val Misa          = 0x301
  val Medeleg       = 0x302
  val Mideleg       = 0x303
  val Mie           = 0x304
  val Mtvec         = 0x305
  val Mcounteren    = 0x306

  // Machine Trap Handling
  val Mscratch      = 0x340
  val Mepc          = 0x341
  val Mcause        = 0x342
  val Mtval         = 0x343
  val Mip           = 0x344

  // Machine Memory Protection
  // TBD
  val Pmpcfg0       = 0x3A0
  val Pmpcfg1       = 0x3A1
  val Pmpcfg2       = 0x3A2
  val Pmpcfg3       = 0x3A3
  val PmpaddrBase   = 0x3B0

  // Trigger Registers
  val Tselect = 0x7A0
  val Tdata1 = 0x7A1
  val Tdata2 = 0x7A2
  val Tinfo = 0x7A4
  val Tcontrol = 0x7A5

  // Debug Mode Registers
  val Dcsr          = 0x7B0
  val Dpc           = 0x7B1
  val Dscratch0     = 0x7B2
  val Dscratch1     = 0x7B3

  // Machine Counter/Timers
  // Currently, NutCore uses perfcnt csr set instead of standard Machine Counter/Timers
  // 0xB80 - 0x89F are also used as perfcnt csr

  // Machine Counter Setup (not implemented)
  // Debug/Trace Registers (shared with Debug Mode) (not implemented)
  // Debug Mode Registers (not implemented)

  def privEcall  = 0x000.U
  def privEbreak = 0x001.U
  def privMret   = 0x302.U
  def privSret   = 0x102.U
  def privUret   = 0x002.U
  def privDret   = 0x7b2.U

  def ModeM     = 0x3.U
  def ModeH     = 0x2.U
  def ModeS     = 0x1.U
  def ModeU     = 0x0.U

  def IRQ_UEIP  = 0
  def IRQ_SEIP  = 1
  def IRQ_MEIP  = 3

  def IRQ_UTIP  = 4
  def IRQ_STIP  = 5
  def IRQ_MTIP  = 7

  def IRQ_USIP  = 8
  def IRQ_SSIP  = 9
  def IRQ_MSIP  = 11

  def IRQ_DEBUG = 12

  val IntPriority = Seq(
    IRQ_DEBUG,
    IRQ_MEIP, IRQ_MSIP, IRQ_MTIP,
    IRQ_SEIP, IRQ_SSIP, IRQ_STIP,
    IRQ_UEIP, IRQ_USIP, IRQ_UTIP
  )
}

trait SSDHasExceptionNO {
  def instrAddrMisaligned = 0
  def instrAccessFault    = 1
  def illegalInstr        = 2
  def breakPoint          = 3
  def loadAddrMisaligned  = 4
  def loadAccessFault     = 5
  def storeAddrMisaligned = 6
  def storeAccessFault    = 7
  def ecallU              = 8
  def ecallS              = 9
  def ecallM              = 11
  def instrPageFault      = 12
  def loadPageFault       = 13
  def storePageFault      = 15

  val ExcPriority = Seq(
    breakPoint, // TODO: different BP has different priority
    instrPageFault,
    instrAccessFault,
    illegalInstr,
    instrAddrMisaligned,
    ecallM, ecallS, ecallU,
    storeAddrMisaligned,
    loadAddrMisaligned,
    storePageFault,
    loadPageFault,
    storeAccessFault,
    loadAccessFault
  )
}

trait DebugCSR {
  
  this: SSDHasCSRConst =>
  
  class DcsrStruct extends Bundle {
    val debugver  = Output(UInt(4.W)) // [28:31]
    val pad1      = Output(UInt(10.W))// [27:18]
    val ebreakvs  = Output(Bool())    // [17] reserved for Hypervisor debug
    val ebreakvu  = Output(Bool())    // [16] reserved for Hypervisor debug
    val ebreakm   = Output(Bool())    // [15]
    val pad0      = Output(Bool())    // [14] ebreakh has been removed
    val ebreaks   = Output(Bool())    // [13]
    val ebreaku   = Output(Bool())    // [12]
    val stepie    = Output(Bool())    // [11]
    val stopcount = Output(Bool())    // [10]
    val stoptime  = Output(Bool())    // [9]
    val cause     = Output(UInt(3.W)) // [8:6]
    val v         = Output(Bool())    // [5]
    val mprven    = Output(Bool())    // [4]
    val nmip      = Output(Bool())    // [3]
    val step      = Output(Bool())    // [2]
    val prv       = Output(UInt(2.W)) // [1:0]
    require(this.getWidth == 32)
  }

  object DcsrStruct extends DcsrStruct {
    def DEBUGVER_NONE   = 0.U
    def DEBUGVER_SPEC   = 4.U
    def DEBUGVER_CUSTOM = 15.U
    def CAUSE_EBREAK        = 1.U
    def CAUSE_TRIGGER       = 2.U
    def CAUSE_HALTREQ       = 3.U
    def CAUSE_STEP          = 4.U
    def CAUSE_RESETHALTREQ  = 5.U
    private def debugver_offset   = 28
    private def stopcount_offset  = 10
    private def stoptime_offset   = 9
    private def mprven_offset     = 5
    private def prv_offset        = 0
    def init: UInt = (
      (DEBUGVER_SPEC.litValue << debugver_offset) | /* Debug implementation as it described in 0.13 draft */
      (0L << stopcount_offset) |                    /* Stop count updating has not been supported */
      (0L << stoptime_offset) |                     /* Stop time updating has not been supported */
      (0L << mprven_offset) |                       /* Whether use mstatus.perven as mprven */
      (ModeM.litValue << prv_offset)
    ).U
  }
}

class SSDCSRIO extends FunctionUnitIO {
  val cfIn = Flipped(new CtrlFlowIO)
  val redirect = new RedirectIO
  // for exception check
  val instrValid = Input(Bool())      // same with io.in.valid
  val isBackendException = Input(Bool())
  // for differential testing
  val intrNO = Output(UInt(XLEN.W))
//  val imemMMU = Flipped(new MMUIO)
//  val dmemMMU = Flipped(new MMUIO)
  val wenFix = Output(Bool())
  val CSRregfile = new CSRregfile
  val ArchEvent = new ArchEvent
  val hartid = Input(UInt(XLEN.W))
  val debugInt = Input(Bool())      // debug Interrupt
  val hasI01Valid = Input(Bool())
  val customCtrl = Output(new CustomCSRCtrlIO)
}

class SSDCSR extends NutCoreModule with SSDHasCSRConst with SSDHasExceptionNO with DebugCSR with SdtrigExt{
  val io = IO(new SSDCSRIO)

  val (valid, src1, src2, func) = (io.in.valid, io.in.bits.src1, io.in.bits.src2, io.in.bits.func)
  // indicates pipe0 or pipe1 has valid instructions, so the exception/intrruption can be attached to this instruction
  val hasValidInst = io.hasI01Valid

  def access(valid: Bool, src1: UInt, src2: UInt, func: UInt): UInt = {
    this.valid := valid
    this.src1 := src1
    this.src2 := src2
    this.func := func
    io.out.bits
  }

  // CSR define

  class Priv extends Bundle {
    val m = Output(Bool())
    val h = Output(Bool())
    val s = Output(Bool())
    val u = Output(Bool())
  }

  val csrNotImplemented = RegInit(UInt(XLEN.W), 0.U)

  class MstatusStruct extends Bundle {
    val sd = Output(UInt(1.W))

    val pad1 = if (XLEN == 64) Output(UInt(27.W)) else null
    val sxl = if (XLEN == 64) Output(UInt(2.W)) else null
    val uxl = if (XLEN == 64) Output(UInt(2.W)) else null
    val pad0 = if (XLEN == 64) Output(UInt(9.W)) else Output(UInt(8.W))

    val tsr = Output(UInt(1.W))
    val tw = Output(UInt(1.W))
    val tvm = Output(UInt(1.W))
    val mxr = Output(UInt(1.W))
    val sum = Output(UInt(1.W))
    val mprv = Output(UInt(1.W))
    val xs = Output(UInt(2.W))
    val fs = Output(UInt(2.W))
    val mpp = Output(UInt(2.W))
    val hpp = Output(UInt(2.W))
    val spp = Output(UInt(1.W))
    val pie = new Priv
    val ie = new Priv
  }

  class SatpStruct extends Bundle {
    val mode = UInt(4.W)
    val asid = UInt(16.W)
    val ppn = UInt(44.W)
  }

  class Interrupt extends Bundle {
    val e = new Priv
    val t = new Priv
    val s = new Priv
  }

  // Debug CSRs
  val dcsr = RegInit(UInt(32.W), DcsrStruct.init)
  val dpc = Reg(UInt(64.W))
  val dscratch0 = Reg(UInt(64.W))
  val dscratch1 = Reg(UInt(64.W))
  val debugMode = RegInit(false.B)
  val debugIntrEnable = RegInit(true.B) // debug interrupt will be handle only when debugIntrEnable

  val dpcPrev = RegNext(dpc)
  Debug(dpcPrev =/= dpc, "Debug Mode: dpc is altered! Current is %x, previous is %x\n", dpc, dpcPrev)

  val dcsrData = dcsr.asTypeOf(new DcsrStruct)
  val dcsrMask = ZeroExt(GenMask(15) | GenMask(13, 11) | GenMask(4) | GenMask(2, 0), XLEN)// Dcsr write mask
  def dcsrUpdateSideEffect(dcsr: UInt): UInt = {
    val dcsrOld = WireInit(dcsr.asTypeOf(new DcsrStruct))
    val dcsrNew = dcsr | (dcsrOld.prv(0) | dcsrOld.prv(1)).asUInt // turn 10 priv into 11
    dcsrNew
  }

  // Trigger CSRs
  private val tselectPhy = RegInit(0.U(4.W))
  private val tdata1RegVec = RegInit(VecInit(Seq.fill(TriggerNum)(Tdata1Bundle.default)))
  private val tdata2RegVec = RegInit(VecInit(Seq.fill(TriggerNum)(0.U(64.W))))
  private val tdata1WireVec = tdata1RegVec.map(_.asTypeOf(new Tdata1Bundle))
  private val tdata2WireVec = tdata2RegVec
  private val tdata1Selected = tdata1RegVec(tselectPhy).asTypeOf(new Tdata1Bundle)
  private val tdata2Selected = tdata2RegVec(tselectPhy)
  private val newTriggerChainVec = UIntToOH(tselectPhy, TriggerNum).asBools.zip(tdata1WireVec.map(_.data.asTypeOf(new MControlData).chain)).map { case (bool1, bool2) => bool1 || bool2 }
  private val newTriggerChainIsLegal = TriggerCheckChainLegal(newTriggerChainVec, TriggerChainMaxLength)
  val tinfo = RegInit((BigInt(1) << TrigTypeEnum.MCONTROL.litValue.toInt).U(XLEN.W)) // This value should be 4.U

  val tdata1Prev = RegNext(tdata1Selected.asUInt)
  Debug(tdata1Prev =/= tdata1Selected.asUInt, "Debug Mode: tdata1 is altered! Current is %x, previous is %x\n", tdata1Selected.asUInt, tdata1Prev)
  val tdata2Prev = RegNext(tdata2Selected.asUInt)
  Debug(tdata2Prev =/= tdata2Selected.asUInt, "Debug Mode: tdata2 is altered! Current is %x, previous is %x\n", tdata2Selected.asUInt, tdata2Prev)

  def WriteTselect(wdata: UInt) = {
    Mux(wdata < TriggerNum.U, wdata(3, 0), tselectPhy)
  }

  def GenTdataDistribute(tdata1: Tdata1Bundle, tdata2: UInt): MatchTriggerIO = {
    val res = Wire(new MatchTriggerIO)
    val mcontrol: MControlData = WireInit(tdata1.data.asTypeOf(new MControlData))
    res.matchType := mcontrol.match_.asUInt
    res.select    := mcontrol.select
    res.timing    := mcontrol.timing
    res.action    := mcontrol.action.asUInt
    res.chain     := mcontrol.chain
    res.execute   := mcontrol.execute
    res.load      := mcontrol.load
    res.store     := mcontrol.store
    res.tdata2    := tdata2
    res
  }

  io.customCtrl.frontend_trigger.tUpdate.bits.addr := tselectPhy
  io.customCtrl.mem_trigger.tUpdate.bits.addr := tselectPhy
  io.customCtrl.frontend_trigger.tUpdate.bits.tdata := GenTdataDistribute(tdata1Selected, tdata2Selected)
  io.customCtrl.mem_trigger.tUpdate.bits.tdata := GenTdataDistribute(tdata1Selected, tdata2Selected)

  // Machine-Level CSRs

  val mtvec = RegInit(UInt(XLEN.W), 0.U)
  val mcounteren = RegInit(UInt(XLEN.W), 0.U)
  val mcause = RegInit(UInt(XLEN.W), 0.U)
  val mtval = RegInit(UInt(XLEN.W), 0.U)
  val mepc = RegInit(UInt(XLEN.W), 0.U)

  val mie = RegInit(0.U(XLEN.W))
  val mipWire = WireInit(0.U.asTypeOf(new Interrupt))
  //val mipReg  = RegInit(0.U.asTypeOf(new Interrupt).asUInt)
  val mipReg = RegInit(0.U(XLEN.W))
  val mipFixMask = "h77f".U
  val mip = (mipWire.asUInt | mipReg).asTypeOf(new Interrupt)

  /*def getMisaMxl(mxl: Int): UInt = {mxl.U << (XLEN-2)}
  def getMisaExt(ext: Char): UInt = {1.U << (ext.toInt - 'a'.toInt)}
  var extList = List('a', 's', 'i', 'u')
  if(HasMExtension){ extList = extList :+ 'm'}
  if(HasCExtension){ extList = extList :+ 'c'}
  val misaInitVal = getMisaMxl(2) | extList.foldLeft(0.U)((sum, i) => sum | getMisaExt(i)) //"h8000000000141105".U
  val misa = RegInit(UInt(XLEN.W), misaInitVal.asUInt)*/
  def getMisaMxl(mxl: BigInt): BigInt = mxl << (XLEN - 2)

  def getMisaExt(ext: Char): Long = 1 << (ext.toInt - 'a'.toInt)

  var extList = List('a', 's', 'i', 'u')
  if (HasMExtension) {
    extList = extList :+ 'm'
  }
  if (HasCExtension) {
    extList = extList :+ 'c'
  }
  if (HasFPU) { 
    extList = extList ++ List('f', 'd') 
  }
  val misaInitVal = getMisaMxl(2) | extList.foldLeft(0L)((sum, i) => sum | getMisaExt(i)) //"h8000000000141105".U
  val misa = RegInit(UInt(XLEN.W), misaInitVal.U)
  // MXL = 2          | 0 | EXT = b 00 0000 0100 0001 0001 0000 0101
  // (XLEN-1, XLEN-2) |   |(25, 0)  ZY XWVU TSRQ PONM LKJI HGFE DCBA

  val mvendorid = RegInit(UInt(XLEN.W), 0.U) // this is a non-commercial implementation
  val marchid = RegInit(UInt(XLEN.W), 0.U) // return 0 to indicate the field is not implemented
  val mimpid = RegInit(UInt(XLEN.W), 0.U) // provides a unique encoding of the version of the processor implementation
  val mhartid = Reg(UInt(XLEN.W)) // the hardware thread running the code
  when(RegNext(RegNext(reset.asBool) && !reset.asBool)) {
    mhartid := io.hartid
  }
  val mstatus = RegInit(UInt(XLEN.W), 0.U)
  // val mstatus = RegInit(UInt(XLEN.W), "h8000c0100".U)
  // mstatus Value Table
  // | sd   |
  // | pad1 |
  // | sxl  | hardlinked to 10, use 00 to pass xv6 test
  // | uxl  | hardlinked to 00
  // | pad0 |
  // | tsr  |
  // | tw   |
  // | tvm  |
  // | mxr  |
  // | sum  |
  // | mprv |
  // | xs   | 00 |
  // | fs   | 00 |
  // | mpp  | 00 |
  // | hpp  | 00 |
  // | spp  | 0 |
  // | pie  | 0000 |
  // | ie   | 0000 | uie hardlinked to 0, as N ext is not implemented
  val mstatusStruct = mstatus.asTypeOf(new MstatusStruct)

  def mstatusUpdateSideEffect(mstatus: UInt): UInt = {
    val mstatusOld = WireInit(mstatus.asTypeOf(new MstatusStruct))
    val mstatusNew = Cat(mstatusOld.fs === "b11".U, mstatus(XLEN - 2, 0))
    mstatusNew
  }

  val medeleg = RegInit(UInt(XLEN.W), 0.U)
  val mideleg = RegInit(UInt(XLEN.W), 0.U)
  val mscratch = RegInit(UInt(XLEN.W), 0.U)

  val pmpcfg0 = RegInit(UInt(XLEN.W), 0.U)
  val pmpcfg1 = RegInit(UInt(XLEN.W), 0.U)
  val pmpcfg2 = RegInit(UInt(XLEN.W), 0.U)
  val pmpcfg3 = RegInit(UInt(XLEN.W), 0.U)
  val pmpaddr0 = RegInit(UInt(XLEN.W), 0.U)
  val pmpaddr1 = RegInit(UInt(XLEN.W), 0.U)
  val pmpaddr2 = RegInit(UInt(XLEN.W), 0.U)
  val pmpaddr3 = RegInit(UInt(XLEN.W), 0.U)

  // Superviser-Level CSRs

  // val sstatus = RegInit(UInt(XLEN.W), "h00000000".U)
  val sstatusWmask = "hc6122".U
  // Sstatus Write Mask
  // -------------------------------------------------------
  //    19           9   5     2
  // 0  1100 0000 0001 0010 0010
  // 0  c    0    1    2    2
  // -------------------------------------------------------
  val sstatusRmask = sstatusWmask | "h8000000300018000".U
  // Sstatus Read Mask = (SSTATUS_WMASK | (0xf << 13) | (1ull << 63) | (3ull << 32))
  val stvec = RegInit(UInt(XLEN.W), 0.U)
  // val sie = RegInit(0.U(XLEN.W))
  val sieMask = "h222".U & mideleg
  val sipMask = "h222".U & mideleg
  // val satp = RegInit(UInt(XLEN.W), "h8000000000087fbe".U)
  val satp = RegInit(UInt(XLEN.W), 0.U)
  val sepc = RegInit(UInt(XLEN.W), 0.U)
  val scause = RegInit(UInt(XLEN.W), 0.U)
  val stval = RegInit(UInt(XLEN.W), 0.U)
  val sscratch = RegInit(UInt(XLEN.W), 0.U)
  val scounteren = RegInit(UInt(XLEN.W), 0.U)

  if (Settings.get("HasDTLB")) {
    //BoringUtils.addSource(satp, "CSRSATP")
  }

  // User-Level CSRs
  val uepc = Reg(UInt(XLEN.W))

  // Atom LR/SC Control Bits
  val setLr = WireInit(Bool(), false.B)
  val setLrVal = WireInit(Bool(), false.B)
  val setLrAddr = WireInit(UInt(AddrBits.W), DontCare) //TODO : need check
  val lr = RegInit(Bool(), false.B)
  val lrAddr = RegInit(UInt(AddrBits.W), 0.U)
  BoringUtils.addSink(setLr, "set_lr")
  BoringUtils.addSink(setLrVal, "set_lr_val")
  BoringUtils.addSink(setLrAddr, "set_lr_addr")
  //BoringUtils.addSource(lr, "lr")
  //BoringUtils.addSource(lrAddr, "lr_addr")

  when(setLr) {
    lr := setLrVal
    lrAddr := setLrAddr
  }

  // Hart Priviledge Mode
  val priviledgeMode = RegInit(UInt(2.W), ModeM)

  // perfcnt
  val hasPerfCnt = EnablePerfCnt
  val nrPerfCnts = if (hasPerfCnt) 0x80 else 0x3
  val perfCnts = List.fill(nrPerfCnts)(RegInit(0.U(64.W)))
  val perfCntsLoMapping = (0 until nrPerfCnts).map { case i => MaskedRegMap(0xb00 + i, perfCnts(i)) }
  val perfCntsHiMapping = (0 until nrPerfCnts).map { case i => MaskedRegMap(0xb80 + i, perfCnts(i)(63, 32)) }


  //for difftest
  val mcause_wire = WireInit(UInt(XLEN.W), 0.U)
  val mepc_wire = WireInit(UInt(XLEN.W), 0.U)
  val mstatus_wire = WireInit(UInt(XLEN.W), 0.U)
  val mie_wire = WireInit(UInt(XLEN.W), 0.U)
  mcause_wire := mcause
  mepc_wire := mepc
  mstatus_wire := mstatus
  mie_wire := mie

  // val mstatusStruct_wire = mstatus_wire.asTypeOf(new MstatusStruct)


  // CSR reg map
  val mapping = Map(

    // User Trap Setup
    // MaskedRegMap(Ustatus, ustatus),
    // MaskedRegMap(Uie, uie, 0.U, MaskedRegMap.Unwritable),
    // MaskedRegMap(Utvec, utvec),

    // User Trap Handling
    // MaskedRegMap(Uscratch, uscratch),
    // MaskedRegMap(Uepc, uepc),
    // MaskedRegMap(Ucause, ucause),
    // MaskedRegMap(Utval, utval),
    // MaskedRegMap(Uip, uip),

    // User Floating-Point CSRs (not implemented)
    // MaskedRegMap(Fflags, fflags),
    // MaskedRegMap(Frm, frm),
    // MaskedRegMap(Fcsr, fcsr),

    // User Counter/Timers
    // MaskedRegMap(Cycle, cycle),
    // MaskedRegMap(Time, time),
    // MaskedRegMap(Instret, instret),

    // Supervisor Trap Setup
    MaskedRegMap(Sstatus, mstatus, sstatusWmask, mstatusUpdateSideEffect, sstatusRmask),

    // MaskedRegMap(Sedeleg, Sedeleg),
    // MaskedRegMap(Sideleg, Sideleg),
    MaskedRegMap(Sie, mie, sieMask, MaskedRegMap.NoSideEffect, sieMask),
    MaskedRegMap(Stvec, stvec),
    MaskedRegMap(Scounteren, scounteren),

    // Supervisor Trap Handling
    MaskedRegMap(Sscratch, sscratch),
    MaskedRegMap(Sepc, sepc),
    MaskedRegMap(Scause, scause),
    MaskedRegMap(Stval, stval),
    MaskedRegMap(Sip, mip.asUInt, sipMask, MaskedRegMap.Unwritable, sipMask),

    // Supervisor Protection and Translation
    MaskedRegMap(Satp, satp),

    // Machine Information Registers
    MaskedRegMap(Mvendorid, mvendorid, 0.U, MaskedRegMap.Unwritable),
    MaskedRegMap(Marchid, marchid, 0.U, MaskedRegMap.Unwritable),
    MaskedRegMap(Mimpid, mimpid, 0.U, MaskedRegMap.Unwritable),
    MaskedRegMap(Mhartid, mhartid, 0.U(XLEN.W), MaskedRegMap.Unwritable),

    // Machine Trap Setup
    // MaskedRegMap(Mstatus, mstatus, "hffffffffffffffee".U, (x=>{printf("mstatus write: %x time: %d\n", x, GTimer()); x})),
    MaskedRegMap(Mstatus, mstatus, "hffffffffffffffff".U, mstatusUpdateSideEffect),
    MaskedRegMap(Misa, misa, 0.U, MaskedRegMap.Unwritable), // now MXL, EXT is not changeable
    MaskedRegMap(Medeleg, medeleg, "hbbff".U),
    MaskedRegMap(Mideleg, mideleg, "h222".U),
    MaskedRegMap(Mie, mie),
    MaskedRegMap(Mtvec, mtvec),
    MaskedRegMap(Mcounteren, mcounteren),

    // Machine Trap Handling
    MaskedRegMap(Mscratch, mscratch),
    MaskedRegMap(Mepc, mepc),
    MaskedRegMap(Mcause, mcause),
    MaskedRegMap(Mtval, mtval),
    MaskedRegMap(Mip, mip.asUInt, 0.U, MaskedRegMap.Unwritable),

    // Machine Memory Protection
    MaskedRegMap(Pmpcfg0, pmpcfg0),
    MaskedRegMap(Pmpcfg1, pmpcfg1),
    MaskedRegMap(Pmpcfg2, pmpcfg2),
    MaskedRegMap(Pmpcfg3, pmpcfg3),
    MaskedRegMap(PmpaddrBase + 0, pmpaddr0),
    MaskedRegMap(PmpaddrBase + 1, pmpaddr1),
    MaskedRegMap(PmpaddrBase + 2, pmpaddr2),
    MaskedRegMap(PmpaddrBase + 3, pmpaddr3),

    //--- Trigger ---
    MaskedRegMap(Tselect, tselectPhy, WritableMask, WriteTselect),
    // Todo: support chain length = 2
    MaskedRegMap(Tdata1, tdata1RegVec(tselectPhy),
      WritableMask,
      x => Tdata1Bundle.Write(x, tdata1RegVec(tselectPhy), newTriggerChainIsLegal, debug_mode = debugMode),
      WritableMask,
      x => Tdata1Bundle.Read(x)),
    MaskedRegMap(Tdata2, tdata2RegVec(tselectPhy)),
    MaskedRegMap(Tinfo, tinfo, 0.U(XLEN.W), MaskedRegMap.Unwritable),

    //--- Debug Mode ---
    MaskedRegMap(Dcsr, dcsr, dcsrMask, dcsrUpdateSideEffect),
    MaskedRegMap(Dpc, dpc),
    MaskedRegMap(Dscratch0, dscratch0),
    MaskedRegMap(Dscratch1, dscratch1)

  ) ++ perfCntsLoMapping //++ (if (XLEN == 32) perfCntsHiMapping else Nil)

  val addr = src2(11, 0)
  val rdata = Wire(UInt(XLEN.W))
  val csri = ZeroExt(io.cfIn.instr(19, 15), XLEN) //unsigned imm for csri. [TODO]
  val wdata = LookupTree(func, List(
    SSDCSROpType.wrt -> src1,
    SSDCSROpType.set -> (rdata | src1),
    SSDCSROpType.clr -> (rdata & ~src1),
    SSDCSROpType.wrti -> csri, //TODO: csri --> src2
    SSDCSROpType.seti -> (rdata | csri),
    SSDCSROpType.clri -> (rdata & ~csri)
  ))

  // SATP wen check
  val satpLegalMode = (wdata.asTypeOf(new SatpStruct).mode === 0.U) || (wdata.asTypeOf(new SatpStruct).mode === 8.U)

  // General CSR wen check
  val wen = (valid && func =/= SSDCSROpType.jmp) && (addr =/= Satp.U || satpLegalMode) && !io.isBackendException
  val isIllegalMode = priviledgeMode < addr(9, 8)
  val justRead = (func === SSDCSROpType.set || func === SSDCSROpType.seti) && src1 === 0.U // csrrs and csrrsi are exceptions when their src1 is zero
  val isIllegalWrite = wen && (addr(11, 10) === "b11".U) && !justRead // Write a read-only CSR register
  val isIllegalAccess = isIllegalMode || isIllegalWrite

  MaskedRegMap.generate(mapping, addr, rdata, wen && !isIllegalAccess, wdata)
  val isIllegalAddr = MaskedRegMap.isIllegalAddr(mapping, addr)
  val resetSatp = addr === Satp.U && wen // write to satp will cause the pipeline be flushed
  val csrdata = WireInit(0.U(64.W))
  csrdata := Mux(io.in.valid,rdata,RegNext(csrdata))
  io.out.bits := csrdata

  // Fix Mip/Sip write
  val fixMapping = Map(
    MaskedRegMap(Mip, mipReg.asUInt, mipFixMask),
    MaskedRegMap(Sip, mipReg.asUInt, sipMask, MaskedRegMap.NoSideEffect, sipMask)
  )
  val rdataDummy = Wire(UInt(XLEN.W))
  MaskedRegMap.generate(fixMapping, addr, rdataDummy, wen && !isIllegalAccess, wdata)

  // Trigger Ctrl
  val triggerEnableVec = tdata1RegVec.map { tdata1 =>
    val mcontrolData = tdata1.asTypeOf(new Tdata1Bundle).data.asTypeOf(new MControlData)
    tdata1.asTypeOf(new Tdata1Bundle).type_.asUInt === TrigTypeEnum.MCONTROL && (
      mcontrolData.m && priviledgeMode === ModeM ||
        mcontrolData.s && priviledgeMode === ModeS ||
        mcontrolData.u && priviledgeMode === ModeU)
  }
  val fetchTriggerEnableVec = triggerEnableVec.zip(tdata1WireVec).map {
    case (tEnable, tdata1) => tEnable && tdata1.asTypeOf(new Tdata1Bundle).data.asTypeOf(new MControlData).isFetchTrigger
  }
  val memAccTriggerEnableVec = triggerEnableVec.zip(tdata1WireVec).map {
    case (tEnable, tdata1) => tEnable && tdata1.asTypeOf(new Tdata1Bundle).data.asTypeOf(new MControlData).isMemAccTrigger
  }
  io.customCtrl.frontend_trigger.tEnableVec := fetchTriggerEnableVec
  io.customCtrl.mem_trigger.tEnableVec := memAccTriggerEnableVec

  val tdata1Update = wen && (addr === Tdata1.U)
  val tdata2Update = wen && (addr === Tdata2.U)
  val triggerUpdate = wen && (addr === Tdata1.U || addr === Tdata2.U)
  val frontendTriggerUpdate =
    tdata1Update && wdata.asTypeOf(new Tdata1Bundle).type_.asUInt === TrigTypeEnum.MCONTROL &&
      wdata.asTypeOf(new Tdata1Bundle).data.asTypeOf(new MControlData).isFetchTrigger ||
      tdata1Selected.data.asTypeOf(new MControlData).isFetchTrigger && triggerUpdate
  val memTriggerUpdate =
    tdata1Update && wdata.asTypeOf(new Tdata1Bundle).type_.asUInt === TrigTypeEnum.MCONTROL &&
      wdata.asTypeOf(new Tdata1Bundle).data.asTypeOf(new MControlData).isMemAccTrigger ||
      tdata1Selected.data.asTypeOf(new MControlData).isMemAccTrigger && triggerUpdate

  io.customCtrl.frontend_trigger.tUpdate.valid := RegNext(frontendTriggerUpdate)
  io.customCtrl.mem_trigger.tUpdate.valid := RegNext(memTriggerUpdate)
  val triggerEnableUInt = Cat(triggerEnableVec.reverse)
  //Debug(triggerEnableUInt > 0.U, "Debug Mode: At least 1 trigger is enabled, trigger enable is %b\n", triggerEnableUInt)

  // CSR inst decode
  val ret = Wire(Bool())
  val isEbreak = addr === privEbreak && func === SSDCSROpType.jmp && !io.isBackendException
  val isEcall = addr === privEcall && func === SSDCSROpType.jmp && !io.isBackendException
  val isMret = addr === privMret && func === SSDCSROpType.jmp && !io.isBackendException
  val isSret = addr === privSret && func === SSDCSROpType.jmp && !io.isBackendException
  val isUret = addr === privUret && func === SSDCSROpType.jmp && !io.isBackendException
  val isDret = addr === privDret && func === SSDCSROpType.jmp && !io.isBackendException 

  //  Debug(wen, "csr write: pc %x addr %x rdata %x wdata %x func %x\n", io.cfIn.pc, addr, rdata, wdata, func)
  //  Debug(wen, "[MST] time %d pc %x mstatus %x mideleg %x medeleg %x mode %x\n", GTimer(), io.cfIn.pc, mstatus, mideleg , medeleg, priviledgeMode)
  // Exception and Intr

  // interrupts

  val mtip = WireInit(false.B)
  val meip = WireInit(false.B)
  val msip = WireInit(false.B)
  BoringUtils.addSink(mtip, "mtip")
  BoringUtils.addSink(meip, "meip")
  BoringUtils.addSink(msip, "msip")
  mipWire.t.m := mtip
  mipWire.e.m := meip
  mipWire.s.m := msip
  val pre_mtip = RegNext(mtip)
  Debug(!pre_mtip && mtip, "CSR: MTIP set up!\n")
  Debug(pre_mtip && !mtip, "CSR: MTIP set down!\n")

  // SEIP from PLIC is only used to raise interrupt,
  // but it is not stored in the CSR
  val seip = meip // FIXME: PLIC should generate SEIP different from MEIP
  val mipRaiseIntr = WireInit(mip)
  val mipRaiseIntr_wire = WireInit(mipWire)
  mipRaiseIntr.e.s := mip.e.s | seip


  val ideleg = (mideleg & mipRaiseIntr.asUInt)

  def priviledgedEnableDetect(x: Bool): Bool = Mux(x, ((priviledgeMode === ModeS) && mstatusStruct.ie.s) || (priviledgeMode < ModeS),
    ((priviledgeMode === ModeM) && mstatusStruct.ie.m) || (priviledgeMode < ModeM))

  // TODO: debug Interrupt
  val debugIntr = io.debugInt & debugIntrEnable     // has debug Interrupt and Enable Interrupt
  val preDebugIntr = RegNext(debugIntr)
  Debug(!preDebugIntr && debugIntr, "Debug Mode: debug interrupt is asserted and valid [start high]!\n")
  Debug(preDebugIntr && !debugIntr, "Debug Mode: debug interrupt is asserted and valid [start low]!\n")

  val intrVecEnable = Wire(Vec(12, Bool()))
  intrVecEnable.zip(ideleg.asBools).map { case (x, y) => x := priviledgedEnableDetect(y) }
  val intrVec = Cat(debugIntr && !debugMode, (mie_wire(11, 0) & mipRaiseIntr.asUInt & intrVecEnable.asUInt))

  // 能够感知写 mstatus 会导致 mie 拉高
  val write_mstatus_mie = addr === Mstatus.U && io.in.valid && mstatusUpdateSideEffect(wdata).asTypeOf(new MstatusStruct).ie.m

  // 本身就支持 enable interrupt 或者当拍对 mstatus 的写导致 mie 的拉高
  def priviledgedEnableDetect_wire(x: Bool, write: Bool): Bool = Mux(x, ((priviledgeMode === ModeS) && mstatus.asTypeOf(new MstatusStruct).ie.s) || (priviledgeMode < ModeS),
    ((priviledgeMode === ModeM) && mstatus.asTypeOf(new MstatusStruct).ie.m) || write || (priviledgeMode < ModeM))

  val intrVecEnable_wire = Wire(Vec(12, Bool()))
  intrVecEnable_wire.zip(ideleg.asBools).map { case (x, y) => x := priviledgedEnableDetect_wire(y, write_mstatus_mie) }
  val intrVec_wire = Cat(debugIntr && !debugMode, (mie_wire(11, 0) & mipRaiseIntr_wire.asUInt & intrVecEnable_wire.asUInt))
  // BoringUtils.addSource(intrVec, "intrVecIDU")
  // val intrNO = PriorityEncoder(intrVec)

  val intrNO = IntPriority.foldRight(0.U)((i: Int, sum: UInt) => Mux(intrVec(i), i.U, sum))
  val intrNO_wire = IntPriority.foldRight(0.U)((i: Int, sum: UInt) => Mux(intrVec_wire(i), i.U, sum))
  val raiseIntr = intrVec.asUInt.orR
  val raiseIntr_wire = intrVec_wire.asUInt.orR
  val hasDebugIntr = intrNO_wire === IRQ_DEBUG.U

  // dontTouch(mstatusStruct_wire)
  dontTouch(intrVecEnable_wire)
  dontTouch(intrVec_wire)
  dontTouch(intrNO_wire)
  dontTouch(mipRaiseIntr_wire)

  // In this situation, hart will enter debug mode instead of handling a breakpoint exception simply.
  // Ebreak block instructions backwards, so it's ok to not keep extra info to distinguish between breakpoint
  // exception and enter-debug-mode exception.
  val ebreakEnterDebugMode =
    (priviledgeMode === ModeM && dcsrData.ebreakm) ||
    (priviledgeMode === ModeS && dcsrData.ebreaks) ||
    (priviledgeMode === ModeU && dcsrData.ebreaku) 

  val raiseDebugException = !debugMode && isEbreak && ebreakEnterDebugMode
  
  // TODO: merge iduExceptionVec, csrExceptionVec as raiseExceptionVec
  val csrExceptionVec = Wire(Vec(16, Bool()))
  csrExceptionVec.map(_ := false.B)
  csrExceptionVec(breakPoint) := io.in.valid && isEbreak
  csrExceptionVec(ecallM) := priviledgeMode === ModeM && io.in.valid && isEcall
  csrExceptionVec(ecallS) := priviledgeMode === ModeS && io.in.valid && isEcall
  csrExceptionVec(ecallU) := priviledgeMode === ModeU && io.in.valid && isEcall
  csrExceptionVec(illegalInstr) := false.B //(isIllegalAddr || isIllegalAccess) && wen && !io.isBackendException // Trigger an illegal instr exception when unimplemented csr is being read/written or not having enough priviledge
  csrExceptionVec(loadPageFault) := false.B
  csrExceptionVec(storePageFault) := false.B
  val hasSingleStep = false.B               // TODO
  val hasTriggerFire = io.cfIn.triggeredFire.canFire
  val triggerFrontendHitVec = io.cfIn.triggeredFire.frontendHit
  val triggerMemHitVec      = io.cfIn.triggeredFire.backendHit
  val triggerHitVec = triggerFrontendHitVec.zip(triggerMemHitVec).map {
    case (frontendHit, memHit) => frontendHit || memHit
  }                              // Todo: update mcontrol.hit
  val triggerCanFireVec = io.cfIn.triggeredFire.frontendCanFire.zip(io.cfIn.triggeredFire.backendCanFire).map {
    case (frontendCanFire, backendCanFire) => frontendCanFire || backendCanFire
  }
  // More than one triggers can hit at the same time, but only fire one
  // We select the first hit trigger to fire
  val triggerFireOH = PriorityEncoderOH(triggerCanFireVec)
  val triggerFireAction = PriorityMux(triggerFireOH, tdata1WireVec.map(_.getTriggerAction)).asUInt

  Debug(hasSingleStep, "Debug Mode: single step exception\n")
  Debug(hasTriggerFire, p"Debug Mode: trigger fire, frontend hit vec ${Binary(io.cfIn.triggeredFire.frontendHit.asUInt)} " +
    p"backend hit vec ${Binary(io.cfIn.triggeredFire.backendHit.asUInt)}\n")

  val iduExceptionVec = io.cfIn.exceptionVec
  val raiseExceptionVec = csrExceptionVec.asUInt | iduExceptionVec.asUInt
  val raiseException = raiseExceptionVec.orR | hasTriggerFire
  val regularExceptionNO = ExcPriority.foldRight(0.U)((i: Int, sum: UInt) => Mux(raiseExceptionVec(i), i.U, sum))  
  val exceptionNO = Mux(hasSingleStep || hasTriggerFire, 3.U, regularExceptionNO)
  val causeNO = (raiseIntr << (XLEN - 1)) | Mux(raiseIntr, intrNO, exceptionNO)
  io.intrNO := Mux(raiseIntr, causeNO, 0.U)
  val causeNo_wire = (raiseIntr_wire << (XLEN - 1)) | Mux(raiseIntr_wire, intrNO_wire, exceptionNO) //do not support

  val raiseExceptionIntr = (raiseException || raiseIntr) && RegNext(io.instrValid)
  //val raiseExceptionIntr_wire = (raiseException || raiseIntr_wire) && io.instrValid //dont support raise exception
  val raiseExceptionIntr_wire = (raiseException || raiseIntr_wire) && hasValidInst
  io.wenFix := raiseExceptionIntr_wire
  val retTarget = Wire(UInt(VAddrBits.W))
  val trapTarget = Wire(UInt(VAddrBits.W))

  val hasBreakPoint = io.in.valid && isEbreak
  val hasDebugEbreakException = hasBreakPoint && ebreakEnterDebugMode
  val hasDebugTriggerException = hasTriggerFire && triggerFireAction === TrigActionEnum.DEBUG_MODE
  // temporaily donnot consider singlestep
  val hasDebugException = hasDebugEbreakException || hasDebugTriggerException
  val hasDebugTrap = hasDebugException || hasDebugIntr          // 分为 debug 中断和 debug 例外, 暂时不考虑 debug 例外
  val ebreakEnterParkLoop = debugMode && raiseExceptionIntr

  io.redirect.valid := (valid && func === SSDCSROpType.jmp) || hasValidInst && raiseExceptionIntr_wire /*raiseExceptionIntr*/ || resetSatp || frontendTriggerUpdate || memTriggerUpdate
  io.redirect.rtype := 0.U
  io.redirect.target := Mux(resetSatp || frontendTriggerUpdate || memTriggerUpdate, io.cfIn.pc + 4.U, Mux(raiseExceptionIntr_wire, trapTarget, retTarget))
  io.redirect.btbIsBranch := 0.U
  io.redirect.pc := io.cfIn.pc

  // Branch control

  val debugTrapTarget = Mux(!isEbreak && debugMode, 0x38020808.U, 0x38020800.U) // 0x808 is when an exception occurs in debug mode prog buf exec
  val deleg = Mux(raiseIntr, mideleg, medeleg)
  // val delegS = ((deleg & (1 << (causeNO & 0xf))) != 0) && (priviledgeMode < ModeM);
  val delegS = (deleg(causeNO(3, 0))) && (priviledgeMode < ModeM)
  val tvalWen = raiseIntr // in Core-riscv64, no exception will come together with PF

  ret := isMret || isSret || isUret || isDret
  trapTarget := Mux((hasDebugTrap && !debugMode) || ebreakEnterParkLoop, debugTrapTarget, Mux(delegS, stvec, mtvec)(VAddrBits - 1, 0))
  retTarget := DontCare

  when (valid && isDret) {
    val mstatusNew = WireInit(mstatus.asTypeOf(new MstatusStruct))
    val debugModeNew = WireInit(debugMode)
    when (dcsr.asTypeOf(new DcsrStruct).prv =/= ModeM) {mstatusNew.mprv := 0.U} //If the new privilege mode is less privileged than M-mode, MPRV in mstatus is cleared.
    mstatus := mstatusNew.asUInt
    mstatus_wire := mstatusNew.asUInt
    priviledgeMode := dcsr.asTypeOf(new DcsrStruct).prv
    debugModeNew := false.B
    debugIntrEnable := true.B
    debugMode := debugModeNew
    retTarget := dpc(VAddrBits-1, 0)
    Debug("Debug Mode: Dret executed, returning to %x. \n", retTarget)
  } 

  when (valid && isMret) {
    val mstatusOld = WireInit(mstatus.asTypeOf(new MstatusStruct))
    val mstatusNew = WireInit(mstatus.asTypeOf(new MstatusStruct))
    // mstatusNew.mpp.m := ModeU //TODO: add mode U
    mstatusNew.ie.m := mstatusOld.pie.m
    priviledgeMode := mstatusOld.mpp
    mstatusNew.pie.m := true.B
    mstatusNew.mpp := ModeU
    mstatus := mstatusNew.asUInt
    mstatus_wire := mstatusNew.asUInt
    lr := false.B
    retTarget := mepc(VAddrBits - 1, 0)
  }

  when (valid && isSret) {
    val mstatusOld = WireInit(mstatus.asTypeOf(new MstatusStruct))
    val mstatusNew = WireInit(mstatus.asTypeOf(new MstatusStruct))
    // mstatusNew.mpp.m := ModeU //TODO: add mode U
    mstatusNew.ie.s := mstatusOld.pie.s
    priviledgeMode := Cat(0.U(1.W), mstatusOld.spp)
    mstatusNew.pie.s := true.B
    mstatusNew.spp := ModeU
    mstatus := mstatusNew.asUInt
    mstatus_wire := mstatusNew.asUInt
    lr := false.B
    retTarget := sepc(VAddrBits - 1, 0)
  }

  when (valid && isUret) {
    val mstatusOld = WireInit(mstatus.asTypeOf(new MstatusStruct))
    val mstatusNew = WireInit(mstatus.asTypeOf(new MstatusStruct))
    // mstatusNew.mpp.m := ModeU //TODO: add mode U
    mstatusNew.ie.u := mstatusOld.pie.u
    priviledgeMode := ModeU
    mstatusNew.pie.u := true.B
    mstatus := mstatusNew.asUInt
    mstatus_wire := mstatusNew.asUInt
    retTarget := uepc(VAddrBits - 1, 0)
  }

  when (raiseExceptionIntr && addr === Mstatus.U && io.in.valid) {
    val mstatusOld = WireInit(mstatus.asTypeOf(new MstatusStruct))
    val mstatusNew = WireInit(mstatus.asTypeOf(new MstatusStruct))

    when (delegS) {
      scause := causeNO
      sepc := SignExt(io.cfIn.pc, XLEN)
      mstatusNew.spp := priviledgeMode
      mstatusNew.pie.s := mstatusOld.ie.s
      mstatusNew.ie.s := false.B
      priviledgeMode := ModeS
      when(tvalWen) {
        stval := 0.U
      } // TODO: should not use =/=
      // printf("[*] mstatusNew.spp %x\n", mstatusNew.spp)
      // trapTarget := stvec(VAddrBits-1. 0)
    }.otherwise {
      mcause := causeNO
      mcause_wire := causeNO
      mepc := RegNext(SignExt(io.cfIn.pc, XLEN))
      mepc_wire := RegNext(SignExt(io.cfIn.pc, XLEN))
      mstatusNew.mpp := (priviledgeMode)
      mstatusNew.pie.m := mstatusOld.ie.m
      mstatusNew.ie.m := false.B
      priviledgeMode := ModeM
      when(tvalWen) {
        mtval := 0.U
      }
      // TODO: should not use =/=
      // trapTarget := mtvec(VAddrBits-1. 0)
    }

    mstatus := mstatusNew.asUInt
    mstatus_wire := mstatusNew.asUInt
  }.elsewhen(raiseExceptionIntr_wire) {
    // val mstatusOld = WireInit(mstatus.asTypeOf(new MstatusStruct))
    val mstatusNew1 = mstatusUpdateSideEffect(wdata).asTypeOf(new MstatusStruct)
    val mstatusNew2 = mstatus.asTypeOf(new MstatusStruct)
    val mstatusNew = WireInit(Mux(addr === Mstatus.U && io.in.valid, mstatusNew1, mstatusNew2))

    mcause := causeNo_wire
    mcause_wire := causeNo_wire
    mepc := (SignExt(io.cfIn.pc, XLEN))
    mepc_wire := (SignExt(io.cfIn.pc, XLEN))
    mstatusNew.mpp := (priviledgeMode)
    // mstatusNew.pie.m := mstatusUpdateSideEffect(wdata).ie.m && !(RegNext(wdata).asTypeOf(new MstatusStruct).ie.m)
    mstatusNew.ie.m := false.B
    mstatusNew.pie.m := (mstatus).asTypeOf(new MstatusStruct).ie.m
    priviledgeMode := ModeM
    when(tvalWen) {
      mtval := 0.U
    }

    mstatus := mstatusNew.asUInt
    mstatus_wire := mstatusNew.asUInt
    Debug("[SSDCSR] Intr or Exception\n")
  }.elsewhen(addr === Mstatus.U && io.in.valid) {
    val mstatusNew = WireInit(mstatusUpdateSideEffect(wdata).asTypeOf(new MstatusStruct))
    mstatus := mstatusNew.asUInt
    mstatus_wire := mstatusNew.asUInt
  }

  // jtag Debug Mode
  when (hasDebugTrap && hasValidInst) {
    val dcsrNew = WireInit(dcsr.asTypeOf(new DcsrStruct))
    val debugModeNew = WireInit(debugMode)
    when (!debugMode) {
      import DcsrStruct._
      debugModeNew := true.B
      dcsrNew.prv := priviledgeMode
      priviledgeMode := ModeM
      when (hasDebugIntr) {
        dpc := (SignExt(io.cfIn.pc, XLEN))
        dcsrNew.cause := CAUSE_HALTREQ
        Debug(hasDebugIntr, "Debug Mode: Trap to %x at pc %x\n", debugTrapTarget, dpc)
      }.otherwise {
        dpc := (SignExt(io.cfIn.pc, XLEN))
        dcsrNew.cause := MuxCase(0.U, Seq(
          hasTriggerFire -> CAUSE_TRIGGER,
          hasBreakPoint -> CAUSE_HALTREQ,
          hasSingleStep -> CAUSE_STEP
        ))
      }
      dcsr := dcsrNew.asUInt
      debugIntrEnable := false.B
    }.elsewhen (debugMode) {
      // do nothing
    }.otherwise {
      // do nothing 
    }
    debugMode := debugModeNew
  }

  io.in.ready := true.B
  io.out.valid := valid

  // perfcnt

  val generalPerfCntList = Map(
    "Mcycle" -> (0xb00, "perfCntCondMcycle"),
    "Minstret" -> (0xb02, "perfCntCondMinstret"),
    "MultiCommit" -> (0xb03, "perfCntCondMultiCommit"),
    "MimemStall" -> (0xb04, "perfCntCondMimemStall"),
    "MaluInstr" -> (0xb05, "perfCntCondMaluInstr"),
    "MbruInstr" -> (0xb06, "perfCntCondMbruInstr"),
    "MlsuInstr" -> (0xb07, "perfCntCondMlsuInstr"),
    "MmduInstr" -> (0xb08, "perfCntCondMmduInstr"),
    "McsrInstr" -> (0xb09, "perfCntCondMcsrInstr"),
    "MloadInstr" -> (0xb0a, "perfCntCondMloadInstr"),
    "MmmioInstr" -> (0xb0b, "perfCntCondMmmioInstr"),
    "MicacheHit" -> (0xb0c, "perfCntCondMicacheHit"),
    "MdcacheHit" -> (0xb0d, "perfCntCondMdcacheHit"),
    "MmulInstr" -> (0xb0e, "perfCntCondMmulInstr"),
    "MifuFlush" -> (0xb0f, "perfCntCondMifuFlush"),
    "MbpBRight" -> (0xb10, "MbpBRight"),
    "MbpBWrong" -> (0xb11, "MbpBWrong"),
    "MbpJRight" -> (0xb12, "MbpJRight"),
    "MbpJWrong" -> (0xb13, "MbpJWrong"),
    "MbpIRight" -> (0xb14, "MbpIRight"),
    "MbpIWrong" -> (0xb15, "MbpIWrong"),
    "MbpRRight" -> (0xb16, "MbpRRight"),
    "MbpRWrong" -> (0xb17, "MbpRWrong"),
    "Ml2cacheHit" -> (0xb18, "perfCntCondMl2cacheHit"),
    "Custom1" -> (0xb19, "Custom1"),
    "Custom2" -> (0xb1a, "Custom2"),
    "Custom3" -> (0xb1b, "Custom3"),
    "Custom4" -> (0xb1c, "Custom4"),
    "Custom5" -> (0xb1d, "Custom5"),
    "Custom6" -> (0xb1e, "Custom6"),
    "Custom7" -> (0xb1f, "Custom7"),
    "Custom8" -> (0xb20, "Custom8")
  )

  val sequentialPerfCntList = Map(
    "MrawStall" -> (0xb31, "perfCntCondMrawStall"),
    "MexuBusy" -> (0xb32, "perfCntCondMexuBusy"),
    "MloadStall" -> (0xb33, "perfCntCondMloadStall"),
    "MstoreStall" -> (0xb34, "perfCntCondMstoreStall"),
    "ISUIssue" -> (0xb35, "perfCntCondISUIssue")
  )

  val outOfOrderPerfCntList = Map(
    "MrobFull" -> (0xb31, "perfCntCondMrobFull"),
    "Malu1rsFull" -> (0xb32, "perfCntCondMalu1rsFull"),
    "Malu2rsFull" -> (0xb33, "perfCntCondMalu2rsFull"),
    "MbrursFull" -> (0xb34, "perfCntCondMbrursFull"),
    "MlsursFull" -> (0xb35, "perfCntCondMlsursFull"),
    "MmdursFull" -> (0xb36, "perfCntCondMmdursFull"),
    "MmemqFull" -> (0xb37, "perfCntCondMmemqFull"),
    "MrobEmpty" -> (0xb38, "perfCntCondMrobEmpty"),
    "MstqFull" -> (0xb39, "perfCntCondMstqFull"),
    "McmtCnt0" -> (0xb40, "perfCntCondMcmtCnt0"),
    "McmtCnt1" -> (0xb41, "perfCntCondMcmtCnt1"),
    "McmtCnt2" -> (0xb42, "perfCntCondMcmtCnt2"),
    "McmtStrHaz1" -> (0xb43, "perfCntCondMcmtStrHaz1"),
    "McmtStrHaz2" -> (0xb44, "perfCntCondMcmtStrHaz2"),
    "MaluInstr2" -> (0xb45, "perfCntCondMaluInstr2"),
    "Mdispatch0" -> (0xb46, "perfCntCondMdispatch0"),
    "Mdispatch1" -> (0xb47, "perfCntCondMdispatch1"),
    "Mdispatch2" -> (0xb48, "perfCntCondMdispatch2"),
    "MlsuIssue" -> (0xb49, "perfCntCondMlsuIssue"),
    "MmduIssue" -> (0xb4a, "perfCntCondMmduIssue"),
    "MbruCmt" -> (0xb4b, "perfCntCondMbruCmt"),
    "MbruCmtWrong" -> (0xb4c, "perfCntCondMbruCmtWrong"),
    "MicacheLoss" -> (0xb4d, "perfCntCondMicacheLoss"),
    "MdcacheLoss" -> (0xb4e, "perfCntCondMdcacheLoss"),
    "Ml2cacheLoss" -> (0xb4f, "perfCntCondMl2cacheLoss"),
    "MbrInROB_0" -> (0xb50, "perfCntCondMbrInROB_0"),
    "MbrInROB_1" -> (0xb51, "perfCntCondMbrInROB_1"),
    "MbrInROB_2" -> (0xb52, "perfCntCondMbrInROB_2"),
    "MbrInROB_3" -> (0xb53, "perfCntCondMbrInROB_3"),
    "MbrInROB_4" -> (0xb54, "perfCntCondMbrInROB_4"),
    "Mdp1StBlk" -> (0xb55, "perfCntCondMdp1StBlk"),
    "Mdp1StRSf" -> (0xb56, "perfCntCondMdp1StRSf"),
    "Mdp1StROBf" -> (0xb57, "perfCntCondMdp1StROBf"),
    "Mdp1StConf" -> (0xb58, "perfCntCondMdp1StConf"),
    "Mdp1StCnt" -> (0xb59, "perfCntCondMdp1StCnt"),
    "Mdp2StBlk" -> (0xb5a, "perfCntCondMdp2StBlk"),
    "Mdp2StRSf" -> (0xb5b, "perfCntCondMdp2StRSf"),
    "Mdp2StROBf" -> (0xb5c, "perfCntCondMdp2StROBf"),
    "Mdp2StConf" -> (0xb5d, "perfCntCondMdp2StConf"),
    "Mdp2StSeq" -> (0xb5e, "perfCntCondMdp2StSeq"),
    "Mdp2StCnt" -> (0xb5f, "perfCntCondMdp2StCnt"),
    "MloadCnt" -> (0xb60, "perfCntCondMloadCnt"),
    "MstoreCnt" -> (0xb61, "perfCntCondMstoreCnt"),
    "MmemSBL" -> (0xb62, "perfCntCondMmemSBL"),
    "MpendingLS  " -> (0xb63, "perfCntCondMpendingLS"), //Maunally updated
    "MpendingSCmt" -> (0xb64, "perfCntCondMpendingSCmt"), //Maunally updated
    "MpendingSReq" -> (0xb65, "perfCntCondMpendingSReq"), //Maunally updated
    "MicacheReq" -> (0xb66, "perfCntCondMicacheReq"),
    "MdcacheReq" -> (0xb67, "perfCntCondMdcacheReq"),
    "Ml2cacheReq" -> (0xb68, "perfCntCondMl2cacheReq"),
    "MdpNoInst" -> (0xb69, "perfCntCondMdpNoInst")
    // "MmemLBS"  -> (0xb6a, "perfCntCondMmemLBS"   ),//TODO
  )

  val perfCntList = generalPerfCntList ++ (if (EnableOutOfOrderExec) outOfOrderPerfCntList else sequentialPerfCntList)

  val perfCntCond = List.fill(0x80)(WireInit(false.B))
  (perfCnts zip perfCntCond).map { case (c, e) => {
    when(e) {
      c := c + 1.U
    }
  }
  }
  // Manually update perf counter
  val pendingLS = WireInit(0.U(5.W))
  val pendingSCmt = WireInit(0.U(5.W))
  val pendingSReq = WireInit(0.U(5.W))
  BoringUtils.addSink(pendingLS, "perfCntSrcMpendingLS")
  BoringUtils.addSink(pendingSCmt, "perfCntSrcMpendingSCmt")
  BoringUtils.addSink(pendingSReq, "perfCntSrcMpendingSReq")
  when(perfCntCond(0xb03 & 0x7f)) {
    perfCnts(0xb02 & 0x7f) := perfCnts(0xb02 & 0x7f) + 2.U
  } // Minstret += 2 when MultiCommit
  if (hasPerfCnt) {
    when(true.B) {
      perfCnts(0xb63 & 0x7f) := perfCnts(0xb63 & 0x7f) + pendingLS
    }
    when(true.B) {
      perfCnts(0xb64 & 0x7f) := perfCnts(0xb64 & 0x7f) + pendingSCmt
    }
    when(true.B) {
      perfCnts(0xb65 & 0x7f) := perfCnts(0xb66 & 0x7f) + pendingSReq
    }
  }

  BoringUtils.addSource(WireInit(true.B), "perfCntCondMcycle")
  perfCntList.map { case (name, (addr, boringId)) => {
    BoringUtils.addSink(perfCntCond(addr & 0x7f), boringId)
    if (!hasPerfCnt) {
      // do not enable perfcnts except for Mcycle and Minstret
      if (addr != perfCntList("Mcycle")._1 && addr != perfCntList("Minstret")._1) {
        perfCntCond(addr & 0x7f) := false.B
      }
    }
  }
  }

  val Coretrap = WireInit(false.B)
  BoringUtils.addSink(Coretrap, "Coretrap")

  def readWithScala(addr: Int): UInt = mapping(addr)._1

  val mtvec_wire = WireInit(UInt(XLEN.W), 0.U)
  val mtval_wire = WireInit(UInt(XLEN.W), 0.U)
  val mscratch_wire = WireInit(UInt(XLEN.W), 0.U)
  val mideleg_wire = WireInit(UInt(XLEN.W), 0.U)
  val medeleg_wire = WireInit(UInt(XLEN.W), 0.U)

  mtvec_wire := mtvec
  mtval_wire := mtval
  mscratch_wire := mscratch
  mideleg_wire := mideleg
  medeleg_wire := medeleg

  when(addr === Mtvec.U && io.in.valid) {
    mtvec_wire := wdata
  }.elsewhen(addr === Mepc.U && io.in.valid) {
    mepc_wire := wdata
  }.elsewhen(addr === Mie.U && io.in.valid) {
    mie_wire := wdata
  }.elsewhen(addr === Mtval.U && io.in.valid) {
    mtval_wire := wdata
  }.elsewhen(addr === Mideleg.U && io.in.valid) {
    mideleg_wire := wdata
  }.elsewhen(addr === Medeleg.U && io.in.valid) {
    medeleg_wire := wdata
  }.elsewhen(addr === Mscratch.U && io.in.valid) {
    mscratch_wire := wdata
  }

  BoringUtils.addSource(mtvec_wire, "mtvec_wire")
  BoringUtils.addSource(mcause_wire, "mcause_wire")
  BoringUtils.addSource(mepc_wire, "mepc_wire")
  BoringUtils.addSource(mstatus_wire, "mstatus_wire")
  BoringUtils.addSource(mie_wire, "mie_wire")
  BoringUtils.addSource(mtval_wire, "mtval_wire")
  BoringUtils.addSource(mscratch_wire, "mscratch_wire")
  BoringUtils.addSource(medeleg_wire, "medeleg_wire")
  BoringUtils.addSource(mideleg_wire, "mideleg_wire")


  /*io.CSRregfile.priviledgeMode := priviledgeMode
  io.CSRregfile.mstatus := mstatus //wrong
  //io.CSRregfile.sstatus := mstatus_wire & sstatusRmask //modify
  io.CSRregfile.sstatus := mstatus & sstatusRmask
  io.CSRregfile.mepc := mepc
  io.CSRregfile.sepc := sepc
  io.CSRregfile.mtval := mtval
  io.CSRregfile.stval := stval
  io.CSRregfile.mtvec := mtvec
  io.CSRregfile.stvec := stvec
  io.CSRregfile.mcause := mcause
  io.CSRregfile.scause := scause
  io.CSRregfile.satp := satp
  io.CSRregfile.mip := mipReg
  io.CSRregfile.mie := mie
  io.CSRregfile.mscratch := mscratch
  io.CSRregfile.sscratch := sscratch
  io.CSRregfile.mideleg := mideleg
  io.CSRregfile.medeleg := medeleg*/
  io.CSRregfile.priviledgeMode := priviledgeMode
  io.CSRregfile.mstatus := mstatus_wire //wrong
  //io.CSRregfile.sstatus := mstatus_wire & sstatusRmask //modify
  io.CSRregfile.sstatus := mstatus_wire & sstatusRmask
  io.CSRregfile.mepc := mepc_wire
  io.CSRregfile.sepc := sepc
  io.CSRregfile.mtval := mtval_wire
  io.CSRregfile.stval := stval
  io.CSRregfile.mtvec := mtvec_wire
  io.CSRregfile.stvec := stvec
  io.CSRregfile.mcause := mcause_wire
  io.CSRregfile.scause := scause
  io.CSRregfile.satp := satp
  io.CSRregfile.mip := mipReg
  io.CSRregfile.mie := mie_wire
  io.CSRregfile.mscratch := mscratch_wire
  io.CSRregfile.sscratch := sscratch
  io.CSRregfile.mideleg := mideleg_wire
  io.CSRregfile.medeleg := medeleg_wire


  io.ArchEvent.intrNO := Mux(raiseIntr_wire && hasValidInst, intrNO_wire, 0.U)
  io.ArchEvent.exceptionPC := (SignExt(io.cfIn.pc, XLEN))
  io.ArchEvent.exceptionInst := (io.cfIn.instr)
  io.ArchEvent.cause := (Mux(raiseException && hasValidInst, exceptionNO, 0.U))

}

