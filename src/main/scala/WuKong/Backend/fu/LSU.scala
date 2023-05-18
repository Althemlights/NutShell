package WuKong.Backend.fu

import WuKong.Backend._
import WuKong._
import _root_.utils._
import bus.simplebus.{SimpleBusCmd, SimpleBusReqBundle, SimpleBusUC}
import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import WuKong.Backend.fu.LoadPipe.LoadPipe
import utils.stallPointConnect
import WuKong.{CoreModule, AddressSpace}
class LSArbiterIO[T <: Data](private val gen: T, val n: Int) extends Bundle {
  // See github.com/freechipsproject/chisel3/issues/765 for why gen is a private val and proposed replacement APIs.

  val in  = Flipped(Vec(n, Decoupled(gen)))
  val out = Decoupled(gen)
  val store = Input(Bool())
  val chosen = Output(UInt(log2Ceil(n).W))
}

//only support two inputs!
class LSArbiter[T <: Data](val gen: T, val n: Int) extends Module {
  val io = IO(new LSArbiterIO(gen, n))

  io.chosen := (n-1).asUInt
  io.out.bits := io.in(n-1).bits
  for (i <- n-2 to 0 by -1) {
    when (io.in(i).valid && !io.store) {
      io.chosen := i.asUInt
      io.out.bits := io.in(i).bits
    }.elsewhen(io.in(i).valid && io.store) {
      io.chosen := i.asUInt +1.U
      io.out.bits := io.in(i+1).bits
    }
  }

  // val grant = ArbiterCtrl(io.in.map(_.valid))
  val grantL = VecInit(true.B, false.B)
  val grantS = VecInit(false.B, true.B)
  val grant = Mux(io.store, grantS, grantL)
  for ((in, g) <- io.in zip grant)
    in.ready := g && io.out.ready
  io.out.valid := io.in(0).valid || io.in(1).valid
}

object LSUOpType { //TODO: refactor LSU fuop
  def lb   = "b0000000".U
  def lh   = "b0000001".U
  def lw   = "b0000010".U
  def ld   = "b0000011".U
  def lbu  = "b0000100".U
  def lhu  = "b0000101".U
  def lwu  = "b0000110".U
  def sb   = "b0001000".U
  def sh   = "b0001001".U
  def sw   = "b0001010".U
  def sd   = "b0001011".U

  def lr      = "b0100000".U
  def sc      = "b0100001".U
  def amoswap = "b0100010".U
  def amoadd  = "b1100011".U
  def amoxor  = "b0100100".U
  def amoand  = "b0100101".U
  def amoor   = "b0100110".U
  def amomin  = "b0110111".U
  def amomax  = "b0110000".U
  def amominu = "b0110001".U
  def amomaxu = "b0110010".U

  def isAdd(func: UInt) = func(6)
  def isAtom(func: UInt): Bool = func(5)
  def isStore(func: UInt): Bool = func(3)
  def isLoad(func: UInt): Bool = !isStore(func) & !isAtom(func)
  def isLR(func: UInt): Bool = func === lr
  def isSC(func: UInt): Bool = func === sc
  def isAMO(func: UInt): Bool = isAtom(func) && !isLR(func) && !isSC(func)

  def needMemRead(func: UInt): Bool = isLoad(func) || isAMO(func) || isLR(func)
  def needMemWrite(func: UInt): Bool = isStore(func) || isAMO(func) || isSC(func)

  def atomW = "010".U
  def atomD = "011".U
}

class LSUIO extends BankedFunctionUnitIO {
  val memStall = Output(Bool())
  val invalid = Input(Vec(3,Bool()))
  val dmem = Vec(2, new SimpleBusUC(addrBits = VAddrBits))

  val storeBypassCtrl = Flipped((new LSUPipeBypassCtrl).storeBypassCtrlE2)
  val storeBypassPort = Flipped((new StorePipeBypassPort).storeBypassPortE2)
  val isMMIO = Output(Vec(2,Bool()))
}

class StoreHitCtrl extends Bundle{
  val hit = Output(Bool())
  val hitData = Output(UInt(64.W))
  val hitMask = Output(UInt(8.W))
}
class storePipeEntry extends StoreBufferEntry{
  val isCacheStore        = Output(Bool())
  val isMMIOStore         = Output(Bool())
  val isI0MMIOStore       = Output(Bool())
  val isI1MMIOStore       = Output(Bool())
  val func                = Output(UInt(7.W))
  val offset              = Output(UInt(64.W))
  val rs1                 = Output(UInt(64.W))
  val mergeAddr           = Output(Bool())
}

class LSU extends  CoreModule with HasStoreBufferConst{
  val io = IO(new LSUIO)
  val (valid, src1, src2, func, invalid, offset) = (
    VecInit(io.in(0).valid,io.in(1).valid),
    VecInit(io.in(0).bits.src1, io.in(1).bits.src1),
    VecInit(io.in(0).bits.src2, io.in(1).bits.src2),
    VecInit(io.in(0).bits.func, io.in(1).bits.func),
    io.invalid,
    VecInit(io.in(0).bits.offset, io.in(1).bits.offset))
//  def access(valid: Vec[Bool] , src1: Vec[UInt], src2: Vec[UInt], func: Vec[UInt], offset: Vec[UInt]): UInt = {
//    this.valid := valid
//    this.src1 := src1
//    this.src2 := src2
//    this.func := func
//    this.offset := offset
//    io.out.bits
//  }
  def genWmask(addr: UInt, sizeEncode: UInt): UInt = {
    LookupTree(sizeEncode, List(
      "b00".U -> 0x1.U, //0001 << addr(2:0)
      "b01".U -> 0x3.U, //0011
      "b10".U -> 0xf.U, //1111
      "b11".U -> 0xff.U //11111111
    )) << addr(2, 0)
  }
  def genWdata(data: UInt, sizeEncode: UInt): UInt = {
    LookupTree(sizeEncode, List(
      "b00".U -> Fill(8, data(7, 0)),
      "b01".U -> Fill(4, data(15, 0)),
      "b10".U -> Fill(2, data(31, 0)),
      "b11".U -> data
    ))
  }

  
  val i0isLoad  = valid(0) && LSUOpType.isLoad(func(0))
  val i0isStore = valid(0) && LSUOpType.isStore(func(0))
  val i1isLoad  = valid(1) && LSUOpType.isLoad(func(1))
  val i1isStore = valid(1) && LSUOpType.isStore(func(1))

  val bothLoad = i0isLoad && i1isLoad
  dontTouch(bothLoad)

  val wdata = Mux(i0isStore,src2(0),src2(1))
  val size = Mux(i0isStore,func(0)(1,0),func(1)(1,0))
  val storeOffset = Mux(i0isStore, offset(0),offset(1))
  val storeSrc1 = Mux(i0isStore, src1(0),src1(1))
  val storeFunc = Mux(i0isStore, func(0),func(1))
  
  val storeReqAddr  = Mux(i0isStore,src1(0) + offset(0),src1(1) + offset(1))
  val storeReqWdata = genWdata(wdata, size)
  val storeReqWmask = genWmask(storeReqAddr, size)

  val name0 = "load0"
  val name1 = "load1"
  val loadPipe0 = Module(new LoadPipe()(name0))
  val loadPipe1 = Module(new LoadPipe()(name1))
  //store pipeline
  /*
  ||----------EX2------------||
  ||--------memStage3--------|| register <-  invalid(0)
  ||----------EX3------------||
  ||--------memStage4--------|| register <-  invalid(1)
  ||----------EX4------------||
  ||-------storeBuffer-------|| register <-  invalid(2)
   */
  val lsuPipeIn = Wire(Vec(2,Flipped(Decoupled(new storePipeEntry))))
  val lsuPipeOut = Wire(Vec(2,Decoupled(new storePipeEntry)))
  val lsuPipeStage3 = Module(new stallPointConnect(new storePipeEntry)).suggestName("memStage3")
  val lsuPipeStage4 = Module(new stallPointConnect(new storePipeEntry)).suggestName("memStage4")
  //cache signal
  // storeCacheIn \ loadCacheIn --> cacheIn
  val cacheIn = Wire(Decoupled(Vec(2, new SimpleBusReqBundle)))
  val storeCacheIn = Wire(Decoupled(Vec(2, new SimpleBusReqBundle)))
  val loadCacheIn = Wire(Decoupled(Vec(2, new SimpleBusReqBundle)))



  //store buffer
  val storeBuffer = Module(new StoreBuffer)



  //MMIO & OutBuffer
  val outBuffer = Module(new Queue(new StoreBufferEntry, entries = 1))
  val MMIOStorePkt = Wire(Decoupled(new StoreBufferEntry))
  val isMMIOStore = AddressSpace.isMMIO(storeReqAddr) && (i0isStore || i1isStore)
  val MMIOStorePending = (lsuPipeStage4.right.valid && lsuPipeStage4.right.bits.isMMIOStore) || outBuffer.io.deq.valid

  outBuffer.io.enq.valid := lsuPipeStage4.right.valid && lsuPipeStage4.right.bits.isMMIOStore && !invalid(2)
  outBuffer.io.enq.bits.data := lsuPipeStage4.right.bits.data
  outBuffer.io.enq.bits.mask := lsuPipeStage4.right.bits.mask
  outBuffer.io.enq.bits.size := lsuPipeStage4.right.bits.size
  outBuffer.io.enq.bits.paddr := lsuPipeStage4.right.bits.paddr
  outBuffer.io.deq.ready := MMIOStorePkt.ready
  MMIOStorePkt.bits := outBuffer.io.deq.bits
  MMIOStorePkt.valid := outBuffer.io.deq.valid
  MMIOStorePkt.ready := false.B
  val outBufferFire = outBuffer.io.deq.fire()
  BoringUtils.addSource(MMIOStorePkt.valid,"MMIOStorePktValid")
  BoringUtils.addSource(MMIOStorePkt.bits,"MMIOStorePktBits")
  BoringUtils.addSink(MMIOStorePkt.ready,"MMIOStorePktReady")
  BoringUtils.addSource(MMIOStorePending,"MMIOStorePending")
  BoringUtils.addSource(outBufferFire,"outBufferFire")
  
  val loads2valid0 = WireInit(false.B)
  val loads2valid1 = WireInit(false.B)
  loads2valid0 := loadPipe0.io.loadS2Valid
  loads2valid1 := loadPipe1.io.loadS2Valid
  //stall signal
  val cacheStoreStall = WireInit(false.B)
  BoringUtils.addSink(cacheStoreStall,"cacheStoreStall")
  val cacheLoadStall = WireInit(false.B)
  BoringUtils.addSink(cacheLoadStall,"cacheLoadStall")
  // val bufferFullStall = (storeBuffer.io.isAlmostFull && (lsuPipeOut(1).valid || loads2valid0 || loads2valid1) ) || storeBuffer.io.isFull  //when almost full, still can store one
  val bufferFullStall = storeBuffer.io.isAlmostFull || storeBuffer.io.isFull  //when almost full, still can store one
  BoringUtils.addSource(bufferFullStall,"bufferFullStall")
  

  
  val real_bank_conflict = WireInit(false.B)
  BoringUtils.addSink(real_bank_conflict,"real_bank_conflict")

  // io.memStall := (cacheStall || !cacheIn.ready) && (i0isLoad || i1isLoad || loads2valid0 || loads2valid1) || bufferFullStall || real_bank_conflict
  io.memStall := (cacheStoreStall) && (i0isLoad || i1isLoad || loads2valid0 || loads2valid1) || bufferFullStall || real_bank_conflict || (cacheLoadStall ) ||
      !cacheIn.ready && (i0isLoad || i1isLoad)

  lsuPipeIn(0).valid := (i0isStore || i1isStore) && !invalid(0)
  lsuPipeIn(0).bits.paddr := storeReqAddr(PAddrBits-1,0)
  lsuPipeIn(0).bits.offset := storeOffset
  lsuPipeIn(0).bits.rs1 := storeSrc1
  lsuPipeIn(0).bits.mergeAddr := i0isStore && io.storeBypassCtrl.asUInt.orR
  lsuPipeIn(0).bits.data := storeReqWdata
  lsuPipeIn(0).bits.size := size
  lsuPipeIn(0).bits.mask := storeReqWmask
  lsuPipeIn(0).bits.func := storeFunc
  lsuPipeIn(0).bits.isCacheStore := cacheIn.fire() && cacheIn.bits(0).cmd === SimpleBusCmd.write
  lsuPipeIn(0).bits.isMMIOStore := isMMIOStore
  lsuPipeIn(0).bits.isI0MMIOStore := isMMIOStore && i0isStore
  lsuPipeIn(0).bits.isI1MMIOStore := isMMIOStore && i1isStore
  lsuPipeOut(1).ready := !bufferFullStall
  lsuPipeStage3.io.isStall := false.B
  lsuPipeStage4.io.isStall := io.memStall //There is only one stall point in LSU


  loadPipe0.io.in.bits.src1 := src1(0)
  loadPipe0.io.in.bits.offset := offset(0)
  loadPipe0.io.in.bits.func := func(0)
  loadPipe0.io.in.valid := valid(0) && i0isLoad
  loadPipe0.io.storePipeE3.bits <> lsuPipeOut(0).bits
  loadPipe0.io.storePipeE4.bits <> lsuPipeOut(1).bits
  loadPipe0.io.storePipeE3.valid <> lsuPipeOut(0).valid
  loadPipe0.io.storePipeE4.valid <> lsuPipeOut(1).valid
  loadPipe0.io.storebuffer <> storeBuffer.io.snapshot
  loadPipe0.io.writePtr <> storeBuffer.io.writePtr
  loadPipe0.io.readPtr <> storeBuffer.io.readPtr
  loadPipe0.io.dmem.req.bits <> loadCacheIn.bits(0)
  loadPipe0.io.dmem.req.ready := loadCacheIn.ready
  loadPipe0.io.dmem.resp <> io.dmem(0).resp
  loadPipe0.io.invalid <> invalid(0)
  loadPipe0.io.stall := io.memStall

  loadPipe1.io.in.bits.src1 := src1(1)
  loadPipe1.io.in.bits.offset := offset(1)
  loadPipe1.io.in.bits.func := func(1)
  loadPipe1.io.in.valid := valid(1) && i1isLoad
  loadPipe1.io.storePipeE3.bits <> lsuPipeOut(0).bits
  loadPipe1.io.storePipeE4.bits <> lsuPipeOut(1).bits
  loadPipe1.io.storePipeE3.valid <> lsuPipeOut(0).valid
  loadPipe1.io.storePipeE4.valid <> lsuPipeOut(1).valid
  loadPipe1.io.storebuffer <> storeBuffer.io.snapshot
  loadPipe1.io.writePtr <> storeBuffer.io.writePtr
  loadPipe1.io.readPtr <> storeBuffer.io.readPtr
  loadPipe1.io.dmem.req.bits <> loadCacheIn.bits(1)
  loadPipe1.io.dmem.req.ready := loadCacheIn.ready
  loadPipe1.io.dmem.resp <> io.dmem(1).resp
  loadPipe1.io.invalid <> invalid(0)
  loadPipe1.io.stall := io.memStall

  io.out(0) <> loadPipe0.io.out
  io.out(1) <> loadPipe1.io.out
  dontTouch(io.out)

  for(i <- 1 to 1){
    lsuPipeIn(i).bits := lsuPipeOut(i-1).bits
    lsuPipeIn(i).valid := lsuPipeOut(i-1).valid
    lsuPipeOut(i-1).ready := lsuPipeIn(i).ready
  }

  //store pipeline Rs bypass 
  val bypassEnaE2 = io.storeBypassCtrl.asUInt.orR && lsuPipeIn(0).valid
  val bypassDataE2 = PriorityMux(io.storeBypassCtrl,io.storeBypassPort)
  val bypassWdata = genWdata(bypassDataE2,lsuPipeIn(0).bits.func(1,0))
  lsuPipeIn(0).bits.data := Mux(bypassEnaE2,bypassWdata,storeReqWdata)

  val lsuPipeList0 = List(lsuPipeStage3,lsuPipeStage4)
  val pipeIndexList0 = List(0,1)
  (lsuPipeList0 zip pipeIndexList0).foreach{ case(a,b) =>
    a.io.left <> lsuPipeIn(b)
    a.io.right <> lsuPipeOut(b)
    a.io.rightOutFire <> lsuPipeOut(b).fire()
    a.io.inValid <> invalid(b)
  }
  //store buffer
  //load/store issue ctrl (issue to DCache)
  storeCacheIn.bits(0).apply(
    addr = storeBuffer.io.out.bits.paddr,
    size = storeBuffer.io.out.bits.size,
    wdata = storeBuffer.io.out.bits.data,
    wmask = storeBuffer.io.out.bits.mask,
    cmd = SimpleBusCmd.write
  )
  storeCacheIn.bits(1).apply( //invalid
    addr = 0.U,
    size = 0.U,
    wdata = 0.U,
    wmask = 0.U,
    cmd = 0.U
  )

  storeCacheIn.valid := storeBuffer.io.out.valid
  loadCacheIn.valid := (io.in(0).valid && i0isLoad) || (io.in(1).valid && i1isLoad)
  storeBuffer.io.out.ready := storeCacheIn.ready && (!loadCacheIn.valid || bufferFullStall)

  val cacheInArbiter = Module(new LSArbiter(Vec(2, new SimpleBusReqBundle),2))
  
  val storeEn = storeBuffer.io.isAlmostFull || storeBuffer.io.isFull || !loadCacheIn.valid
  
  
  cacheInArbiter.io.in(0) <> loadCacheIn
  cacheInArbiter.io.in(1) <> storeCacheIn
  cacheInArbiter.io.store := storeEn
  cacheIn <> cacheInArbiter.io.out


  io.in(0).ready := lsuPipeIn(0).ready || loadCacheIn.ready
  io.in(1).ready := loadCacheIn.ready

  io.isMMIO(0) := lsuPipeStage3.right.bits.isI0MMIOStore || loadPipe0.io.mmio && loads2valid0
  io.isMMIO(1) := lsuPipeStage3.right.bits.isI1MMIOStore || loadPipe1.io.mmio && loads2valid1
  //store buffer snapshit
  storeBuffer.io.in.valid := lsuPipeStage4.io.right.valid && lsuPipeStage4.io.right.valid && !lsuPipeStage4.io.right.bits.isMMIOStore && !invalid(2)
  storeBuffer.io.in.bits.paddr := lsuPipeStage4.io.right.bits.paddr
  storeBuffer.io.in.bits.data := lsuPipeStage4.io.right.bits.data
  storeBuffer.io.in.bits.mask := lsuPipeStage4.io.right.bits.mask
  storeBuffer.io.in.bits.size := lsuPipeStage4.io.right.bits.size



  io.dmem(0).req.bits <> cacheIn.bits(0)
  // io.dmem(0).req.valid := Mux(storeCacheIn.fire(), storeBuffer.io.out.valid, io.in(0).valid && i0isLoad)

  io.dmem(0).req.valid := cacheIn.valid && Mux( storeEn ,true.B,  Mux(!(io.in(0).valid && i0isLoad) && !(io.in(1).valid && i1isLoad) , storeBuffer.io.out.valid, io.in(0).valid && i0isLoad))
  cacheIn.ready := io.dmem(0).req.ready && io.dmem(1).req.ready

  io.dmem(1).req.bits <> cacheIn.bits(1)
  io.dmem(1).req.valid := cacheIn.valid && io.in(1).valid && i1isLoad

}
