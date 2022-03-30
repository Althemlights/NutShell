package SSDbackend

import bus.simplebus.{SimpleBusCmd, SimpleBusReqBundle, SimpleBusUC}
import chisel3._
import nutcore._
import chisel3.util._
import _root_.utils.{LookupTree, SignExt}
import chisel3.util.experimental.BoringUtils
import utils._

//class FunctionUnitIO extends NutCoreBundle {
//  val in = Flipped(Decoupled(new Bundle {
//    val src1 = Output(UInt(XLEN.W))
//    val src2 = Output(UInt(XLEN.W))
//    val func = Output(FuOpType())
//  }))
//  val out = Decoupled(Output(UInt(XLEN.W)))

class SSDLSUIO extends FunctionUnitIO{
  val memStall = Output(Bool())
  val flush = Input(Bool())
  val LoadReady = Output(Bool())
  val StoreReady = Output(Bool())
  val dmem = new SimpleBusUC(addrBits = PAddrBits) // without dtlb
  val mmio = new SimpleBusUC
  //val isMMIO = Output(Bool())
}
class StoreHitCtrl extends Bundle{
  val addrHit = Output(Bool())
  val fullHit = Output(Bool())
  val hitData = Output(UInt(64.W))
  val hitMask = Output(UInt(8.W))
}
class storePipeEntry extends StoreBufferEntry{
  val isStore       = Output(Bool())
  val isCacheStore  = Output(Bool())
  val func          = Output(UInt(7.W))
}

class SSDLSU extends  NutCoreModule with HasStoreBufferConst{
  val io = new SSDLSUIO
  val (valid, src1, src2, func, flush) = (io.in.valid, io.in.bits.src1, io.in.bits.src2, io.in.bits.func, io.flush)
  def access(valid: Bool, src1: UInt, src2: UInt, func: UInt): UInt = {
    this.valid := valid
    this.src1 := src1
    this.src2 := src2
    this.func := func
    io.out.bits
  }
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

  val addr = src1 + src2
  val wdata = src2
  val size = func(1,0)
  val isStore = valid && LSUOpType.isStore(func)

  val reqAddr  = addr(PAddrBits-1,0)
  val reqWdata = genWdata(wdata, size)
  val reqWmask = genWmask(addr, size)

  //stall signal
  val memStall = WireInit(false.B)
  val CacheStall = storePipeOut(0).valid && !storePipeOut(0).bits.isStore && storePipeOut(1).valid && !storePipeOut(1).bits.isStore
  BoringUtils.addSink(memStall,"memStall")
  io.memStall := memStall
  //store pipeline
  val storePipeIn = Wire(Vec(3,Flipped(Decoupled(new storePipeEntry))))
  val storePipeOut = Wire(Vec(3,Decoupled(new storePipeEntry)))
  val storePipeStage0 = Module(new stallPointConnect(new storePipeEntry))
  val storePipeStage1 = Module(new normalPipeConnect(new storePipeEntry))
  val storePipeStage2 = Module(new stallPointConnect(new storePipeEntry))
  val storePipeStage3 = Module(new normalPipeConnect(new storePipeEntry))

  storePipeIn(0).valid := valid && isStore || cacheIn.req.fire()
  storePipeIn(0).bits.isStore := isStore
  storePipeIn(0).bits.paddr := reqAddr
  storePipeIn(0).bits.data := reqWdata
  storePipeIn(0).bits.size := size
  storePipeIn(0).bits.mask := reqWmask
  storePipeIn(0).bits.func := func
  storePipeIn(0).bits.isCacheStore := cacheIn.req.valid && cacheIn.req.bits.cmd === SimpleBusCmd.write
  storePipeOut(3).ready := storeBuffer.io.in.ready
  storePipeStage2.io.isStall := memStall
  storePipeStage0.io.isStall := CacheStall   //need to confirm

  io.LoadReady := cacheIn.req.ready && !storeBuffer.io.isFull
  io.StoreReady := storePipeIn(0).ready

  for(i <- 1 to 2){
    storePipeIn(i).bits := storePipeOut(i-1).bits
    storePipeIn(i).valid := storePipeOut(i-1).bits
    storePipeOut(i-1).ready := storePipeIn(i).ready
  }
  val storePipeList0 = List(storePipeStage0,storePipeStage2)
  val pipeIndexList0 = List(0,2)
  (storePipeList0 zip pipeIndexList0).foreach{ case(a,b) =>
    a.io.left <> storePipeIn(b)
    a.io.right <> storePipeOut(b)
    a.io.rightOutFire <> storePipeOut(b).fire()
    a.io.isFlush <> flush
  }
  val storePipeList1 = List(storePipeStage1,storePipeStage3)
  val pipeIndexList1 = List(1,3)
  (storePipeList1 zip pipeIndexList1).foreach{ case(a,b) =>
    a.io.left <> storePipeIn(b)
    a.io.right <> storePipeOut(b)
    a.io.rightOutFire <> storePipeOut(b).fire()
    a.io.isFlush <> flush
  }
  //store buffer
  val storeBuffer = Module(new StoreBuffer)
  val snapshot = storeBuffer.io.snapshot
  storeBuffer.io.snapshotena := SBaddrHit
  storeBuffer.io.in <> storePipeStage3.io.right
  //cache
  //io.dmem <> Cache(in = cacheIn, mmio = Vec(io.mmio), flush = Cat(io.flush,io.flush), empty = DontCare, enable = HasDcache)(CacheConfig(ro = false, name = "dcache"))
  BoringUtils.addSource(io.flush,"cacheStall")
  //load/store issue ctrl (issue to DCache)
  val cacheIn = Wire(Flipped(new SimpleBusUC(addrBits = PAddrBits)))
  val StoreCacheIn = Wire(Decoupled(new SimpleBusReqBundle))
  val LoadCacheIn = Wire(Decoupled(new SimpleBusReqBundle))
  StoreCacheIn.bits.apply(
    addr = storeBuffer.io.out.bits.paddr,
    size = storeBuffer.io.out.bits.size,
    wdata = storeBuffer.io.out.bits.data,
    wmask = storeBuffer.io.out.bits.mask,
    cmd = SimpleBusCmd.write
  )
  StoreCacheIn.valid := storeBuffer.io.out.valid
  LoadCacheIn.bits.apply(
    addr = reqAddr,
    size = size,
    wdata = 0.U, // not used in load
    wmask = reqWmask, // for partical check
    cmd = SimpleBusCmd.read
  )
  LoadCacheIn.valid :=  valid && !isStore

  val cacheInArbiter0 = Module(new Arbiter((new SimpleBusReqBundle),2)) //store has high priority
  cacheInArbiter0.io.in(0) <> StoreCacheIn
  cacheInArbiter0.io.in(1) <> LoadCacheIn
  val cacheInArbiter1 = Module(new Arbiter((new SimpleBusReqBundle),2)) //store has high priority
  cacheInArbiter1.io.in(0) <> StoreCacheIn
  cacheInArbiter1.io.in(1) <> LoadCacheIn
  cacheIn := Mux(storeBuffer.io.isFull,cacheInArbiter0.io.out,cacheInArbiter1.io.out)

  //store buffer pointer
  val readAddr, writeAddr, headAddr, tailAddr = Wire(UInt(log2Up(StoreBufferSize).W))
  val readFlag, writeFlag = Wire(UInt(1.W))
  Cat(readFlag,readAddr) := storeBuffer.io.readPtr
  Cat(writeFlag,writeAddr) := storeBuffer.io.writePtr
  headAddr := Mux(readFlag =/= writeFlag,Cat(1.U,writeAddr),Cat(0.U,writeAddr))
  tailAddr := Cat(0.U,writeAddr)

  //storeBuffer hit check
  val cacheStage2Raddr = Wire(UInt(PAddrBits.W))
  val cacheStage2Size = Wire(UInt(2.W))
  val cacheStage2Mask = genWmask(cacheStage2Raddr,cacheStage2Size)
  BoringUtils.addSink(cacheStage2Raddr,"cacheStage2Raddr")
  BoringUtils.addSink(cacheStage2Size,"cacheStage2Size")
  
  val SBaddrHitVec = Wire(Vec(StoreBufferSize,Bool()))
  val SBaddrHit = Wire(Bool())
  val storeDataVec = Wire(Vec(StoreBufferSize,UInt(XLEN.W)))
  val SBMaskHitVec = Wire(Vec(StoreBufferSize,UInt((XLEN/8).W)))
  val SBhitData = Wire(0.U(XLEN.W))
  val SBhitMask = Wire(0.U((XLEN/8).W))

  for(i <- 0 to StoreBufferSize-1){
    SBaddrHitVec(i) := cacheStage2Raddr(StoreBufferSize-1,3) === snapshot(i).paddr(StoreBufferSize-1,3)
    storeDataVec(i) := snapshot(i).data
    SBMaskHitVec(i) := snapshot(i).mask
  }
  //store buffer hit data process
  def hitCheck(headAddr:UInt, tailAddr:UInt, vecChecked:Vec[Bool]):Bool ={
    val outVec = Wire(VecInit(Seq.fill(StoreBufferSize)(false.B)))
    for(i <- 0 to StoreBufferSize*2-1){
      when(i.U < headAddr && i.U >= tailAddr){
        outVec(i.U) := vecChecked(i.U(log2Up(StoreBufferSize)-1,0))
      }.otherwise{
        outVec(i.U) := false.B
      }
    }
    outVec.asUInt.orR
  }
  SBaddrHit := hitCheck(headAddr,tailAddr,SBaddrHitVec)
  SBhitData := Mux1H(SBaddrHitVec,storeDataVec)
  SBhitMask := Mux1H(SBaddrHitVec,SBMaskHitVec)

  //store pipeline stage3,4 hit check and hit data merge
  val stage3hit = Wire(Bool())
  val stage4hit = Wire(Bool())
  val stage3hitData = Wire(0.U(XLEN.W))
  val stage3hitMask = Wire(0.U((XLEN/8).W))
  val stage4hitData = Wire(0.U(XLEN.W))
  val stage4hitMask = Wire(0.U((XLEN/8).W))
  stage3hit := cacheStage2Raddr(StoreBufferSize-1,3) === storePipeStage2.right.bits.paddr(StoreBufferSize-1,3)
  stage4hit := cacheStage2Raddr(StoreBufferSize-1,3) === storePipeStage3.right.bits.paddr(StoreBufferSize-1,3)
  stage4hitMask := storePipeStage3.right.bits.mask | SBhitMask
  stage4hitData := MergeData(storePipeStage3.right.bits.data,SBhitData,storePipeStage3.right.bits.mask,SBhitMask)
  stage3hitMask := storePipeStage2.right.bits.mask | stage4hitMask
  stage3hitData := MergeData(storePipeStage2.right.bits.data,stage4hitData,storePipeStage2.right.bits.mask,stage4hitMask)

  //fianl hit data & mask
  val addrHit = Wire(Bool())       //just means an address hit, not a data hit
  val addrHitVec = Wire(UInt(3.W))
  val partialHit = Wire(Bool())    //Contains partial data you need
  val fullHit = Wire(Bool())       //Contains all the data you need
  val hitData = Wire(0.U(XLEN.W))
  val hitMask = Wire(0.U((XLEN/8).W))

  addrHitVec := Cat(stage3hit,stage4hit,SBaddrHit)
  addrHit := addrHitVec.orR
  hitData := PriorityMux(Seq(stage3hit,stage4hit,SBaddrHit),Seq(stage3hitData,stage4hitData,SBhitData))
  hitMask := PriorityMux(Seq(stage3hit,stage4hit,SBaddrHit),Seq(stage3hitMask,stage4hitMask,SBhitMask))
  fullHit := (cacheStage2Mask & hitMask).orR && !(cacheStage2Mask & ~hitMask).orR
  partialHit := !fullHit

  BoringUtils.addSource(RegEnable(addrHit,addrHit),"addrHit")
  BoringUtils.addSource(RegEnable(fullHit,addrHit),"fullHit")

  //store hit signal buffer
  val storeHitCtrl = Module(new stallPointConnect(new StoreHitCtrl))
  storeHitCtrl.left.valid := storePipeOut(1).valid
  storeHitCtrl.right.ready := storePipeIn(3).ready
  storeHitCtrl.left.bits.addrHit := addrHit
  storeHitCtrl.left.bits.fullHit := fullHit
  storeHitCtrl.left.bits.hitMask := hitMask
  storeHitCtrl.left.bits.hitData := hitData
  storeHitCtrl.isStall := memStall
  storeHitCtrl.isFlush := flush

  val hitCtrlSignal = storeHitCtrl.right.bits

  //merge and shift -> final data (E3 means in stage3)
  val addrE3 = storePipeOut(2).bits.paddr
  val fullHitE3 = hitCtrlSignal.addrHit && hitCtrlSignal.fullHit
  val partialHitDataE3 = MaskExpand(hitMask) & hitCtrlSignal.hitData | io.dmem.resp.bits.rdata
  val hitDataE3 = Mux(fullHitE3,hitCtrlSignal.hitData,partialHitDataE3)  //need shift

  val rdataSel = LookupTree(addrE3(2, 0), List(
    "b000".U -> hitDataE3(63, 0),
    "b001".U -> hitDataE3(63, 8),
    "b010".U -> hitDataE3(63, 16),
    "b011".U -> hitDataE3(63, 24),
    "b100".U -> hitDataE3(63, 32),
    "b101".U -> hitDataE3(63, 40),
    "b110".U -> hitDataE3(63, 48),
    "b111".U -> hitDataE3(63, 56)
  ))
  val rdataPartialLoad = LookupTree(storePipeOut(2).bits.func, List(
    LSUOpType.lb   -> SignExt(rdataSel(7, 0) , XLEN),
    LSUOpType.lh   -> SignExt(rdataSel(15, 0), XLEN),
    LSUOpType.lw   -> SignExt(rdataSel(31, 0), XLEN),
    LSUOpType.lbu  -> ZeroExt(rdataSel(7, 0) , XLEN),
    LSUOpType.lhu  -> ZeroExt(rdataSel(15, 0), XLEN),
    LSUOpType.lwu  -> ZeroExt(rdataSel(31, 0), XLEN)
  ))
  val partialLoad = !storePipeOut(2).bits.isStore && (storePipeOut(2).bits.func =/= LSUOpType.ld)
  val rdataFinal = Mux(partialLoad,rdataPartialLoad,hitDataE3)
  //LSU out
  io.in.ready := storePipeIn(0).ready || LoadCacheIn.ready
  io.out.valid := io.dmem.resp.fire() || fullHitE3
  io.out.bits := rdataFinal
}

