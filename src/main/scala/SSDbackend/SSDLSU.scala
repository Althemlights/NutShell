package SSDbackend

import bus.simplebus.{SimpleBusCmd, SimpleBusReqBundle, SimpleBusUC}
import chisel3._
import nutcore._
import chisel3.util._
import _root_.utils.{LookupTree, SignExt}
import chisel3.util.experimental.BoringUtils
import firrtl.WireKind
import _root_.utils._

//class FunctionUnitIO extends NutCoreBundle {
//  val in = Flipped(Decoupled(new Bundle {
//    val src1 = Output(UInt(XLEN.W))
//    val src2 = Output(UInt(XLEN.W))
//    val func = Output(FuOpType())
//  }))
//  val out = Decoupled(Output(UInt(XLEN.W)))

class SSDLSUIO extends FunctionUnitIO{
  val offset = Input(UInt(XLEN.W))
  val memStall = Output(Bool())
  val flush = Input(Bool())
  val LoadReady = Output(Bool())
  val StoreReady = Output(Bool())
  val dmem = new SimpleBusUC(addrBits = VAddrBits) // without dtlb
  val NotReadyWhenReq = Output(Bool())
  //val mmio = new SimpleBusUC
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
  val io = IO(new SSDLSUIO)
  val (valid, src1, src2, func, flush, offset) = (io.in.valid, io.in.bits.src1, io.in.bits.src2, io.in.bits.func, io.flush, io.offset)
  def access(valid: Bool, src1: UInt, src2: UInt, func: UInt, offset: UInt): UInt = {
    this.valid := valid
    this.src1 := src1
    this.src2 := src2
    this.func := func
    this.offset := offset
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

  val addr = src1 + offset
  val wdata = src2
  val size = func(1,0)
  val isStore = valid && LSUOpType.isStore(func)
  val isLoad = valid && LSUOpType.isLoad(func)

  //load process
  val loadProcessing = RegInit(false.B)
  when(isLoad){loadProcessing := true.B}.elsewhen(io.out.fire()){loadProcessing := false.B}
  BoringUtils.addSource(loadProcessing,"loadProcessing")
  io.NotReadyWhenReq := valid && !io.dmem.req.ready

  val reqAddr  = addr(VAddrBits-1,0)
  val reqWdata = genWdata(wdata, size)
  val reqWmask = genWmask(addr, size)

  //store pipeline
  val lsuPipeIn = Wire(Vec(3,Flipped(Decoupled(new storePipeEntry))))
  val lsuPipeOut = Wire(Vec(3,Decoupled(new storePipeEntry)))
  val lsuPipeStage2 = Module(new normalPipeConnect(new storePipeEntry)).suggestName("memStage2")
  val lsuPipeStage3 = Module(new normalPipeConnect(new storePipeEntry)).suggestName("memStage3")
  val lsuPipeStage4 = Module(new stallPointConnect(new storePipeEntry)).suggestName("memStage4")
  //cache signal
  val cacheIn = Wire(Decoupled(new SimpleBusReqBundle))
  val StoreCacheIn = Wire(Decoupled(new SimpleBusReqBundle))
  val LoadCacheIn = Wire(Decoupled(new SimpleBusReqBundle))
  dontTouch(cacheIn)
  dontTouch(StoreCacheIn)
  dontTouch(LoadCacheIn)
  io.dmem.req <> cacheIn
  io.dmem.resp.ready := true.B
  //store buffer
  val storeBuffer = Module(new StoreBuffer)
  //stall signal
  val memStall = WireInit(Bool(),false.B)
  //val CacheStall = lsuPipeOut(0).valid && !lsuPipeOut(0).bits.isStore && lsuPipeOut(1).valid && !lsuPipeOut(1).bits.isStore
  BoringUtils.addSink(memStall,"memStall")
  io.memStall := memStall

  lsuPipeIn(0).valid := isStore || cacheIn.fire()
  lsuPipeIn(0).bits.isStore := isStore
  lsuPipeIn(0).bits.paddr := reqAddr(PAddrBits-1,0)
  lsuPipeIn(0).bits.data := reqWdata
  lsuPipeIn(0).bits.size := size
  lsuPipeIn(0).bits.mask := reqWmask
  lsuPipeIn(0).bits.func := func
  lsuPipeIn(0).bits.isCacheStore := cacheIn.valid && cacheIn.bits.cmd === SimpleBusCmd.write
  lsuPipeOut(2).ready := storeBuffer.io.in.ready
  lsuPipeStage4.io.isStall := memStall

  io.LoadReady := cacheIn.ready && !storeBuffer.io.isFull
  io.StoreReady := lsuPipeIn(0).ready

  for(i <- 1 to 2){
    lsuPipeIn(i).bits := lsuPipeOut(i-1).bits
    lsuPipeIn(i).valid := lsuPipeOut(i-1).valid
    lsuPipeOut(i-1).ready := lsuPipeIn(i).ready
  }
  val storePipeList0 = List(lsuPipeStage4)
  val pipeIndexList0 = List(2)
  (storePipeList0 zip pipeIndexList0).foreach{ case(a,b) =>
    a.io.left <> lsuPipeIn(b)
    a.io.right <> lsuPipeOut(b)
    a.io.rightOutFire <> lsuPipeOut(b).fire()
    a.io.isFlush <> flush
  }
  val storePipeList1 = List(lsuPipeStage2,lsuPipeStage3)
  val pipeIndexList1 = List(0,1)
  (storePipeList1 zip pipeIndexList1).foreach{ case(a,b) =>
    a.io.left <> lsuPipeIn(b)
    a.io.right <> lsuPipeOut(b)
    a.io.rightOutFire <> lsuPipeOut(b).fire()
    a.io.isFlush <> flush
  }
  //store buffer
  //cache
  //io.dmem <> Cache(in = cacheIn, mmio = Vec(io.mmio), flush = Cat(io.flush,io.flush), empty = DontCare, enable = HasDcache)(CacheConfig(ro = false, name = "dcache"))
  //BoringUtils.addSource(io.flush,"cacheStall")
  //load/store issue ctrl (issue to DCache)
  StoreCacheIn.bits.apply(
    addr = storeBuffer.io.out.bits.paddr,
    size = storeBuffer.io.out.bits.size,
    wdata = storeBuffer.io.out.bits.data,
    wmask = storeBuffer.io.out.bits.mask,
    cmd = SimpleBusCmd.write
  )
  StoreCacheIn.valid := storeBuffer.io.out.valid
  storeBuffer.io.out.ready := StoreCacheIn.ready
  LoadCacheIn.bits.apply(
    addr = reqAddr,
    size = size,
    wdata = 0.U(XLEN.W), // not used in load
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
  cacheIn.bits :=  Mux(storeBuffer.io.isFull,cacheInArbiter0.io.out.bits,cacheInArbiter1.io.out.bits)
  cacheIn.valid :=  Mux(storeBuffer.io.isFull,cacheInArbiter0.io.out.valid,cacheInArbiter1.io.out.valid)
  cacheInArbiter0.io.out.ready := cacheIn.ready
  cacheInArbiter1.io.out.ready := cacheIn.ready
  //store buffer pointer
  val readAddr, writeAddr, headAddr, tailAddr = Wire(UInt(log2Up(StoreBufferSize).W))
  val readFlag, writeFlag = WireInit(false.B)
  readFlag := storeBuffer.io.readPtr(log2Up(StoreBufferSize))
  readAddr := storeBuffer.io.readPtr(log2Up(StoreBufferSize)-1,0)
  writeFlag := storeBuffer.io.writePtr(log2Up(StoreBufferSize))
  writeAddr := storeBuffer.io.writePtr(log2Up(StoreBufferSize)-1,0)
  headAddr := Mux(readFlag =/= writeFlag,Cat(1.U,writeAddr),Cat(0.U,writeAddr))
  tailAddr := Cat(0.U,writeAddr)

  //storeBuffer hit check
  val cacheStage2Raddr = WireInit(0.U(32.W))
  val cacheStage2Size = WireInit(0.U(3.W))
  BoringUtils.addSink(cacheStage2Raddr,"cacheStage2Raddr")
  BoringUtils.addSink(cacheStage2Size,"cacheStage2Size")

  val cacheStage2Mask = genWmask(cacheStage2Raddr,cacheStage2Size)

  val SBaddrHitVec = Wire(Vec(StoreBufferSize,Bool()))
  val SBaddrHit = Wire(Bool())
  val storeDataVec = Wire(Vec(StoreBufferSize,UInt(XLEN.W)))
  val SBMaskHitVec = Wire(Vec(StoreBufferSize,UInt((XLEN/8).W)))
  val SBhitData = Wire(UInt(XLEN.W))
  val SBhitMask = Wire(UInt((XLEN/8).W))
  val snapshot = storeBuffer.io.snapshot

  for(i <- 0 to StoreBufferSize-1){
    SBaddrHitVec(i) := cacheStage2Raddr(StoreBufferSize-1,3) === snapshot(i).paddr(StoreBufferSize-1,3)
    storeDataVec(i) := snapshot(i).data
    SBMaskHitVec(i) := snapshot(i).mask
  }
  //store buffer hit data process
  def hitCheck(headAddr:UInt, tailAddr:UInt, vecChecked:Vec[Bool]):Bool ={
    val outVec = WireInit(VecInit(Seq.fill(StoreBufferSize)(false.B)))
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
  dontTouch(SBaddrHit)
  dontTouch(SBaddrHitVec)
  //store pipeline stage3,4 hit check and hit data merge
  val stage3hit = Wire(Bool())
  val stage4hit = Wire(Bool())
  val stage3hitData = Wire(UInt(XLEN.W))
  val stage3hitMask = Wire(UInt((XLEN/8).W))
  val stage4hitData = Wire(UInt(XLEN.W))
  val stage4hitMask = Wire(UInt((XLEN/8).W))
  stage3hit := cacheStage2Raddr(PAddrBits-1,3) === lsuPipeStage3.right.bits.paddr(PAddrBits-1,3) && lsuPipeStage3.right.valid
  stage4hit := cacheStage2Raddr(PAddrBits-1,3) === lsuPipeStage4.right.bits.paddr(PAddrBits-1,3) && lsuPipeStage4.right.valid
  stage4hitMask := lsuPipeStage4.right.bits.mask | SBhitMask
  stage4hitData := MergeData(lsuPipeStage4.right.bits.data,SBhitData,lsuPipeStage4.right.bits.mask,SBhitMask)
  stage3hitMask := lsuPipeStage3.right.bits.mask | stage4hitMask
  stage3hitData := MergeData(lsuPipeStage3.right.bits.data,stage4hitData,lsuPipeStage3.right.bits.mask,stage4hitMask)

  //fianl hit data & mask
  val addrHit = Wire(Bool())       //just means an address hit, not a data hit
  val addrHitVec = Wire(UInt(3.W))
  val partialHit = Wire(Bool())    //Contains partial data you need
  val fullHit = Wire(Bool())       //Contains all the data you need
  val hitData = Wire(UInt(XLEN.W))
  val hitMask = Wire(UInt((XLEN/8).W))

  addrHitVec := Cat(stage3hit,stage4hit,SBaddrHit)
  addrHit := addrHitVec.orR
  hitData := PriorityMux(Seq(stage3hit,stage4hit,SBaddrHit),Seq(stage3hitData,stage4hitData,SBhitData))
  hitMask := PriorityMux(Seq(stage3hit,stage4hit,SBaddrHit),Seq(stage3hitMask,stage4hitMask,SBhitMask))
  fullHit := (cacheStage2Mask & hitMask).orR && !(cacheStage2Mask & ~hitMask).orR
  partialHit := !fullHit

  //BoringUtils.addSource(addrHit,"addrHit")
  //BoringUtils.addSource(fullHit,"fullHit")

  //store hit signal buffer
  val storeHitCtrl = Module(new stallPointConnect(new StoreHitCtrl))
  storeHitCtrl.left.valid := lsuPipeOut(0).valid && !lsuPipeOut(0).bits.isStore && !lsuPipeOut(0).bits.isCacheStore && addrHit && (fullHit || partialHit)
  storeHitCtrl.right.ready := lsuPipeIn(2).ready
  storeHitCtrl.rightOutFire := lsuPipeOut(1).fire() && !lsuPipeOut(1).bits.isStore && !lsuPipeOut(1).bits.isCacheStore
  storeHitCtrl.left.bits.addrHit := addrHit
  storeHitCtrl.left.bits.fullHit := fullHit
  storeHitCtrl.left.bits.hitMask := hitMask
  storeHitCtrl.left.bits.hitData := hitData
  storeHitCtrl.isStall := memStall
  storeHitCtrl.isFlush := flush

  val hitCtrlSignal = storeHitCtrl.right.bits

  //merge and shift -> final data (E3 means in stage3)
  val addrE3 = lsuPipeOut(1).bits.paddr
  val addrHitE3 = hitCtrlSignal.addrHit && storeHitCtrl.right.valid
  val partialHitDataE3 = MaskExpand(hitCtrlSignal.hitMask) & hitCtrlSignal.hitData | ~MaskExpand(hitCtrlSignal.hitMask) & io.dmem.resp.bits.rdata
  val hitDataE3 = Mux(addrHitE3,Mux(hitCtrlSignal.fullHit,hitCtrlSignal.hitData,partialHitDataE3) ,io.dmem.resp.bits.rdata)
  dontTouch(addrHitE3)
  dontTouch(partialHitDataE3)
  dontTouch(hitDataE3)

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
  val rdataPartialLoad = LookupTree(lsuPipeOut(1).bits.func, List(
    LSUOpType.lb   -> SignExt(rdataSel(7, 0) , XLEN),
    LSUOpType.lh   -> SignExt(rdataSel(15, 0), XLEN),
    LSUOpType.lw   -> SignExt(rdataSel(31, 0), XLEN),
    LSUOpType.lbu  -> ZeroExt(rdataSel(7, 0) , XLEN),
    LSUOpType.lhu  -> ZeroExt(rdataSel(15, 0), XLEN),
    LSUOpType.lwu  -> ZeroExt(rdataSel(31, 0), XLEN)
  ))
  val partialLoad = !lsuPipeOut(1).bits.isStore && (lsuPipeOut(1).bits.func =/= LSUOpType.ld) && lsuPipeOut(1).valid
  val rdataFinal = Mux(partialLoad,rdataPartialLoad,hitDataE3)
  //LSU out
  io.in.ready := lsuPipeIn(0).ready || LoadCacheIn.ready
  io.out.valid := io.dmem.resp.fire() || addrHitE3
  dontTouch(io.out.valid)
  io.out.bits := rdataFinal
  //store buffer snapshit
  storeBuffer.io.snapshotena := SBaddrHit
  storeBuffer.io.in.valid := lsuPipeStage4.io.right.valid && lsuPipeStage4.io.right.bits.isStore
  lsuPipeStage4.io.right.ready := storeBuffer.io.in.ready
  storeBuffer.io.in.bits.paddr := lsuPipeStage4.io.right.bits.paddr
  storeBuffer.io.in.bits.data := lsuPipeStage4.io.right.bits.data
  storeBuffer.io.in.bits.mask := lsuPipeStage4.io.right.bits.mask
  storeBuffer.io.in.bits.size := lsuPipeStage4.io.right.bits.size
  //mmio
}

class SSDLSU_fake extends  NutCoreModule with HasStoreBufferConst {
  val io = IO(new SSDLSUIO)
  val (valid, src1, src2, func, flush) = (io.in.valid, io.in.bits.src1, io.in.bits.src2, io.in.bits.func, io.flush)

  def access(valid: Bool, src1: UInt, src2: UInt, func: UInt): UInt = {
    this.valid := valid
    this.src1 := src1
    this.src2 := src2
    this.func := func
    io.out.bits
  }
  io.memStall := false.B
  io.LoadReady := false.B
  io.StoreReady := false.B
  io.dmem.req.valid := false.B
  io.dmem.req.bits := DontCare
  io.dmem.resp.ready := false.B
  io.in.ready := false.B
  io.out.bits := 0.U
  io.out.valid := false.B

  //for test
//  val a = WireInit(false.B)
//  BoringUtils.addSink(a,"d")

//  BoringUtils.addSource(false.B,"cacheStall")
//  BoringUtils.addSource(false.B,"addrHit")
//  BoringUtils.addSource(false.B,"fullHit")


}