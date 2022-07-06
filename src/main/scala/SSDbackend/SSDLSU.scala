package SSDbackend

import bus.simplebus.{SimpleBusCmd, SimpleBusReqBundle, SimpleBusUC}
import chisel3.{util, _}
import nutcore._
import chisel3.util._
import _root_.utils.{LookupTree, SignExt}
import chisel3.util.experimental.BoringUtils
import firrtl.WireKind
import _root_.utils._

class SSDLSUIO extends FunctionUnitIO{
  val offset = Input(UInt(XLEN.W))
  val memStall = Output(Bool())
  val flush = Input(Vec(4,Bool()))
  val LoadReady = Output(Bool())
  val StoreReady = Output(Bool())
  val dmem = new SimpleBusUC(addrBits = VAddrBits)
  val storeBypassCtrl = Flipped(new StorePipeBypassCtrl)
  val storeBypassPort = Flipped(new StorePipeBypassPort)
}
class StoreHitCtrl extends Bundle{
  val hit = Output(Bool())
  val hitData = Output(UInt(64.W))
  val hitMask = Output(UInt(8.W))
}
class storePipeEntry extends StoreBufferEntry{
  val isStore       = Output(Bool())
  val isCacheStore  = Output(Bool())
  val func          = Output(UInt(7.W))
  val pc            = Output(UInt(VAddrBits.W))
  val offset        = Output(UInt(64.W))
  val rs1           = Output(UInt(64.W))
  val mergeAddr     = Output(Bool())
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

  val reqAddr  = addr(VAddrBits-1,0)
  val reqWdata = genWdata(wdata, size)
  val reqWmask = genWmask(addr, size)

  //store pipeline
  /*
  ||----------EX1------------||
  ||--------memStage2--------|| register <- flush(0)
  ||----------EX2------------||
  ||--------memStage3--------|| register <- flush(1)
  ||----------EX3------------||
  ||--------memStage4--------|| register <- flush(2)
  ||----------EX4------------||
  ||-------storeBuffer-------|| register <- flush(3)
   */
  val lsuPipeIn = Wire(Vec(3,Flipped(Decoupled(new storePipeEntry))))
  val lsuPipeOut = Wire(Vec(3,Decoupled(new storePipeEntry)))
  val lsuPipeStage2 = Module(new stallPointConnect(new storePipeEntry)).suggestName("memStage2")
  val lsuPipeStage3 = Module(new stallPointConnect(new storePipeEntry)).suggestName("memStage3")
  val lsuPipeStage4 = Module(new stallPointConnect(new storePipeEntry)).suggestName("memStage4")
  //cache signal
  val cacheIn = Wire(Decoupled(new SimpleBusReqBundle))
  val s2NotReady = WireInit(false.B)
  val s3NotReady = WireInit(false.B)
  BoringUtils.addSink(s2NotReady,"s2NotReady")
  BoringUtils.addSink(s3NotReady,"s3NotReady")
  val StoreCacheIn = Wire(Decoupled(new SimpleBusReqBundle))
  val LoadCacheIn = Wire(Decoupled(new SimpleBusReqBundle))
  dontTouch(cacheIn)
  dontTouch(StoreCacheIn)
  dontTouch(LoadCacheIn)
  io.dmem.req <> cacheIn
  io.dmem.resp.ready := true.B
  //store buffer
  val storeBuffer = Module(new StoreBuffer)
  val cacheS2NotReady = s2NotReady  && LoadCacheIn.valid || !s2NotReady && LoadCacheIn.valid && storeBuffer.io.isAlmostFull
  val cacheS3NotReady = s3NotReady && lsuPipeStage2.right.valid && !lsuPipeStage2.right.bits.isStore && !lsuPipeStage2.right.bits.isCacheStore
  //stall signal
  val memStall = WireInit(false.B)
  BoringUtils.addSink(memStall,"memStall")
  val pc = WireInit(0.U(VAddrBits.W))
  BoringUtils.addSink(pc,"lsuPC")
  io.memStall := memStall && lsuPipeStage3.right.valid && !lsuPipeStage3.right.bits.isStore && !lsuPipeStage3.right.bits.isCacheStore ||
    !storeBuffer.io.in.ready && lsuPipeStage3.right.valid && lsuPipeStage3.right.bits.isStore && LoadCacheIn.valid ||
    cacheS2NotReady || cacheS3NotReady

  lsuPipeIn(0).valid := isStore  || LoadCacheIn.valid
  lsuPipeIn(0).bits.isStore := isStore
  lsuPipeIn(0).bits.paddr := reqAddr(PAddrBits-1,0)
  lsuPipeIn(0).bits.offset := offset
  lsuPipeIn(0).bits.rs1 := PriorityMux(io.storeBypassCtrl.storeBypassCtrlE1,io.storeBypassPort.storeBypassPortE1)
  lsuPipeIn(0).bits.mergeAddr := isStore && io.storeBypassCtrl.storeBypassCtrlE1.asUInt.orR
  lsuPipeIn(0).bits.data := reqWdata
  lsuPipeIn(0).bits.size := size
  lsuPipeIn(0).bits.mask := reqWmask
  lsuPipeIn(0).bits.func := func
  lsuPipeIn(0).bits.isCacheStore := cacheIn.fire() && cacheIn.bits.cmd === SimpleBusCmd.write
  lsuPipeIn(0).bits.pc := pc
  lsuPipeOut(2).ready := storeBuffer.io.in.ready    //when almost full, still can store one
  lsuPipeStage2.io.isStall := cacheS2NotReady
  lsuPipeStage3.io.isStall := cacheS3NotReady
  lsuPipeStage4.io.isStall := memStall


  io.LoadReady := cacheIn.ready && !storeBuffer.io.isAlmostFull
  io.StoreReady := lsuPipeIn(0).ready

  for(i <- 1 to 2){
    lsuPipeIn(i).bits := lsuPipeOut(i-1).bits
    lsuPipeIn(i).valid := lsuPipeOut(i-1).valid
    lsuPipeOut(i-1).ready := lsuPipeIn(i).ready
  }
  //store pipeline Rs bypass and addr merge
  val storeAddr = lsuPipeOut(0).bits.offset + lsuPipeOut(0).bits.rs1
  val storeMask = genWmask(storeAddr, lsuPipeOut(0).bits.func(1,0))
  lsuPipeIn(1).bits.paddr := Mux(lsuPipeOut(0).bits.mergeAddr,storeAddr(VAddrBits-1,0),lsuPipeOut(0).bits.paddr)
  lsuPipeIn(1).bits.mask := Mux(lsuPipeOut(0).bits.mergeAddr,storeMask,lsuPipeOut(0).bits.mask)
  val bypassEnaE2 = io.storeBypassCtrl.storeBypassCtrlE2.asUInt.orR && lsuPipeOut(0).bits.isStore
  val bypassDataE2 = PriorityMux(io.storeBypassCtrl.storeBypassCtrlE2,io.storeBypassPort.storeBypassPortE2)
  val bypassWdata = genWdata(bypassDataE2,lsuPipeOut(0).bits.func(1,0))
  lsuPipeIn(1).bits.data := Mux(bypassEnaE2,bypassWdata,lsuPipeOut(0).bits.data)

  val storePipeList0 = List(lsuPipeStage2,lsuPipeStage3,lsuPipeStage4)
  val pipeIndexList0 = List(0,1,2)
  (storePipeList0 zip pipeIndexList0).foreach{ case(a,b) =>
    a.io.left <> lsuPipeIn(b)
    a.io.right <> lsuPipeOut(b)
    a.io.rightOutFire <> lsuPipeOut(b).fire()
    a.io.isFlush <> flush(b)
  }
  //store buffer
  //load/store issue ctrl (issue to DCache)
  StoreCacheIn.bits.apply(
    addr = storeBuffer.io.out.bits.paddr,
    size = storeBuffer.io.out.bits.size,
    wdata = storeBuffer.io.out.bits.data,
    wmask = storeBuffer.io.out.bits.mask,
    cmd = SimpleBusCmd.write
  )

  StoreCacheIn.valid := storeBuffer.io.out.valid
  storeBuffer.io.out.ready := StoreCacheIn.ready//io.dmem.resp.fire() && io.dmem.resp.bits.cmd === SimpleBusCmd.writeResp    //data in storebuffer retires when store in cache is finished (maybe)
  dontTouch(io.dmem.resp.bits.cmd)
  LoadCacheIn.bits.apply(
    addr = reqAddr,
    size = size,
    wdata = 0.U(XLEN.W), // not used in load
    wmask = reqWmask, // for partical check
    cmd = SimpleBusCmd.read
  )
  LoadCacheIn.valid :=  valid && !isStore

  val cacheInArbiter0 = Module(new Arbiter((new SimpleBusReqBundle),2)) //store has higher priority,and store ready is driven by arbiter0, load ready is driven by arbiter1
  val cacheInArbiter1 = Module(new Arbiter((new SimpleBusReqBundle),2))
  cacheInArbiter0.io.in(0) <> StoreCacheIn
  cacheInArbiter0.io.in(1) <> LoadCacheIn
  cacheInArbiter1.io.in(0) <> LoadCacheIn
  cacheInArbiter1.io.in(1) <> StoreCacheIn
  StoreCacheIn.ready := Mux(storeBuffer.io.isAlmostFull, cacheInArbiter0.io.in(0).ready, cacheInArbiter1.io.in(1).ready)

  cacheIn.bits :=  Mux(storeBuffer.io.isAlmostFull,cacheInArbiter0.io.out.bits,cacheInArbiter1.io.out.bits)
  cacheIn.valid :=  Mux(storeBuffer.io.isAlmostFull,cacheInArbiter0.io.out.valid,cacheInArbiter1.io.out.valid)
  cacheInArbiter0.io.out.ready := cacheIn.ready
  cacheInArbiter1.io.out.ready := cacheIn.ready
  //store buffer pointer
  val readAddr, writeAddr = Wire(UInt(log2Up(StoreBufferSize).W))
  val headAddr, tailAddr = Wire(UInt((log2Up(StoreBufferSize)+1).W))
  val readFlag, writeFlag = WireInit(false.B)
  readFlag := storeBuffer.io.readPtr(log2Up(StoreBufferSize))
  readAddr := storeBuffer.io.readPtr(log2Up(StoreBufferSize)-1,0)
  writeFlag := storeBuffer.io.writePtr(log2Up(StoreBufferSize))
  writeAddr := storeBuffer.io.writePtr(log2Up(StoreBufferSize)-1,0)
  headAddr := Mux(readFlag =/= writeFlag,Cat(1.U(1.W),writeAddr),Cat(0.U(1.W),writeAddr))
  tailAddr := Cat(0.U(1.W),readAddr)
  dontTouch(headAddr)
  dontTouch(tailAddr)

  //storeBuffer hit check in lsu stage 2
  val beCheckedPaddr = lsuPipeStage2.right.bits.paddr
  val beCheckedSize = lsuPipeStage2.right.bits.size
  val beCheckedMask = genWmask(beCheckedPaddr,beCheckedSize)

  val SBaddrHitVec = Wire(Vec(StoreBufferSize,Bool()))
  val SBaddrHit = Wire(Bool())
  val SBDataVec = Wire(Vec(2*StoreBufferSize,UInt(XLEN.W)))
  val SBMaskVec = Wire(Vec(2*StoreBufferSize,UInt((XLEN/8).W)))
  val SBhitData = Wire(UInt(XLEN.W))
  val SBhitMask = Wire(UInt((XLEN/8).W))
  val snapshot = storeBuffer.io.snapshot

  for(i <- 0 to StoreBufferSize-1){
    SBaddrHitVec(i) := beCheckedPaddr(PAddrBits-1,3) === snapshot(i).paddr(PAddrBits-1,3)
  }
  for(i <- 0 to 2*StoreBufferSize-1){
    SBDataVec(i) := snapshot(i.U(log2Up(StoreBufferSize)-1,0)).data
    SBMaskVec(i) := snapshot(i.U(log2Up(StoreBufferSize)-1,0)).mask
  }
  //store buffer hit data process
  def hitCheck(headAddr:UInt, tailAddr:UInt, vecChecked:Vec[Bool]):Vec[Bool] ={
    val outVec = WireInit(VecInit(Seq.fill(2*StoreBufferSize)(false.B)))
    for(i <- 0 to StoreBufferSize*2-1){
      when(i.U < headAddr && i.U >= tailAddr){
        outVec(i.U) := vecChecked(i.U(log2Up(StoreBufferSize)-1,0))
      }.otherwise{
        outVec(i.U) := false.B
      }
    }
    outVec
  }
  val SBHitVecExpand = hitCheck(headAddr,tailAddr,SBaddrHitVec)
  SBaddrHit := SBHitVecExpand.asUInt.orR
  SBhitData := Mux1H(SBHitVecExpand,SBDataVec)
  SBhitMask := Mux1H(SBHitVecExpand,SBMaskVec)
  dontTouch(SBaddrHit)
  dontTouch(SBaddrHitVec)
  dontTouch(SBHitVecExpand)
  //store pipeline stage3,4 hit check and hit data merge
  val stage3hit = Wire(Bool())
  val stage4hit = Wire(Bool())
  val stage3hitData = Wire(UInt(XLEN.W))
  val stage3hitMask = Wire(UInt((XLEN/8).W))
  val stage4hitData = Wire(UInt(XLEN.W))
  val stage4hitMask = Wire(UInt((XLEN/8).W))
  stage3hit := beCheckedPaddr(PAddrBits-1,3) === lsuPipeStage3.right.bits.paddr(PAddrBits-1,3) && lsuPipeStage3.right.valid && lsuPipeStage3.right.bits.isStore
  stage4hit := beCheckedPaddr(PAddrBits-1,3) === lsuPipeStage4.right.bits.paddr(PAddrBits-1,3) && lsuPipeStage4.right.valid && lsuPipeStage4.right.bits.isStore
  stage4hitMask := (lsuPipeStage4.right.bits.mask & MaskExpand((lsuPipeStage4.right.valid && lsuPipeStage4.right.bits.isStore).asUInt))
  stage4hitData := MergeData(lsuPipeStage4.right.bits.data,SBhitData,stage4hitMask,SBhitMask)
  stage3hitMask := (lsuPipeStage3.right.bits.mask & MaskExpand((lsuPipeStage3.right.valid && lsuPipeStage3.right.bits.isStore).asUInt))
  stage3hitData := MergeData(lsuPipeStage3.right.bits.data,stage4hitData,stage3hitMask,stage4hitMask | SBhitMask)

  //fianl hit data & mask
  val addrHitVecE2 = Cat(stage3hit,stage4hit,SBaddrHit)
  val addrHitE2 = addrHitVecE2.orR
  val hitDataE2 = PriorityMux(Seq(stage3hit,stage4hit,SBaddrHit),Seq(stage3hitData,stage4hitData,SBhitData))
  val hitMaskE2 = PriorityMux(Seq(stage3hit,stage4hit,SBaddrHit),Seq(stage3hitMask|stage4hitMask|SBhitMask,stage4hitMask|SBhitMask,SBhitMask))
  val fullHitE2 = !(beCheckedMask & ~hitMaskE2).orR
  val partialHitE2 = (beCheckedMask & hitMaskE2).orR && (beCheckedMask & ~hitMaskE2).orR
  val missE2 = !addrHitE2 || (!fullHitE2 && !partialHitE2)
  val hitE2 = !missE2

  //store hit signal buffer
  val storeHitCtrl = Module(new stallPointConnect(new StoreHitCtrl))
  storeHitCtrl.left.valid := lsuPipeOut(0).valid && !lsuPipeOut(0).bits.isStore && !lsuPipeOut(0).bits.isCacheStore
  storeHitCtrl.right.ready := lsuPipeOut(1).ready
  storeHitCtrl.rightOutFire := lsuPipeOut(1).fire
  storeHitCtrl.left.bits.hit := hitE2
  storeHitCtrl.left.bits.hitMask := hitMaskE2
  storeHitCtrl.left.bits.hitData := hitDataE2
  storeHitCtrl.isStall := lsuPipeStage3.isStall
  storeHitCtrl.isFlush := flush(1)

  val hitCtrlSignal = storeHitCtrl.right.bits

  //merge and shift -> final data (E3 means in stage3)
  val addrE3 = lsuPipeOut(1).bits.paddr
  val addrHitE3 = storeHitCtrl.right.valid && storeHitCtrl.right.bits.hit
  val mergedDataE3 = MaskExpand(hitCtrlSignal.hitMask) & hitCtrlSignal.hitData | ~MaskExpand(hitCtrlSignal.hitMask) & io.dmem.resp.bits.rdata
  val hitDataE3 = Mux(addrHitE3,mergedDataE3,io.dmem.resp.bits.rdata)
  dontTouch(addrHitE3)
  dontTouch(hitDataE3)
  BoringUtils.addSource(addrHitE3,"storeHit")

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
  val rdataFinal = LookupTree(lsuPipeOut(1).bits.func, List(
    LSUOpType.lb   -> SignExt(rdataSel(7, 0) , XLEN),
    LSUOpType.lh   -> SignExt(rdataSel(15, 0), XLEN),
    LSUOpType.lw   -> SignExt(rdataSel(31, 0), XLEN),
    LSUOpType.lbu  -> ZeroExt(rdataSel(7, 0) , XLEN),
    LSUOpType.lhu  -> ZeroExt(rdataSel(15, 0), XLEN),
    LSUOpType.lwu  -> ZeroExt(rdataSel(31, 0), XLEN)
  ))

  //LSU out
  io.in.ready := lsuPipeIn(0).ready || LoadCacheIn.ready
  io.out.valid := (io.dmem.resp.fire() || addrHitE3) && lsuPipeStage3.right.valid && !lsuPipeStage3.right.bits.isStore && !lsuPipeStage3.right.bits.isCacheStore
  dontTouch(io.out.valid)
  val partialLoad = !lsuPipeOut(1).bits.isStore && (lsuPipeOut(1).bits.func =/= LSUOpType.ld) && lsuPipeOut(1).valid
  val out = Mux(partialLoad,rdataFinal,Mux(addrHitE3,mergedDataE3,io.dmem.resp.bits.rdata))
  val outBuffer = RegEnable(out,io.out.valid)
  io.out.bits := Mux(io.out.valid,out,outBuffer)
  //store buffer snapshit
  storeBuffer.io.in.valid := lsuPipeStage4.io.right.valid && lsuPipeStage4.io.right.bits.isStore && !flush(3)
  lsuPipeStage4.io.right.ready := storeBuffer.io.in.ready
  storeBuffer.io.in.bits.paddr := lsuPipeStage4.io.right.bits.paddr
  storeBuffer.io.in.bits.data := lsuPipeStage4.io.right.bits.data
  storeBuffer.io.in.bits.mask := lsuPipeStage4.io.right.bits.mask
  storeBuffer.io.in.bits.size := lsuPipeStage4.io.right.bits.size
  //mydebug
  val loadCond = lsuPipeOut(1).fire() && !lsuPipeOut(1).bits.isStore && !lsuPipeOut(1).bits.isCacheStore && io.out.valid
  val storeCond = lsuPipeOut(1).fire() && lsuPipeOut(1).bits.isStore
  dontTouch(loadCond)
  dontTouch(storeCond)
  val tag = lsuPipeOut(1).bits.paddr === "h8000ce7a".U
  dontTouch(tag)

  if(SSDCoreConfig().EnableLSUDebug){
  myDebug(loadCond, "Load  addr:%x, mask:%b, data:%x, at PC: %x\n",lsuPipeOut(1).bits.paddr,lsuPipeOut(1).bits.mask,io.out.bits,lsuPipeOut(1).bits.pc)
  myDebug(storeCond,"Store addr:%x, mask:%b, data:%x, at PC: %x\n",lsuPipeOut(1).bits.paddr,lsuPipeOut(1).bits.mask,lsuPipeOut(1).bits.data,lsuPipeOut(1).bits.pc)
}
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


}