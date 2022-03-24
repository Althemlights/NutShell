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
  val instr = Input(UInt(32.W)) // maybe not used
  val issueStall = Output(Bool())
  val memStall = Output(Bool())
  val flush = Input(Bool())
  val dmem = new SimpleBusUC(addrBits = PAddrBits) // without dtlb
  val mmio = new SimpleBusUC
  //val isMMIO = Output(Bool())
}

class SSDLSU extends  NutCoreModule with HasStoreBufferConst{
  val io = new SSDLSUIO
  val (valid, src1, src2, func, flush, memStall, issueStall) = (io.in.valid, io.in.bits.src1, io.in.bits.src2, io.in.bits.func, io.flush, io.memStall, io.issueStall)
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


  //store pipeline
  val storePipeIn = Wire(Vec(3,Flipped(Decoupled(new StoreBufferEntry))))
  val storePipeOut = Wire(Vec(3,Decoupled(new StoreBufferEntry)))
  val storePipeStage0 = Module(new stallPointConnect(new StoreBufferEntry))
  val storePipeStage1 = Module(new normalPipeConnect(new StoreBufferEntry))
  val storePipeStage2 = Module(new stallPointConnect(new StoreBufferEntry))

  storePipeIn(0).valid := valid
  storePipeIn(0).bits.isStore := isStore
  storePipeIn(0).bits.paddr := reqAddr
  storePipeIn(0).bits.data := reqWdata
  storePipeIn(0).bits.size := size
  storePipeIn(0).bits.mask := reqWmask
  storePipeOut(2).ready := storeBuffer.io.in.ready
  storePipeStage2.io.isStall := memStall
  storePipeStage0.io.isStall := issueStall

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
  val pipeIndexList1 = List(1)
  (storePipeList1 zip pipeIndexList1).foreach{ case(a,b) =>
    a.io.left <> storePipeIn(b)
    a.io.right <> storePipeOut(b)
    a.io.rightOutFire <> storePipeOut(b).fire()
    a.io.isFlush <> flush
  }
  //store buffer
  val storeBuffer = Module(new StoreBuffer)
  val snapshot = storeBuffer.io.snapshot
  storeBuffer.io.in <> storePipeStage3.io.right
  //cache
  io.dmem <> Cache(in = cacheIn, mmio = io.mmio, flush = Cat(io.flush,io.flush), empty = DontCare, enable = HasDcache)(CacheConfig(ro = false, name = "dcache"))
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
  StoreCacheIn.valid := !storeBuffer.io.out.valid && !LoadCacheIn.valid || storeBuffer.io.isFull

  LoadCacheIn.bits.apply(
    addr = reqAddr,
    size = size,
    wdata = 0.U, // not used in load
    wmask = reqWmask, // for partical check
    cmd = SimpleBusCmd.read
  )

  LoadCacheIn.valid :=  valid && !isStore && !storeBuffer.io.isFull

  val cacheInArbiter = Module(new Arbiter((new SimpleBusReqBundle),2))
  cacheInArbiter.io.in(0) <> StoreCacheIn
  cacheInArbiter.io.in(1) <> LoadCacheIn
  cacheInArbiter.io.out <> cacheIn

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

  val storeHit = Wire(Bool())
  val addrHitVec = Wire(Vec(StoreBufferSize,Bool()))
  val partialHitVec = Wire(Vec(StoreBufferSize,Bool()))
  val fullHitVec = Wire(Vec(StoreBufferSize,Bool()))
  val addrHit = Wire(Bool())
  val partialHit = Wire(Bool())
  val fullHit = Wire(Bool())
  val SBMaskHitVec = Wire(Vec(StoreBufferSize,UInt(8.W))) // load need and store buffer have
  val SBMaskMissVec = Wire(Vec(StoreBufferSize,UInt(8.W))) //load need but store buffer doesnt have
  val SBMaskHit = Wire(Vec(StoreBufferSize,Bool()))  // each byte hit or miss
  val SBMaskMiss = Wire(Vec(StoreBufferSize,Bool()))
  val storeDataVec = Wire(Vec(StoreBufferSize,UInt(XLEN.W)))
  val SBhitData = Wire(0.U(XLEN.W))
  val SBhitMask = Wire(0.U(XLEN/8.W))

  for(i <- 0 to StoreBufferSize-1){
    SBMaskHitVec(i) := cacheStage2Mask & snapshot(i).mask
    SBMaskMissVec(i) := cacheStage2Mask & ~snapshot(i).mask
    SBMaskHit(i) := SBMaskHitVec(i).orR
    SBMaskMiss(i) := SBMaskMissVec(i).orR
    partialHitVec(i) := SBMaskHit(i) && SBMaskMiss(i)
    fullHitVec(i) := SBMaskHit(i) && !SBMaskMiss(i)
    storeDataVec(i) := snapshot(i).data & MaskExpand(cacheStage2Mask) & MaskExpand(snapshot(i).mask)
  }
  //store buffer hit data process
  def hitCheck(headAddr:UInt, tailAddr:UInt, vecChecked:UInt):Bool ={
    val outVec = Wire(VecInit(Seq.fill(StoreBufferSize)(false.B)))
    for(i <- 0 to StoreBufferSize*2-1){
      if(i.U < headAddr && i.U >= tailAddr){
        outVec(i.U(log2Up(StoreBufferSize)-1)) :=  vecChecked(i.U(log2Up(StoreBufferSize)-1))
      }
    }
    outVec.asUInt.orR
  }
  addrHit := hitCheck(headAddr,tailAddr,addrHitVec) && storePipeOut(1).valid && !storePipeOut(1).bits.isStore
  partialHit := hitCheck(headAddr,tailAddr,partialHitVec)
  fullHit := hitCheck(headAddr,tailAddr,fullHitVec)
  SBhitData := Mux1H(addrHitVec,storeDataVec)
  SBhitMask := Mux1H(addrHitVec,SBMaskHitVec)

  BoringUtils.addSource(RegEnable(addrHit,addrHit),"storeBufferHit")
  BoringUtils.addSource(RegEnable(fullHit,addrHit),"fullHit")
  BoringUtils.addSource(RegEnable(SBhitData,addrHit),"storeBufferHitData")
  BoringUtils.addSource(RegEnable(SBhitMask,addrHit),"storeBufferHitMask")

  //LSU out
  io.in.ready := storePipeIn(0).ready || LoadCacheIn.ready
  io.out.valid := io.dmem.resp.fire()
  io.out.bits := io.dmem.resp.bits.rdata
}