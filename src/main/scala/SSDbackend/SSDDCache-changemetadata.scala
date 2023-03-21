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

package SSDbackend

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import bus.simplebus._
import bus.axi4._
import chisel3.experimental.IO
import com.google.protobuf.Internal.FloatList
import utils._
import top.Settings
import nutcore._
import system._

import freechips.rocketchip.tilelink._
import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp, TransferSizes}
import freechips.rocketchip.tilelink.{ClientMetadata, ClientStates}
import chipsalliance.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.rocket.constants.MemoryOpConstants
import freechips.rocketchip.tilelink.MemoryOpCategories._
import freechips.rocketchip.config.Parameters

case object DCacheParamsKey extends Field[DCacheParameters]

case class DCacheParameters (
                         ro: Boolean = false,
                         name: String = "dcache",
                         userBits: Int = 0,
                         idBits: Int = 0,

                         totalSize: Int = 16, // Kbytes
                         ways: Int = 4,
                         sramNum: Int = 4,
                         srcBits: Int = 1  
)

trait HasDCacheParameters {
  implicit val p: Parameters
  val cacheConfig = p(DCacheParamsKey)

  val PAddrBits: Int
  val XLEN: Int

  val cacheName = cacheConfig.name
  val userBits = cacheConfig.userBits
  val idBits = cacheConfig.idBits
  val srcBits = cacheConfig.srcBits

  val TotalSize = cacheConfig.totalSize
  val Ways = cacheConfig.ways
  val LineSize = XLEN // byte
  val LineBeats = LineSize / 8 //DATA W64IDTH 
  val Sets = TotalSize * 1024 / LineSize / Ways
  val OffsetBits = log2Up(LineSize)
  val IndexBits = log2Up(Sets)
  val WordIndexBits = log2Up(LineBeats)
  val TagBits = PAddrBits - OffsetBits - IndexBits
  val WayIdBits = log2Up(Ways)
  
  val sramNum = cacheConfig.sramNum
  val BankBits = log2Up(sramNum)

  def addrBundle = new Bundle {
    val tag = UInt(TagBits.W)
    val index = UInt(IndexBits.W)
    val wordIndex = UInt((WordIndexBits - BankBits).W)
    val bankIndex = UInt(BankBits.W)
    val byteOffset = UInt((if (XLEN == 64) 3 else 2).W)
  }

  def BankHitVec(addr: UInt) : UInt = {
    VecInit((0 until sramNum).map{m => m.U === getbankIdx(addr)}).asUInt
  }

  def CacheMetaArrayReadBus() = new SRAMReadBus(new DMetaBundle, set = Sets, way = Ways)
  def CacheTagArrayReadBus() = new SRAMReadBus(new DTagBundle, set = Sets, way = Ways)
  def CacheWayIdArrayReadBus() = new SRAMReadBus(new DWayIdBundle, set = Sets, way = 1)
  def CacheDataArrayReadBus() = new SRAMReadBus(new DDataBundle, set = Sets * LineBeats / sramNum, way = Ways)
  def CacheMetaArrayWriteBus() = new SRAMWriteBus(new DMetaBundle, set = Sets, way = Ways)
  def CacheTagArrayWriteBus() = new SRAMWriteBus(new DTagBundle, set = Sets, way = Ways)
  def CacheWayIdArrayWriteBus() = new SRAMWriteBus(new DWayIdBundle, set = Sets, way = 1)
  def CacheDataArrayWriteBus() = new SRAMWriteBus(new DDataBundle, set = Sets * LineBeats / sramNum, way = Ways)

  def getMetaIdx(addr: UInt) = addr.asTypeOf(addrBundle).index
  def getDataIdx(addr: UInt) = Cat(addr.asTypeOf(addrBundle).index, addr.asTypeOf(addrBundle).wordIndex)
  def getbankIdx(addr: UInt) = addr.asTypeOf(addrBundle).bankIndex

  def isSameWord(a1: UInt, a2: UInt) = ((a1 >> 2) == (a2 >> 2))
  def isSetConflict(a1: UInt, a2: UInt) = (a1.asTypeOf(addrBundle).index === a2.asTypeOf(addrBundle).index)

}

abstract class DCacheBundle() extends Bundle with HasNutCoreParameter with HasDCacheParameters with HasNutCoreParameters
abstract class DCacheModule() extends Module with HasNutCoreParameter with HasDCacheParameters with MemoryOpConstants with HasNutCoreParameters

class DWayIdBundle(implicit val p: Parameters) extends DCacheBundle {
  val way_id = Output(UInt((WayIdBits * Ways).W))

  def apply(way_id: UInt) = {
    this.way_id := way_id
    this
  }
}

class DTagBundle(implicit val p: Parameters) extends DCacheBundle {
  val tag = Output(UInt(TagBits.W))

  def apply(tag: UInt) = {
    this.tag := tag
    this
  }
}

class DMetaBundle(implicit val p: Parameters) extends DCacheBundle {
  val coh = Output(UInt(ClientStates.width.W))

  def apply(coh: ClientMetadata) = {
    this.coh := coh.asUInt
    this
  }
}

class DDataBundle(implicit val p: Parameters) extends DCacheBundle {
  val data = Output(UInt(DataBits.W))

  def apply(data: UInt) = {
    this.data := data
    this
  }
}

class DCacheIO(implicit val p: Parameters) extends Bundle with HasNutCoreParameter with HasDCacheParameters {
  val in = Flipped(new SimpleBusUC(userBits = userBits, idBits = idBits))
  val flush = Input(Bool())
}

trait HasDCacheIO {
  implicit val p: Parameters
  val io = IO(new DCacheIO)
}

sealed class ProbeVictimIO(implicit val p: Parameters) extends DCacheBundle {
  val index = Output(UInt(IndexBits.W))
  val victimWay = Output(UInt(Ways.W))
  val valid = Output(Bool())
}

sealed class DStage1IO(implicit val p: Parameters) extends DCacheBundle {
  val req = new SimpleBusReqBundle(userBits = userBits, idBits = idBits)
  val mmio = Output(Bool())
}
// meta read
sealed class DCacheStage1(implicit val p: Parameters) extends DCacheModule {
  class SSDCacheStage1IO extends Bundle {
    val in = Flipped(Decoupled(new SimpleBusReqBundle(userBits = userBits, idBits = idBits)))
    val out = Decoupled(new DStage1IO)
    val metaReadBus = CacheMetaArrayReadBus()
    val dataReadBus = Vec(sramNum, CacheDataArrayReadBus())
    val tagReadBus = CacheTagArrayReadBus()
  }
  val io = IO(new SSDCacheStage1IO)

  val new_cmd = LookupTree(io.in.bits.cmd, List(
    SimpleBusCmd.read   -> M_XRD,   //int load             
    SimpleBusCmd.write  -> M_XWR    //int store
  ))

  // read meta array, tag array and data array
  val readBusValid = io.in.fire
  io.metaReadBus.apply(valid = readBusValid, setIdx = getMetaIdx(io.in.bits.addr))
  io.tagReadBus.apply(valid = readBusValid, setIdx = getMetaIdx(io.in.bits.addr))
  for (w <- 0 until sramNum) {
    io.dataReadBus(w).apply(valid = readBusValid && (w.U === getbankIdx(io.in.bits.addr)), setIdx = getDataIdx(io.in.bits.addr))
  }

  //metaArray need to reset before Load
  //s1 is not ready when metaArray is resetting or meta/dataArray is being written
  val dataReadBusReady = VecInit(io.dataReadBus.map(_.req.ready)).asUInt.andR

  val s1NotReady = (!io.metaReadBus.req.ready || !dataReadBusReady || !io.tagReadBus.req.ready) && io.in.valid
  BoringUtils.addSource(s1NotReady,"s1NotReady")

  io.out.bits.req := io.in.bits
  io.out.bits.req.cmd := new_cmd
  io.out.valid := io.in.valid && io.metaReadBus.req.ready && dataReadBusReady && io.tagReadBus.req.ready
  io.in.ready := io.out.ready && io.metaReadBus.req.ready && dataReadBusReady && io.tagReadBus.req.ready
  io.out.bits.mmio := AddressSpace.isMMIO(io.in.bits.addr)

  //Debug(io.in.fire, "[Dcache req] Addr: %x  Cmd: %x  Wdata: %x\n", io.in.bits.addr, io.in.bits.cmd, io.in.bits.wdata)
}


// check
sealed class DCacheStage2(edge: TLEdgeOut)(implicit val p: Parameters) extends DCacheModule {

  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new DStage1IO))
    val out = Decoupled(new SimpleBusRespBundle(userBits = userBits, idBits = idBits))
    val flush = Input(Bool())
    val metaReadResp = Flipped(Vec(Ways, new DMetaBundle))
    val tagReadResp = Flipped(Vec(Ways, new DTagBundle))
    val dataReadResp = Flipped(Vec(sramNum, Vec(Ways, new DDataBundle)))

    //val dataReadBus = Vec(sramNum, CacheDataArrayReadBus())
    val metaWriteBus = CacheMetaArrayWriteBus()
    val dataWriteBus = Vec(sramNum, CacheDataArrayWriteBus())
    val tagWriteBus = CacheTagArrayWriteBus()

    val mem_getPutAcquire = DecoupledIO(new TLBundleA(edge.bundle))
    val mem_grantReleaseAck = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))
    val mem_finish = DecoupledIO(new TLBundleE(edge.bundle))
    
    val probeVictim = Flipped(new ProbeVictimIO)    
  })
  
  //hit miss check
  val metaWay = io.metaReadResp
  val tagWay = io.tagReadResp
  val req = io.in.bits.req
  val addr = req.addr.asTypeOf(addrBundle)
  val hitVec = VecInit((tagWay zip metaWay).map{case (t, m) => (m.coh.asTypeOf(new ClientMetadata).isValid() && (t.tag === addr.tag))}).asUInt
    //hit tag and meta not nothing
  val hitTag = hitVec.orR && io.in.valid      
    //has hit tag: find its coh
  val coh = Mux(hitTag, Mux1H(hitVec, metaWay).coh.asTypeOf(new ClientMetadata), ClientMetadata.onReset)
  val hitMeta = coh.onAccess(req.cmd)._1
  val hit = hitTag && hitMeta && io.in.valid
    //miss need acquire and release(if not hitTag)
  val miss = !hit && io.in.valid

    //find invalid
  val invalidVec = VecInit(metaWay.map(m => m.coh === ClientStates.Nothing)).asUInt
  val hasInvalidWay = invalidVec.orR
    //give preference to high position of invalid way
  val refillInvalidWaymask = Mux(invalidVec >= 8.U, "b1000".U,
    Mux(invalidVec >= 4.U, "b0100".U,
    Mux(invalidVec >= 2.U, "b0010".U, "b0001".U)))

  val victimIndex = io.probeVictim.index
  val victimWay = io.probeVictim.victimWay
  val victimWayValid = io.probeVictim.valid
  val victimWayReg = RegInit(0.U(Ways.W))
  when (victimWayValid && (victimIndex === addr.index)) {
    victimWayReg := victimWay
  }

  val waymask = Mux(hit || (miss && hitTag), hitVec, Mux(hasInvalidWay, refillInvalidWaymask, victimWayReg))
  val wordMask = Mux(req.isWrite(), MaskExpand(req.wmask), 0.U(DataBits.W))
  
  //if hit: 看看是否需要更新元数据，更新元数据或者与DataArray交互数据，更新LRU表
  val hitNewCoh = coh.onAccess(req.cmd)._3
  val needUpdateMeta = coh =/= hitNewCoh
  val hitWrite = hit && req.isWrite()
  val hitRead = hit && req.isRead()
  
    //update meta
  val metaHitWriteBus = Wire(CacheMetaArrayWriteBus()).apply(
    valid = hit && needUpdateMeta, setIdx = getMetaIdx(req.addr), waymask = waymask,
    data = Wire(new DMetaBundle).apply(coh = hitNewCoh)
  )
   
    //cmd write: write data to cache
  val bankHitVec = BankHitVec(req.addr)
  val hitBank = Mux1H(bankHitVec, io.dataReadResp)
  val dataRead = Mux1H(waymask, hitBank).data
  val dataMasked = MaskData(dataRead, req.wdata, wordMask)
  val dataHitWriteBus = Wire(Vec(sramNum, CacheDataArrayWriteBus()))
  for (w <- 0 until sramNum) {
    dataHitWriteBus(w).apply(
      data = Wire(new DDataBundle).apply(dataMasked),
      valid = hitWrite && w.U === addr.bankIndex, setIdx = Cat(addr.index, addr.wordIndex), waymask = waymask)
  }
  
  //if miss

  //mmio | miss
    //core modules: acquireAccess
  val acquireAccess = Module(new AcquireAccess(edge))
  acquireAccess.io.mem_getPutAcquire <> io.mem_getPutAcquire
  acquireAccess.io.mem_grantAck <> io.mem_grantReleaseAck
  acquireAccess.io.mem_finish <> io.mem_finish
  acquireAccess.io.req.bits := req
  acquireAccess.io.req.valid := miss || (io.in.bits.mmio && io.in.valid)
  acquireAccess.io.req.bits.wdata := Mux(hitTag, dataMasked, io.in.bits.req.wdata)
  acquireAccess.io.isMMIO := io.in.bits.mmio
  acquireAccess.io.waymask := waymask
  acquireAccess.io.hitTag := hitTag
  acquireAccess.io.cohOld := coh
  acquireAccess.io.resp.ready := io.out.ready
  
  val metaWriteArb = Module(new Arbiter(CacheMetaArrayWriteBus().req.bits, 2))
  val dataWriteArb = Seq.fill(sramNum)(Module(new Arbiter(CacheDataArrayWriteBus().req.bits, 2)))

  for (w <- 0 until sramNum) {
    dataWriteArb(w).io.in(0) <> dataHitWriteBus(w).req
    dataWriteArb(w).io.in(1) <> acquireAccess.io.dataWriteBus(w).req
  }
  
  for (w <- 0 until sramNum) {
    io.dataWriteBus(w).req <> dataWriteArb(w).io.out
  }

  metaWriteArb.io.in(0) <> metaHitWriteBus.req
  metaWriteArb.io.in(1) <> acquireAccess.io.metaWriteBus.req
  io.metaWriteBus.req <> metaWriteArb.io.out

  io.tagWriteBus.req <> acquireAccess.io.tagWriteBus.req

  val isGrant = io.mem_grantReleaseAck.bits.opcode === TLMessages.Grant || io.mem_grantReleaseAck.bits.opcode === TLMessages.GrantData
  //val isRelAck = io.mem_grantReleaseAck.bits.opcode === TLMessages.ReleaseAck
  //io.mem_grantReleaseAck.ready := Mux(isGrant, acquireAccess.io.mem_grantAck.ready, Mux(isRelAck, release.io.mem_releaseAck.ready, false.B))
  io.mem_grantReleaseAck.ready := Mux(isGrant, acquireAccess.io.mem_grantAck.ready, false.B)

  io.out <> acquireAccess.io.resp
  //io.out.valid := io.in.valid && (hit || (miss && acquireAccess.io.resp.valid && relOK)) 
  io.out.valid := io.in.valid && (hit || (miss && acquireAccess.io.resp.valid)) 
  io.out.bits.rdata := Mux(hit, dataRead, acquireAccess.io.resp.bits.rdata)
  
  val acquireReady = Mux(miss, acquireAccess.io.req.ready, true.B)
  //val releaseReady = Mux(needRel, release.io.req.ready, true.B)
  //io.in.ready := io.out.ready && acquireReady && releaseReady && !miss
  io.in.ready := io.out.ready && acquireReady && !miss

  val cacheStall = WireInit(false.B)
  val s1NotReady = WireInit(false.B)
  BoringUtils.addSink(s1NotReady,"s1NotReady")
  //cacheStall := miss || s1NotReady || !acquireReady || !releaseReady
  cacheStall := miss || s1NotReady || !acquireReady
  BoringUtils.addSource(cacheStall,"cacheStall")
}

class DCache()(implicit p: Parameters) extends LazyModule with HasNutCoreParameter with HasDCacheParameters with HasNutCoreParameters{
  
  val clientParameters = TLMasterPortParameters.v1(
    Seq(TLMasterParameters.v1(
      name = "dcache",
      sourceId = IdRange(0, 1 << srcBits),
      supportsProbe = TransferSizes(LineSize)
      //supportsGet = TransferSizes(LineSize),
      //supportsPutFull = TransferSizes(LineSize),
      //supportsPutPartial = TransferSizes(LineSize)
    )),
    requestFields = Seq(),
    echoFields = Seq()
  )

  val clientNode = TLClientNode(Seq(clientParameters))

  lazy val module = new DCacheImp(this)
}

class DCacheImp(outer: DCache) extends LazyModuleImp(outer) with HasDCacheIO with HasNutCoreParameter with HasDCacheParameters with HasNutCoreParameters{ 

  val (bus, edge) = outer.clientNode.out.head
  require(bus.params.dataBits == 256)
  // cache pipeline
  val s1 = Module(new DCacheStage1)
  val s2 = Module(new DCacheStage2(edge))

  //core modules
  val probe = Module(new Probe(edge))

  //meta 
  val tagArray = Module(new MetaSRAMTemplateWithArbiter(nRead = 2, new DTagBundle, set = Sets, way = Ways, shouldReset = true))
  val metaArray = Module(new MetaSRAMTemplateWithArbiter(nRead = 2, new DMetaBundle, set = Sets, way = Ways, shouldReset = true))
  //val dataArray = Module(new DataSRAMTemplateWithArbiter(nRead = 3, new DDataBundle, set = Sets * LineBeats, way = Ways))

  val dataArray = Array.fill(sramNum) {
    Module(new DataSRAMTemplateWithArbiter(
      nRead = 2,
      new DDataBundle,
      set = Sets * LineBeats / sramNum,
      way = Ways
    ))
  }

  if (cacheName == "icache") {
    metaArray.reset := reset.asBool
  }

  s1.io.in <> io.in.req

  s2.io.mem_getPutAcquire <> bus.a 
  //s2.io.mem_release <> bus.c
  s2.io.mem_grantReleaseAck <> bus.d 
  s2.io.mem_finish <> bus.e 

  s2.io.probeVictim <> probe.io.probeVictim
  //DontCare <> bus.b  
  probe.io.mem_probe <> bus.b
  //TLArbiter.lowest(edge, bus.c, probe.io.mem_probeAck, s2.io.mem_release)
  probe.io.mem_probeAck <> bus.c
  
  //val channelCArb = Module(new channelCArb(edge))
  //channelCArb.io.in(0) <> probe.mem_probeAck
  //channelCArb.io.in(1) <> s2.io.mem_release

  PipelineConnect(s1.io.out, s2.io.in, s2.io.out.fire, io.flush)

  io.in.resp <> s2.io.out
  s2.io.flush := io.flush
  //io.mmio <> s2.io.mmio

  metaArray.io.r(1) <> s1.io.metaReadBus
  metaArray.io.r(0) <> probe.io.metaReadBus
  /*for (w <- 0 until sramNum) {
    dataArray(w).io.r(2) <> s1.io.dataReadBus(w)
  }
  for (w <- 0 until sramNum) {
    dataArray(w).io.r(1) <> s2.io.dataReadBus(w)
  }*/
  for (w <- 0 until sramNum) {
    dataArray(w).io.r(1) <> s1.io.dataReadBus(w)
  }
  for (w <- 0 until sramNum) {
    dataArray(w).io.r(0) <> probe.io.dataReadBus(w)
  }

  tagArray.io.r(1) <> s1.io.tagReadBus
  tagArray.io.r(0) <> probe.io.tagReadBus

  val metaWriteArb = Module(new Arbiter(CacheMetaArrayWriteBus().req.bits, 2))
  //val dataWriteArb = Module(new Arbiter(CacheDataArrayWriteBus().req.bits, 2))
  metaWriteArb.io.in(1) <> s2.io.metaWriteBus.req
  metaWriteArb.io.in(0) <> probe.io.metaWriteBus.req
  metaArray.io.w.req <> metaWriteArb.io.out
  //metaArray.io.w <> s2.io.metaWriteBus
  //dataWriteArb.io.in(0) <> probe.io.dataWriteBus
  //dataWriteArb.io.in(1) <> s2.io.dataWriteBus
  //dataArray.io.w <> dataWriteArb.io.out
  //dataArray.io.w <> s2.io.dataWriteBus
  for (w <- 0 until sramNum) {
    dataArray(w).io.w <> s2.io.dataWriteBus(w)
  }
  tagArray.io.w <> s2.io.tagWriteBus

  s2.io.metaReadResp := s1.io.metaReadBus.resp.data
  s2.io.tagReadResp := s1.io.tagReadBus.resp.data
  for (w <- 0 until sramNum) {
    s2.io.dataReadResp(w) := s1.io.dataReadBus(w).resp.data
  }
}