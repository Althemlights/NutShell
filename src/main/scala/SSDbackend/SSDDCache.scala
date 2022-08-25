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

case class SSDCacheConfig (
                         ro: Boolean = false,
                         name: String = "cache",
                         userBits: Int = 0,
                         idBits: Int = 0,

                         totalSize: Int = 32, // Kbytes
                         ways: Int = 4
                       )

sealed trait HasCacheConst {
  implicit val cacheConfig: SSDCacheConfig

  val PAddrBits: Int
  val XLEN: Int

  val cacheName = cacheConfig.name
  val userBits = cacheConfig.userBits
  val idBits = cacheConfig.idBits

  val TotalSize = cacheConfig.totalSize
  val Ways = cacheConfig.ways
  val LineSize = XLEN // byte
  val LineBeats = LineSize / 8 //DATA WIDTH 64
  val Sets = TotalSize * 1024 / LineSize / Ways
  val OffsetBits = log2Up(LineSize)
  val IndexBits = log2Up(Sets)
  val WordIndexBits = log2Up(LineBeats)
  val TagBits = PAddrBits - OffsetBits - IndexBits

  def addrBundle = new Bundle {
    val tag = UInt(TagBits.W)
    val index = UInt(IndexBits.W)
    val wordIndex = UInt(WordIndexBits.W)
    val byteOffset = UInt((if (XLEN == 64) 3 else 2).W)
  }

  def CacheMetaArrayReadBus() = new SRAMReadBus(new MetaBundle, set = Sets, way = Ways)
  def CacheDataArrayReadBus() = new SRAMReadBus(new DataBundle, set = Sets * LineBeats, way = Ways)
  def CacheMetaArrayWriteBus() = new SRAMWriteBus(new MetaBundle, set = Sets, way = Ways)
  def CacheDataArrayWriteBus() = new SRAMWriteBus(new DataBundle, set = Sets * LineBeats, way = Ways)

  def getMetaIdx(addr: UInt) = addr.asTypeOf(addrBundle).index
  def getDataIdx(addr: UInt) = Cat(addr.asTypeOf(addrBundle).index, addr.asTypeOf(addrBundle).wordIndex)

  def isSameWord(a1: UInt, a2: UInt) = ((a1 >> 2) == (a2 >> 2))
  def isSetConflict(a1: UInt, a2: UInt) = (a1.asTypeOf(addrBundle).index === a2.asTypeOf(addrBundle).index)
}

sealed abstract class CacheBundle(implicit cacheConfig: SSDCacheConfig) extends Bundle with HasNutCoreParameter with HasCacheConst
sealed abstract class CacheModule(implicit cacheConfig: SSDCacheConfig) extends Module with HasNutCoreParameter with HasCacheConst with HasNutCoreLog

sealed class MetaBundle(implicit val cacheConfig: SSDCacheConfig) extends CacheBundle {
  val tag = Output(UInt(TagBits.W))
  val valid = Output(Bool())
  val dirty = Output(Bool())

  def apply(tag: UInt, valid: Bool, dirty: Bool) = {
    this.tag := tag
    this.valid := valid
    this.dirty := dirty
    this
  }
}

sealed class DataBundle(implicit val cacheConfig: SSDCacheConfig) extends CacheBundle {
  val data = Output(UInt(DataBits.W))

  def apply(data: UInt) = {
    this.data := data
    this
  }
}



class SSDCacheIO(implicit val cacheConfig: SSDCacheConfig) extends Bundle with HasNutCoreParameter with HasCacheConst {
  val in = Flipped(new SimpleBusUC(userBits = userBits, idBits = idBits))
  val flush = Input(Bool())
  val out = new SimpleBusC
  //val mmio = new SimpleBusUC
}
trait HasSSDCacheIO {
  implicit val cacheConfig: SSDCacheConfig
  val io = IO(new SSDCacheIO)
}

sealed class SSDStage1IO(implicit val cacheConfig: SSDCacheConfig) extends CacheBundle {
  val req = new SimpleBusReqBundle(userBits = userBits, idBits = idBits)
}
// meta read
sealed class SSDCacheStage1(implicit val cacheConfig: SSDCacheConfig) extends CacheModule {
  class SSDCacheStage1IO extends Bundle {
    val in = Flipped(Decoupled(new SimpleBusReqBundle(userBits = userBits, idBits = idBits)))
    val out = Decoupled(new SSDStage1IO)
    val metaReadBus = CacheMetaArrayReadBus()
    val dataReadBus = CacheDataArrayReadBus()
  }
  val io = IO(new SSDCacheStage1IO)

  // read meta array and data array
  val readBusValid = io.in.fire()
  io.metaReadBus.apply(valid = readBusValid, setIdx = getMetaIdx(io.in.bits.addr))
  io.dataReadBus.apply(valid = readBusValid, setIdx = getDataIdx(io.in.bits.addr))

  //metaArray need to reset before Load
  //s1 is not ready when metaArray is resetting or meta/dataArray is being written

  if(cacheName == "dcache") {
    val s1NotReady = (!io.metaReadBus.req.ready || !io.dataReadBus.req.ready)&& io.in.valid
    BoringUtils.addSource(s1NotReady,"s1NotReady")
  }

  io.out.bits.req := io.in.bits
  io.out.valid := io.in.valid && io.metaReadBus.req.ready && io.dataReadBus.req.ready
  io.in.ready := io.out.ready && io.metaReadBus.req.ready && io.dataReadBus.req.ready
}


// check
sealed class SSDCacheStage2(implicit val cacheConfig: SSDCacheConfig) extends CacheModule {
  class SSDCacheStage2IO extends Bundle {
    val in = Flipped(Decoupled(new SSDStage1IO))
    val out = Decoupled(new SimpleBusRespBundle(userBits = userBits, idBits = idBits))
    val flush = Input(Bool())
    val metaReadResp = Flipped(Vec(Ways, new MetaBundle))
    val dataReadResp = Flipped(Vec(Ways, new DataBundle))
    val metaWriteBus = CacheMetaArrayWriteBus()
    val dataWriteBus = CacheDataArrayWriteBus()

    val mem = new SimpleBusUC
    //val mmio = new SimpleBusUC
  }

  val io = IO(new SSDCacheStage2IO)

  val metaWriteArb = Module(new Arbiter(CacheMetaArrayWriteBus().req.bits, 2))
  val dataWriteArb = Module(new Arbiter(CacheDataArrayWriteBus().req.bits, 2))

  val metaWay = io.metaReadResp
  val dataWay = io.dataReadResp
  val req = io.in.bits.req
  val addr = req.addr.asTypeOf(addrBundle)
  val hitVec = VecInit(metaWay.map(m => m.valid && (m.tag === addr.tag))).asUInt
  val hit = hitVec.orR && io.in.valid
  val miss = !(hitVec.orR) && io.in.valid
  val mmio = AddressSpace.isMMIO(req.addr)
  val storeHit = WireInit(false.B)
  if (cacheName == "dcache") {
    BoringUtils.addSink(storeHit,"storeHit")
  }


  val victimWaymask = if (Ways > 1) (1.U << LFSR64()(log2Up(Ways) - 1, 0)) else "b1".U
  val invalidVec = VecInit(metaWay.map(m => !m.valid)).asUInt
  val hasInvalidWay = invalidVec.orR
  val refillInvalidWaymask = Mux(invalidVec >= 8.U, "b1000".U,
    Mux(invalidVec >= 4.U, "b0100".U,
      Mux(invalidVec >= 2.U, "b0010".U, "b0001".U)))

  val hitReadBurst = hit && req.isReadBurst()

  val waymask = Mux(hit, hitVec, Mux(hasInvalidWay, refillInvalidWaymask, victimWaymask.asUInt))
  val meta = Mux1H(waymask, metaWay)

  assert(!(mmio && hit), "MMIO request should not hit in cache")


  val dataRead = Mux1H(waymask, io.dataReadResp).data
  val wordMask = Mux(req.isWrite(), MaskExpand(req.wmask), 0.U(DataBits.W))


  val hitWrite = hit && req.isWrite()

  val dataWriteTag = Cat(addr.index,  addr.wordIndex) === "h1fb".U
  dontTouch(dataWriteTag)

  val dataHitWriteBus = Wire(CacheDataArrayWriteBus()).apply(
    data = Wire(new DataBundle).apply(MaskData(dataRead, req.wdata, wordMask)),
    valid = hitWrite, setIdx = Cat(addr.index,  addr.wordIndex), waymask = waymask)

  val metaHitWriteBus = Wire(CacheMetaArrayWriteBus()).apply(
    valid = hitWrite && !meta.dirty, setIdx = getMetaIdx(req.addr), waymask = waymask,
    data = Wire(new MetaBundle).apply(tag = meta.tag, valid = true.B, dirty = true.B)
  )

  val s_idle :: s_memReadReq :: s_memReadResp :: s_memWriteReq :: s_memWriteResp :: s_mmio_wait :: s_mmioReq :: s_mmioResp :: s_wait_resp :: s_release :: Nil = Enum(10)
  val state = RegInit(s_idle)
  val needFlush = RegInit(false.B)


  when(io.flush && (state =/= s_idle) && !io.out.valid) {
    needFlush := true.B
  }
  when(state === s_wait_resp && needFlush) {
    needFlush := false.B
  }

  val readBeatCnt = Counter(LineBeats)
  val writeBeatCnt = Counter(LineBeats)

  val dataHitWay = Mux1H(waymask, dataWay).data

  // critical word first read
  val raddr = (if (XLEN == 64) Cat(req.addr(PAddrBits - 1, 3), 0.U(3.W))
  else Cat(req.addr(PAddrBits - 1, 2), 0.U(2.W)))
  // dirty block addr
  val waddr = Cat(meta.tag, addr.index, 0.U(OffsetBits.W))
  val cmd = Mux(state === s_memReadReq, SimpleBusCmd.readBurst,
    Mux((writeBeatCnt.value === (LineBeats - 1).U), SimpleBusCmd.writeLast, SimpleBusCmd.writeBurst))
  io.mem.req.bits.apply(addr = Mux(state === s_memReadReq, raddr, waddr),
    cmd = cmd, size = (if (XLEN == 64) "b11".U else "b10".U),
    wdata = dataHitWay, wmask = Fill(DataBytes, 1.U))

  io.mem.resp.ready := true.B
  io.mem.req.valid := (state === s_memReadReq) || ((state === s_memWriteReq) )


  val afterFirstRead = RegInit(false.B)
  val readingFirst = !afterFirstRead && io.mem.resp.fire() && (state === s_memReadResp)
  val inRdataRegDemand = RegEnable(io.mem.resp.bits.rdata, readingFirst)


  switch(state) {
    is(s_idle) {
      afterFirstRead := false.B

      when(miss && !io.flush && !storeHit) {
        state := Mux(meta.dirty, s_memWriteReq, s_memReadReq)
      }
    }

    is(s_memReadReq) {
      when(io.mem.req.fire()) {
        state := s_memReadResp
        readBeatCnt.value := addr.wordIndex
      }
    }

    is(s_memReadResp) {
      when(io.mem.resp.fire()) {
        afterFirstRead := true.B
        readBeatCnt.inc()
        when(io.mem.resp.bits.isReadLast()) {
          state := s_wait_resp
        }
      }
    }

    is(s_memWriteReq) {
      when(io.mem.req.fire()) {
        writeBeatCnt.inc()
      }
      when(io.mem.req.bits.isWriteLast() && io.mem.req.fire()) {
        state := s_memWriteResp
      }
    }

    is(s_memWriteResp) {
      when(io.mem.resp.fire()) {
        state := s_memReadReq
      }
    }
    is(s_wait_resp) {
      when(io.out.fire() || needFlush) {
        state := s_idle
      }
    }
  }

  val dataRefill = MaskData(io.mem.resp.bits.rdata, req.wdata, Mux(readingFirst, wordMask, 0.U(DataBits.W)))
  dontTouch(dataRefill)
  val dataRefillWriteBus = Wire(CacheDataArrayWriteBus).apply(
    valid = (state === s_memReadResp) && io.mem.resp.fire(), setIdx = Cat(addr.index, readBeatCnt.value),
    data = Wire(new DataBundle).apply(dataRefill), waymask = waymask)

  dataWriteArb.io.in(0) <> dataHitWriteBus.req
  dataWriteArb.io.in(1) <> dataRefillWriteBus.req
  io.dataWriteBus.req <> dataWriteArb.io.out

  val metaRefillWriteBus = Wire(CacheMetaArrayWriteBus()).apply(
    valid = (state === s_memReadResp) && io.mem.resp.fire() && io.mem.resp.bits.isReadLast(),
    data = Wire(new MetaBundle).apply(valid = true.B, tag = addr.tag, dirty = req.isWrite()),
    setIdx = getMetaIdx(req.addr), waymask = waymask
  )

  metaWriteArb.io.in(0) <> metaHitWriteBus.req
  metaWriteArb.io.in(1) <> metaRefillWriteBus.req
  io.metaWriteBus.req <> metaWriteArb.io.out

  io.out.bits.user.zip(req.user).map { case (o, i) => o := i }
  io.out.bits.id.zip(req.id).map { case (o, i) => o := i }

  //out is valid when cacheline is refilled
  io.out.valid := io.in.valid && !needFlush && Mux(hit || storeHit, true.B, state === s_wait_resp)
  io.out.bits.rdata := Mux(hit,dataRead,inRdataRegDemand)
  io.out.bits.cmd := Mux(io.in.bits.req.isRead(), SimpleBusCmd.readLast, Mux(io.in.bits.req.isWrite(), SimpleBusCmd.writeResp, DontCare))//DontCare, added by lemover

  // With critical-word first, the pipeline registers between
  // s2 and s3 can not be overwritten before a missing request
  // is totally handled. We use io.isFinish to indicate when the
  // request really ends.

  io.in.ready := io.out.ready && state === s_idle && !miss

  //stall when read req in s2 cant be responed or read req in s1 cant be send to s2( s1.in.ready === false.B)
  val cacheStall = WireInit(false.B)
  val s1NotReady = WireInit(false.B)
  if (cacheName == "dcache"){
    BoringUtils.addSource(cacheStall,"cacheStall")
    BoringUtils.addSink(s1NotReady,"s1NotReady")
    cacheStall := miss || state =/= s_idle || s1NotReady}

}
class SSDCache(implicit val cacheConfig: SSDCacheConfig) extends CacheModule with HasSSDCacheIO {
  // cache pipeline
  val s1 = Module(new SSDCacheStage1)
  val s2 = Module(new SSDCacheStage2)

  val metaArray = Module(new SRAMTemplateWithArbiter(nRead = 1, new MetaBundle, set = Sets, way = Ways, shouldReset = true))
  val dataArray = Module(new SRAMTemplateWithArbiter(nRead = 1, new DataBundle, set = Sets * LineBeats, way = Ways))

  if (cacheName == "icache") {
    metaArray.reset := reset.asBool
  }

  s1.io.in <> io.in.req

  
  PipelineConnect(s1.io.out, s2.io.in, s2.io.out.fire(), io.flush)

  io.in.resp <> s2.io.out
  s2.io.flush := io.flush
  io.out.mem <> s2.io.mem
  io.out.coh := DontCare
//  io.mmio <> s3.io.mmio
  

  metaArray.io.r(0) <> s1.io.metaReadBus
  dataArray.io.r(0) <> s1.io.dataReadBus

  metaArray.io.w <> s2.io.metaWriteBus
  dataArray.io.w <> s2.io.dataWriteBus

  s2.io.metaReadResp := s1.io.metaReadBus.resp.data
  s2.io.dataReadResp := s1.io.dataReadBus.resp.data
  
}


object SSDCache {
  def apply(in: SimpleBusUC, flush: Bool)(implicit cacheConfig: SSDCacheConfig) = {
    val cache = Module(new SSDCache)

    cache.io.flush := flush
    cache.io.in <> in
    
    cache.io.out
  }
}
