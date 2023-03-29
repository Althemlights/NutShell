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

package XiaoHe.SSDbackend

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import bus.simplebus._
import bus.axi4._
import chisel3.experimental.IO
import com.google.protobuf.Internal.FloatList
import utils._
import top.Settings

import scala.collection.immutable
import XiaoHe._
import XiaoHe.SSDbackend._
import XiaoHe.SSDbackend.fu.StoreBufferEntry
import XiaoHe.SSDfrontend._
case class SSDICacheConfig (
                            ro: Boolean = false,
                            userBits: Int = 0,
                            idBits: Int = 0,

                            totalSize: Int = 16, // Kbytes
                            ways: Int = 4
                          )

sealed trait HasICacheConst {
  implicit val cacheConfig: SSDICacheConfig

  val PAddrBits: Int
  val XLEN: Int
  
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
    val index = UInt(IndexBits.W)     //6
    val wordIndex = UInt(WordIndexBits.W) //3
    val byteOffset = UInt((if (XLEN == 64) 3 else 2).W)
  }

  def CacheMetaArrayReadBus() = new SRAMReadBus(new IMetaBundle, set = Sets, way = Ways)
  def CacheDataArrayReadBus() = new SRAMReadBus(new IDataBundle, set = Sets * LineBeats, way = Ways)
  def CacheMetaArrayWriteBus() = new SRAMWriteBus(new IMetaBundle, set = Sets, way = Ways)
  def CacheDataArrayWriteBus() = new SRAMWriteBus(new IDataBundle, set = Sets * LineBeats, way = Ways)

  def getMetaIdx(addr: UInt) = addr.asTypeOf(addrBundle).index
  def getDataIdx(addr: UInt) = Cat(addr.asTypeOf(addrBundle).index, addr.asTypeOf(addrBundle).wordIndex)

  def isSameWord(a1: UInt, a2: UInt) = ((a1 >> 2) == (a2 >> 2))
  def isSetConflict(a1: UInt, a2: UInt) = (a1.asTypeOf(addrBundle).index === a2.asTypeOf(addrBundle).index)
}

sealed abstract class ICacheBundle(implicit cacheConfig: SSDICacheConfig) extends Bundle with HasNutCoreParameter with HasICacheConst
sealed abstract class ICacheModule(implicit cacheConfig: SSDICacheConfig) extends Module with HasNutCoreParameter with HasICacheConst with HasNutCoreLog

sealed class IMetaBundle(implicit val cacheConfig: SSDICacheConfig) extends ICacheBundle {
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

sealed class IDataBundle(implicit val cacheConfig: SSDICacheConfig) extends ICacheBundle {
  val data = Output(UInt(DataBits.W))

  def apply(data: UInt) = {
    this.data := data
    this
  }
}



class SSDICacheIO(implicit val cacheConfig: SSDICacheConfig) extends Bundle with HasNutCoreParameter with HasICacheConst {
  val in = Flipped(new SimpleBusUC(userBits = userBits, idBits = idBits))
  val flush = Input(Bool())
  val out = new SimpleBusC
  val mmio = new SimpleBusUC
}
trait HasSSDICacheIO {
  implicit val cacheConfig: SSDICacheConfig
  val io = IO(new SSDICacheIO)
}

sealed class SSDIStage1IO(implicit val cacheConfig: SSDICacheConfig) extends ICacheBundle {
  val req = new SimpleBusReqBundle(userBits = userBits, idBits = idBits)
  val mmio = Output(Bool())
}
// meta read
sealed class SSDICacheStage1(implicit val cacheConfig: SSDICacheConfig) extends ICacheModule {
  class SSDICacheStage1IO extends Bundle {
    val in = Flipped(Decoupled(new SimpleBusReqBundle(userBits = userBits, idBits = idBits)))
    val out = Decoupled(new SSDIStage1IO)
    val metaReadBus = CacheMetaArrayReadBus()
    val dataReadBus = CacheDataArrayReadBus()
  }
  val io = IO(new SSDICacheStage1IO)

  // read meta array and data array
  val readBusValid = io.in.fire()
  io.metaReadBus.apply(valid = readBusValid, setIdx = getMetaIdx(io.in.bits.addr))
  io.dataReadBus.apply(valid = readBusValid, setIdx = getDataIdx(io.in.bits.addr))


  io.out.bits.req := io.in.bits
  io.out.valid := io.in.valid && io.metaReadBus.req.ready && io.dataReadBus.req.ready
  io.in.ready := io.out.ready && io.metaReadBus.req.ready && io.dataReadBus.req.ready
  io.out.bits.mmio := AddressSpace.isMMIO(io.in.bits.addr)
}


// check
sealed class SSDICacheStage2(implicit val cacheConfig: SSDICacheConfig) extends ICacheModule {
  class SSDICacheStage2IO extends Bundle {
    val in = Flipped(Decoupled(new SSDIStage1IO))
    val out = Decoupled(new SimpleBusRespBundle(userBits = userBits, idBits = idBits))
    val flush = Input(Bool())
    val metaReadResp = Flipped(Vec(Ways, new IMetaBundle))
    val dataReadResp = Flipped(Vec(Ways, new IDataBundle))

    val dataReadBus = CacheDataArrayReadBus()
    val metaWriteBus = CacheMetaArrayWriteBus()
    val dataWriteBus = CacheDataArrayWriteBus()

    val mem = new SimpleBusUC
    val mmio = new SimpleBusUC
  }

  val io = IO(new SSDICacheStage2IO)

  val metaWriteArb = Module(new Arbiter(CacheMetaArrayWriteBus().req.bits, 2))
  val dataWriteArb = Module(new Arbiter(CacheDataArrayWriteBus().req.bits, 2))

  val metaWay = io.metaReadResp
  val req = io.in.bits.req
  val addr = req.addr.asTypeOf(addrBundle)
  val hitVec = VecInit(metaWay.map(m => m.valid && (m.tag === addr.tag))).asUInt
  val hit = hitVec.orR && io.in.valid
  val miss = !(hitVec.orR) && io.in.valid
  val mmio = io.in.valid && io.in.bits.mmio

  //  val victimWaymask = if (Ways > 1) (1.U << LFSR64()(log2Up(Ways) - 1, 0)) else "b1".U
  val victimWaymask = 8.U //Set 3 as default
  val invalidVec = VecInit(metaWay.map(m => !m.valid)).asUInt
  val hasInvalidWay = invalidVec.orR
  val refillInvalidWaymask = Mux(invalidVec >= 8.U, "b1000".U,
    Mux(invalidVec >= 4.U, "b0100".U,
      Mux(invalidVec >= 2.U, "b0010".U, "b0001".U)))

  val hitReadBurst = hit && req.isReadBurst()
  val storeHit = WireInit(false.B)

  val waymask = Mux(hit, hitVec, Mux(hasInvalidWay, refillInvalidWaymask, victimWaymask.asUInt))
  val meta = Mux1H(waymask, metaWay)

  assert(!(mmio && hit), "MMIO request should not hit in cache")


  val dataRead = Mux1H(waymask, io.dataReadResp).data
  //  dontTouch(dataRead)
  val wordMask = Mux(req.isWrite(), MaskExpand(req.wmask), 0.U(DataBits.W))


  val hitWrite = hit && req.isWrite()


  val dataHitWriteBus = Wire(CacheDataArrayWriteBus()).apply(
    data = Wire(new IDataBundle).apply(MaskData(dataRead, req.wdata, wordMask)),
    valid = hitWrite, setIdx = Cat(addr.index, addr.wordIndex), waymask = waymask)

  val metaHitWriteBus = Wire(CacheMetaArrayWriteBus()).apply(
    valid = hitWrite && !meta.dirty, setIdx = getMetaIdx(req.addr), waymask = waymask,
    data = Wire(new IMetaBundle).apply(tag = meta.tag, valid = true.B, dirty = true.B)
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

  val s2_idle :: s2_dataReadWait :: s2_dataOK :: Nil = Enum(3)
  val state2 = RegInit(s2_idle)

  io.dataReadBus.apply(valid = state === s_memWriteReq && state2 === s2_idle,
    setIdx = Cat(addr.index, writeBeatCnt.value))
  val dataWay = RegEnable(io.dataReadBus.resp.data, state2 === s2_dataReadWait)
  val dataHitWay = Mux1H(waymask, dataWay).data


  switch(state2) {
    is(s2_idle) {
      when(io.dataReadBus.req.fire()) {
        state2 := s2_dataReadWait
      }
    }
    is(s2_dataReadWait) {
      state2 := s2_dataOK
    }
    is(s2_dataOK) {
      when(io.mem.req.fire() || hitReadBurst && io.out.ready) {
        state2 := s2_idle
      }
    }
  }

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

  val addrTag = Mux(state === s_memReadReq, raddr, waddr) === "h80022b40".U
  //  dontTouch(addrTag)

  io.mem.resp.ready := true.B
  io.mem.req.valid := (state === s_memReadReq) || ((state === s_memWriteReq) && (state2 === s2_dataOK))


  val afterFirstRead = RegInit(false.B)
  val readingFirst = !afterFirstRead && io.mem.resp.fire() && (state === s_memReadResp)


  // mmio
  io.mmio.req.bits := req
  io.mmio.resp.ready := true.B
  io.mmio.req.valid := (state === s_mmioReq)
  val outBufferValid = WireInit(false.B)
  val mmioStorePending = WireInit(false.B)
  val outBufferFire = WireInit(false.B)
  //Optimal handling when there is mmio store

  // for inst in flash, the max fetch width is 32bit
  val FlashWidth = 4 // 4 Byte
  val mmioCnt = Counter(8 / FlashWidth)
  val FlashInst = RegInit(0.U(64.W))
  val mmioReqOnce = req.addr(2)
  val mmioCntMax = Mux(mmioReqOnce,0.U,1.U)
    io.mmio.req.bits.addr := Mux(mmio, req.addr + (mmioCnt.value << 2).asUInt, req.addr)
    //    io.mmio.req.bits.size := Mux(mmio, "b10".U, "b11".U)
    io.mmio.req.bits.size := "b10".U

    switch(state) {
      is(s_idle) {
        afterFirstRead := false.B

        when((miss && !storeHit || mmio) && !io.flush || mmioStorePending) {
          //        state := Mux(meta.dirty, s_memWriteReq, s_memReadReq)
          state := Mux(mmioStorePending, Mux(outBufferValid, s_mmioReq, s_mmio_wait), Mux(mmio, s_mmioReq, Mux(meta.dirty, s_memWriteReq, s_memReadReq)))
        }
      }

      is(s_mmioReq) {
        when(io.mmio.req.fire()) {
          state := s_mmioResp

        }
      }
      is(s_mmioResp) {
        when(io.mmio.resp.fire()) {
          state := Mux(mmioCnt.value === mmioCntMax,s_wait_resp, s_mmioReq)
          FlashInst := Cat(io.mmio.resp.bits.rdata(31, 0) ,FlashInst(63, 32))
        }
        when(io.mmio.resp.fire() && !mmioReqOnce) {
          mmioCnt.inc()
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
  //  dontTouch(dataRefill)
  val dataRefillWriteBus = Wire(CacheDataArrayWriteBus).apply(
    valid = (state === s_memReadResp) && io.mem.resp.fire(), setIdx = Cat(addr.index, readBeatCnt.value),
    data = Wire(new IDataBundle).apply(dataRefill), waymask = waymask)

  dataWriteArb.io.in(0) <> dataHitWriteBus.req
  dataWriteArb.io.in(1) <> dataRefillWriteBus.req
  io.dataWriteBus.req <> dataWriteArb.io.out

  val metaRefillWriteBus = Wire(CacheMetaArrayWriteBus()).apply(
    valid = (state === s_memReadResp) && io.mem.resp.fire() && io.mem.resp.bits.isReadLast(),
    data = Wire(new IMetaBundle).apply(valid = true.B, tag = addr.tag, dirty = req.isWrite()),
    setIdx = getMetaIdx(req.addr), waymask = waymask)

  val writeDirtyTag = (state === s_memWriteReq) && io.mem.req.bits.addr.asUInt >= "h80022b40".U && io.mem.req.bits.addr.asUInt < "h80022b80".U
  //  dontTouch(writeDirtyTag)

  metaWriteArb.io.in(0) <> metaHitWriteBus.req
  metaWriteArb.io.in(1) <> metaRefillWriteBus.req
  io.metaWriteBus.req <> metaWriteArb.io.out

  io.out.bits.user.zip(req.user).map { case (o, i) => o := i }
  io.out.bits.id.zip(req.id).map { case (o, i) => o := i }

  //out is valid when cacheline is refilled
  io.out.valid := io.in.valid && !needFlush && Mux(hit || storeHit, true.B, state === s_wait_resp)
  val inRdataRegDemand = RegEnable(Mux(mmio, io.mmio.resp.bits.rdata, io.mem.resp.bits.rdata),
    Mux(mmio, state === s_mmioResp, readingFirst))
  io.out.bits.rdata := Mux(hit, dataRead, inRdataRegDemand)

    val memDataLatch = RegEnable(io.mem.resp.bits.rdata, readingFirst)
    val mmioRdataLatch = Mux(mmioReqOnce,Cat(FlashInst(63,32),0.U(32.W)),FlashInst)
    val icacheDataLatch = Mux(mmio,mmioRdataLatch,memDataLatch)
    io.out.bits.rdata := Mux(hit, dataRead, icacheDataLatch)

  io.out.bits.cmd := Mux(io.in.bits.req.isRead(), SimpleBusCmd.readLast, Mux(io.in.bits.req.isWrite(), SimpleBusCmd.writeResp, DontCare))//DontCare, added by lemover

  // With critical-word first, the pipeline registers between
  // s2 and s3 can not be overwritten before a missing request
  // is totally handled. We use io.isFinish to indicate when the
  // request really ends.

  io.in.ready := io.out.ready && state === s_idle && !miss

  //stall when read req in s2 cant be responed or read req in s1 cant be send to s2( s1.in.ready === false.B)
  val cacheStall = WireInit(false.B)
  val s1NotReady = WireInit(false.B)

}

class SSDICache(implicit val cacheConfig: SSDICacheConfig) extends ICacheModule with HasSSDICacheIO {
  // cache pipeline



    val s1 = Module(new SSDICacheStage1)
    val s2 = Module(new SSDICacheStage2)
    val metaArray = Module(new MetaSRAMTemplateWithArbiter(nRead = 1, nWrite = 1, new IMetaBundle, set = Sets, way = Ways, shouldReset = true))
    val dataArray = Module(new ysyxSRAMTemplateWithArbiter(nRead = 2, new IDataBundle, set = Sets * LineBeats, way = Ways))
    val flushICache = WireInit(false.B)
    BoringUtils.addSink(flushICache ,"MOUFlushICache")
    metaArray.reset := reset.asBool || flushICache

    s1.io.in <> io.in.req


    PipelineConnect(s1.io.out, s2.io.in, s2.io.out.fire(), io.flush)

    io.in.resp <> s2.io.out
    s2.io.flush := io.flush
    io.out.mem <> s2.io.mem
    io.out.coh := DontCare
    io.mmio <> s2.io.mmio


    metaArray.io.r(0) <> s1.io.metaReadBus
    dataArray.io.r(0) <> s1.io.dataReadBus
    dataArray.io.r(1) <> s2.io.dataReadBus

    metaArray.io.w(0) <> s2.io.metaWriteBus
    dataArray.io.w <> s2.io.dataWriteBus

    s2.io.metaReadResp := s1.io.metaReadBus.resp.data
    s2.io.dataReadResp := s1.io.dataReadBus.resp.data

    //test tmp
    val dataIndexTag = dataArray.io.w.req.valid && dataArray.io.w.req.bits.setIdx === "h16e".U





}


object SSDICache {
  def apply(in: SimpleBusUC, mmio: SimpleBusUC, flush: Bool)(implicit cacheConfig: SSDICacheConfig) = {
    val cache = Module(new SSDICache)

    cache.io.flush := flush
    cache.io.in <> in
    mmio <> cache.io.mmio
    cache.io.out
  }
}
