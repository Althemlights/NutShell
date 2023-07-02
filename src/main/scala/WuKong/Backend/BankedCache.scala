package WuKong.Backend

/** ************************************************************************************
 * Copyright (c) 2020 Institute of Computing Technology, CAS Copyright (c) 2020
 * University of Chinese Academy of Sciences
 *
 * WuKong is licensed under Mulan PSL v2. You can use this software according
 * to the terms and conditions of the Mulan PSL v2. You may obtain a copy of
 * Mulan PSL v2 at: http://license.coscl.org.cn/MulanPSL2
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY
 * KIND, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO
 * NON-INFRINGEMENT, MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
 *
 * See the Mulan PSL v2 for more details.
 */


import chisel3._
import chisel3.util.{Enum, _}
import chisel3.util.experimental.BoringUtils
import bus.simplebus._
import bus.axi4._
import chisel3.experimental.IO
import utils._
import top.Settings

import scala.collection.immutable
import WuKong._
import WuKong.Backend._
import WuKong.Backend.fu.StoreBufferEntry
import WuKong.Frontend._
import utils.PipelineVector2Connect
import WuKong.{HasCoreParameter, HasCoreLog, AddressSpace}

case class BankedCacheConfig(
                           ro: Boolean = false,
                           userBits: Int = 0,
                           idBits: Int = 0,
                           totalSize: Int = 16, // Kbytes
                           ways: Int = 4,
                           DCacheBanks: Int = 8
                         )

sealed trait BankedHasCacheConst {
  implicit val cacheConfig: BankedCacheConfig

  val PAddrBits: Int
  val XLEN: Int

  val userBits = cacheConfig.userBits
  val idBits = cacheConfig.idBits

  val TotalSize = cacheConfig.totalSize
  val Ways = cacheConfig.ways
  val LineSize = XLEN // byte
  val LineBeats = LineSize / 8 // DATA WIDTH 64
  val Sets = TotalSize * 1024 / LineSize / Ways
  val OffsetBits = log2Up(LineSize)
  val IndexBits = log2Up(Sets)
  val WordIndexBits = log2Up(LineBeats)
  val TagBits = PAddrBits - OffsetBits - IndexBits
  val DCacheWays = cacheConfig.ways
  val DCacheBanks = cacheConfig.DCacheBanks
  val LoadPipelineWidth = 2
  val DCacheSRAMRowBits = XLEN
  val DCacheSets = TotalSize * 1024 / LineSize / Ways
  val nWays = cacheConfig.ways
  val DCacheSetOffset = log2Up(XLEN)
  val DCacheBankOffset = log2Up(LineBeats)
  val DCacheAboveIndexOffset = DCacheSetOffset + log2Up(DCacheSets)
  val DCacheSRAMRowBytes = DCacheSRAMRowBits/8
  val DCacheBanksBits = log2Up(DCacheBanks)

  def addrBundle = new Bundle {
    val tag = UInt(TagBits.W)
    val index = UInt(IndexBits.W) // 6
    val bankIndex = UInt(DCacheBanksBits.W) // 3
    val byteOffset = UInt((if (XLEN == 64) 3 else 2).W)
  }

  def CacheMetaArrayReadBus() =
    new BankedSRAMReadBus(new BankedMetaBundle, set = Sets, way = Ways)
  def CacheDataArrayReadBus() =
    new BankedSRAMReadBus(new BankedDataBundle, set = Sets * LineBeats, way = Ways)
  def CacheMetaArrayWriteBus() =
    new BankedSRAMWriteBus(new BankedMetaBundle, set = Sets, way = Ways)
  def CacheDataArrayWriteBus() =
    new BankedSRAMWriteBus(new BankedDataBundle, set = Sets * LineBeats, way = Ways)

  def getMetaIdx(addr: UInt) = addr.asTypeOf(addrBundle).index
  def getDataIdx(addr: UInt) = Cat(
    addr.asTypeOf(addrBundle).index,
    addr.asTypeOf(addrBundle).bankIndex
  )

  def isSameWord(a1: UInt, a2: UInt) = ((a1 >> 2) == (a2 >> 2))
  def isSetConflict(a1: UInt, a2: UInt) =
    (a1.asTypeOf(addrBundle).index === a2.asTypeOf(addrBundle).index)

  def isBankConflict(a1: UInt, a2: UInt) =
    (a1.asTypeOf(addrBundle).bankIndex === a2.asTypeOf(addrBundle).bankIndex)
}


abstract class BankedCacheBundle(implicit cacheConfig: BankedCacheConfig)
  extends Bundle
    with HasCoreParameter
    with BankedHasCacheConst
abstract class BankedCacheModule(implicit cacheConfig: BankedCacheConfig)
  extends Module
    with HasCoreParameter
    with BankedHasCacheConst
    with HasCoreLog

sealed class BankedMetaBundle(implicit val cacheConfig: BankedCacheConfig)
  extends BankedCacheBundle {
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

class BankMetaBundle_tmp   (implicit val cacheConfig: BankedCacheConfig) extends BankedCacheBundle
  with BankedHasCacheConst {
  val tag = UInt(TagBits.W)
  val valid = (Bool())
  val dirty = (Bool())

  def apply(tag: UInt, valid: Bool, dirty: Bool) = {
    this.tag := tag
    this.valid := valid
    this.dirty := dirty
    this
  }
}

sealed class BankedDataBundle(implicit val cacheConfig: BankedCacheConfig)
  extends BankedCacheBundle {
  val data = Output(UInt(DataBits.W))

  def apply(data: UInt) = {
    this.data := data
    this
  }
}


class BankedCacheIO(implicit val cacheConfig: BankedCacheConfig)
  extends Bundle
    with HasCoreParameter
    with BankedHasCacheConst {
  val in = Vec(2, Flipped(new SimpleBusUC(userBits = userBits, idBits = idBits)))
  val flush = Input(Bool())
  val out = new SimpleBusC
  val mmio = new SimpleBusUC
}
trait HasBankedCacheIO {
  implicit val cacheConfig: BankedCacheConfig
  val io = IO(new BankedCacheIO)
}

sealed class BankedStage1Out(implicit val cacheConfig: BankedCacheConfig)
  extends BankedCacheBundle {
  val req = new SimpleBusReqBundle(userBits = userBits, idBits = idBits)
  val bank_conflict = Output(Bool())
  val mmio = Output(Bool())
}
// meta read
class BankedCacheStage1(implicit val cacheConfig: BankedCacheConfig)
  extends BankedCacheModule {
  class BankedCacheStage1IO extends Bundle {
    val in = Flipped(
      Vec(2, Decoupled(
        new SimpleBusReqBundle(userBits = userBits, idBits = idBits)
      ))
    )
    val out = Vec(2, Decoupled(new BankedStage1Out))
    val release_later = Input(Bool())
    val metaReadBus = Vec(2, CacheMetaArrayReadBus())
    val dataReadBus = Vec(2, (CacheDataArrayReadBus()))
  }
  val io = IO(new BankedCacheStage1IO)
  //stateBusy from stage2,represent state in stage2 is busy and not ready.
  val stateBusy = WireInit(false.B)
  BoringUtils.addSink(stateBusy, "stateBusy")

  val req_addr0 = io.in(0).bits.addr
  val req_addr1 = io.in(1).bits.addr
  val req_valid0 = io.in(0).valid
  val req_valid1 = io.in(1).valid
  val same_bank = isBankConflict(req_addr0, req_addr1)
  //when channel0 in stage2 is finish,allow channel0 and channel1 flow.
  val release_later = WireInit(false.B)
  release_later := io.release_later

  val real_bank_conflict = same_bank && req_valid0 && req_valid1

  //2load situation from stage2
  val process_channel0 = WireInit(false.B)
  val process_channel1 = WireInit(false.B)
  BoringUtils.addSink(process_channel0,"process_channel0")
  BoringUtils.addSink(process_channel1,"process_channel1")

  //represent 2load finish
  val process_channel1_finish = WireInit(false.B)
  BoringUtils.addSink(process_channel1_finish,"process_channel1_finish")

  //readvalid,when bankconflict,allow channel0 to read cache and channel1 is forbidden.
  val readBusValid0 = req_valid0 && (!stateBusy ) && (!process_channel1 || process_channel1_finish) && !release_later
  val readBusValid1 = req_valid1 && (!real_bank_conflict || io.release_later)  && (!stateBusy || io.release_later)
  io.metaReadBus(0).apply(
    valid = readBusValid0,
    setIdx = getMetaIdx(io.in(0).bits.addr)
  )
  io.metaReadBus(1).apply(
    valid = readBusValid1,
    setIdx = getMetaIdx(io.in(1).bits.addr)
  )
  io.dataReadBus(0).apply(
    valid = readBusValid0,
    setIdx = getDataIdx(io.in(0).bits.addr)
  )
  io.dataReadBus(1).apply(
    valid = readBusValid1,
    setIdx = getDataIdx(io.in(1).bits.addr)
  )


  // metaArray need to reset before Load
  // s1 is not ready when metaArray is resetting or meta/dataArray is being written

  val s1NotReady =
    (!io.metaReadBus(0).req.ready || !io.metaReadBus(1).req.ready ||
      !io.dataReadBus(0).req.ready || !io.dataReadBus(1).req.ready) && (io.in(0).valid && io.in(1).valid)
  BoringUtils.addSource(s1NotReady, "s1NotReady")

  io.out(0).bits.bank_conflict := same_bank && req_valid0 && req_valid1 && !io.release_later
  io.out(1).bits.bank_conflict := same_bank && req_valid0 && req_valid1 


  io.out(0).bits.req := io.in(0).bits
  io.out(1).bits.req := io.in(1).bits
  //when bank is conflicting,channel0 is allowed to flow down the pipe and channel1 is forbidden.
  //But when release_later is high, also allow channel0 flow down the pipe.
  //because the bank_conflict signal is used in stage2.
  io.out(0).valid := io.in(0).valid && io.metaReadBus(0).req.ready && io.dataReadBus(0).req.ready 
  io.out(1).valid := io.in(1).valid && io.metaReadBus(1).req.ready && io.dataReadBus(1).req.ready && (!real_bank_conflict || io.release_later) 
  io.in(0).ready := io.out(0).ready && io.metaReadBus(0).req.ready && io.dataReadBus(0).req.ready
  io.in(1).ready := io.out(1).ready && io.metaReadBus(1).req.ready && io.dataReadBus(1).req.ready
  // io.in(0).ready := io.out(0).ready
  // io.in(1).ready := io.out(1).ready
  io.out(0).bits.mmio := AddressSpace.isMMIO(io.in(0).bits.addr)
  io.out(1).bits.mmio := AddressSpace.isMMIO(io.in(1).bits.addr)

  //when bankcoflict and not relese,stall core.
  BoringUtils.addSource(same_bank && req_valid0 && req_valid1 && !io.release_later,"real_bank_conflict")
}

// check
sealed class BankedCacheStage2(implicit val cacheConfig: BankedCacheConfig)
  extends BankedCacheModule {
  class BankedCacheStage2IO extends Bundle {
    val in = Flipped(Vec(2, Decoupled(new BankedStage1Out)))
    val out = Vec(2, Decoupled(
      new SimpleBusRespBundle(userBits = userBits, idBits = idBits)
    ))
    val flush = Input(Bool())
    val metaReadResp = Vec(2, Flipped(Vec(Ways, new BankedMetaBundle)))
    val dataReadResp = Vec(2, Flipped(Vec(Ways, new BankedDataBundle)))

    val dataReadBus = Vec(2, CacheDataArrayReadBus())
    val metaWriteBus = CacheMetaArrayWriteBus()
    val dataWriteBus = CacheDataArrayWriteBus()

    val mem = new SimpleBusUC
    val mmio = new SimpleBusUC
    //later release
    val release_later = Output(Bool())
  }
  val process_channel0 = WireInit(false.B)
  val process_channel1 = WireInit(false.B)
  BoringUtils.addSink(process_channel0, "process_channel0")
  BoringUtils.addSink(process_channel1, "process_channel1")
  val io = IO(new BankedCacheStage2IO)

  val metaWriteArb = Module(new Arbiter(CacheMetaArrayWriteBus().req.bits, 2))
  val dataWriteArb = Module(new Arbiter(CacheDataArrayWriteBus().req.bits, 2))

  val victimWaymask = 8.U // Set 3 as default
  val metaWay = Wire(Vec(2, Vec(Ways, new BankMetaBundle_tmp)))
  val req     = Wire(Vec(2, new SimpleBusReqBundle(userBits = userBits, idBits = idBits)))
  val addr    = Wire(Vec(2, addrBundle))
  val hitVec  = Wire(Vec(2, UInt(4.W)))
  val hit     = Wire(Vec(2, Bool()))
  val miss    = Wire(Vec(2, Bool()))
  dontTouch(miss)

  val mmio          = Wire(Vec(2, Bool()))
  val invalidVec    = Wire(Vec(2, UInt(4.W)))
  val hasInvalidWay = Wire(Vec(2, Bool()))

  val refillInvalidWaymask = Wire(Vec(2, UInt(4.W)))

  val hitReadBurst = Wire(Vec(2, Bool()))
  val waymask = Wire(Vec(2, UInt(4.W)))

  val meta = Wire(Vec(2, new BankedMetaBundle))
  val dataRead = Wire(Vec(2, UInt(64.W)))
  val wordMask = Wire(Vec(2, UInt(64.W)))
  for(i <- 0 until 2){
    metaWay(i) := io.metaReadResp(i)
    req(i)     := io.in(i).bits.req
    addr(i)    := req(i).addr.asTypeOf(addrBundle)
    hitVec(i)  := VecInit(
      metaWay(i).map(m => m.valid && (m.tag === addr(i).tag))
    ).asUInt
    hit(i)     := hitVec(i).orR && io.in(i).valid
    miss(i)    := !(hitVec(i).orR) && io.in(i).valid 
    mmio(i)    := io.in(i).valid && io.in(i).bits.mmio

    invalidVec(i) := VecInit(metaWay(i).map(m => !m.valid)).asUInt
    hasInvalidWay(i) := invalidVec(i).orR
    refillInvalidWaymask(i) := Mux(
      invalidVec(i) >= 8.U,
      "b1000".U,
      Mux(
        invalidVec(i) >= 4.U,
        "b0100".U,
        Mux(invalidVec(i) >= 2.U, "b0010".U, "b0001".U)
      )
    )

    hitReadBurst(i) := hit(i) && req(i).isReadBurst()

    waymask(i) := Mux(
      hit(i),
      hitVec(i),
      Mux(hasInvalidWay(i), refillInvalidWaymask(i), victimWaymask.asUInt)
    )
    meta(i) := Mux1H(waymask(i), metaWay(i))

//    assert(!(mmio && hit), "MMIO request should not hit in cache")
    dataRead(i) := Mux1H(waymask(i), io.dataReadResp(i)).data
    //  dontTouch(dataRead)
    wordMask(i) := Mux(req(i).isWrite(), MaskExpand(req(i).wmask), 0.U(DataBits.W))

  }


  val hitWrite = hit(0) && req(0).isWrite()

  val dataHitWriteBus = Wire(CacheDataArrayWriteBus()).apply(
    data =
      Wire(new BankedDataBundle).apply(MaskData(dataRead(0), req(0).wdata, wordMask(0))),
    valid = hitWrite,
    setIdx = Cat(addr(0).index, addr(0).bankIndex),
    waymask = waymask(0)
  )

  val metaHitWriteBus = Wire(CacheMetaArrayWriteBus()).apply(
    valid = hitWrite && !meta(0).dirty,
    setIdx = getMetaIdx(req(0).addr),
    waymask = waymask(0),
    data = Wire(new BankedMetaBundle)
      .apply(tag = meta(0).tag, valid = true.B, dirty = true.B)
  )

  //    0           1               2                 3                 4               5             6           7                 8           9
  val s_idle :: s_memReadReq :: s_memReadResp :: s_memWriteReq :: s_memWriteResp :: s_mmio_wait :: s_mmioReq :: s_mmioResp :: s_wait_resp :: s_release :: Nil =
    Enum(10)
  val state = RegInit(s_idle)
  val storeHit = WireInit(false.B)
  BoringUtils.addSink(storeHit, "storeHit")



  val readBeatCnt = Counter(LineBeats)
  val writeBeatCnt = Counter(LineBeats)

  val s2_idle :: s2_dataReadWait :: s2_dataOK :: Nil = Enum(3)
  val state2 = RegInit(s2_idle)

  io.dataReadBus(0).apply(
    valid = state === s_memWriteReq && state2 === s2_idle,
    setIdx = Cat(Mux(miss(0),addr(0).index,addr(1).index), writeBeatCnt.value)
  )

  io.dataReadBus(1).apply(
    valid = false.B,
    setIdx = 0.U
  )

  val dataWay =
    RegEnable(io.dataReadBus(0).resp.data, state2 === s2_dataReadWait)
  val dataHitWay = Mux1H(waymask(0), dataWay).data

  switch(state2) {
    is(s2_idle) {
      when(io.dataReadBus(0).req.fire()) {
        state2 := s2_dataReadWait
      }
    }
    is(s2_dataReadWait) {
      state2 := s2_dataOK
    }
    is(s2_dataOK) {
      when(io.mem.req.fire() || hitReadBurst(0) && io.out(0).ready) {
        state2 := s2_idle
      }
    }
  }
  val bank_conflict = io.in(0).bits.bank_conflict
        //double miss logic
  val both_miss = miss(0) && miss(1)
  val later_miss_bank = addr(1).bankIndex
  val both_miss_refill_at_channel1 = RegInit(false.B)
  when(both_miss && state === s_wait_resp && !both_miss_refill_at_channel1){
    both_miss_refill_at_channel1 := true.B
  }.elsewhen(both_miss_refill_at_channel1 && state === s_wait_resp){
    both_miss_refill_at_channel1 := false.B
  }
  // critical word first read
  val raddr = Mux(both_miss,Mux(both_miss_refill_at_channel1,Cat(req(1).addr(PAddrBits - 1, 3), 0.U(3.W)),Cat(req(0).addr(PAddrBits - 1, 3), 0.U(3.W))),
  Mux(miss(0),
    Cat(req(0).addr(PAddrBits - 1, 3), 0.U(3.W)),
    Cat(req(1).addr(PAddrBits - 1, 3), 0.U(3.W))))

  // dirty block addr
  val waddr = Mux(both_miss,
    Mux(both_miss_refill_at_channel1,
      Cat(meta(1).tag, addr(1).index, 0.U(OffsetBits.W)),
      Cat(meta(0).tag, addr(0).index, 0.U(OffsetBits.W))),
    Mux(miss(0),
      Cat(meta(0).tag, addr(0).index, 0.U(OffsetBits.W)),
      Cat(meta(1).tag, addr(1).index, 0.U(OffsetBits.W))))
  val cmd = Mux(
    state === s_memReadReq,
    SimpleBusCmd.readBurst,
    Mux(
      (writeBeatCnt.value === (LineBeats - 1).U),
      SimpleBusCmd.writeLast,
      SimpleBusCmd.writeBurst
    )
  )
  io.mem.req.bits.apply(
    addr = Mux(state === s_memReadReq, raddr, waddr),
    cmd = cmd,
    size = (if (XLEN == 64) "b11".U else "b10".U),
    wdata = dataHitWay,
    wmask = Fill(DataBytes, 1.U)
  )

  val addrTag = Mux(state === s_memReadReq, raddr, waddr) === "h80022b40".U
  //  dontTouch(addrTag)

  io.mem.resp.ready := true.B
  io.mem.req.valid := (state === s_memReadReq) || ((state === s_memWriteReq) && (state2 === s2_dataOK))

  val afterFirstRead = RegInit(false.B)
  val readingFirst =
    !afterFirstRead && io.mem.resp.fire() && (state === s_memReadResp)

  // mmio
  io.mmio.req.bits := req(0)
  io.mmio.resp.ready := true.B
  io.mmio.req.valid := (state === s_mmioReq)
  val outBufferValid = WireInit(false.B)
  val mmioStorePending = WireInit(false.B)
  val outBufferFire = WireInit(false.B)
  // Optimal handling when there is mmio store

  val MMIOStorePkt = Wire(Flipped(Decoupled(new StoreBufferEntry)))
  MMIOStorePkt.valid := false.B
  MMIOStorePkt.bits := 0.U.asTypeOf(new StoreBufferEntry)
     BoringUtils.addSink(mmioStorePending, "MMIOStorePending")
     BoringUtils.addSink(outBufferValid, "MMIOStorePktValid")
     BoringUtils.addSink(MMIOStorePkt.bits, "MMIOStorePktBits")
     BoringUtils.addSource(MMIOStorePkt.ready, "MMIOStorePktReady")
     BoringUtils.addSink(outBufferFire, "outBufferFire")
  MMIOStorePkt.valid := outBufferValid && (state === s_mmioReq)
  val mmioStoreReq = Wire(
    Flipped(
      Decoupled(new SimpleBusReqBundle(userBits = userBits, idBits = idBits))
    )
  )
  val mmioStoreReqLatch = RegEnable(mmioStoreReq.bits, outBufferValid)
  mmioStoreReq.ready := true.B
  mmioStoreReq.valid := (state === s_mmioReq)
  mmioStoreReq.bits.cmd := SimpleBusCmd.write
  mmioStoreReq.bits.addr := MMIOStorePkt.bits.paddr
  mmioStoreReq.bits.wdata := MMIOStorePkt.bits.data
  mmioStoreReq.bits.size := MMIOStorePkt.bits.size
  mmioStoreReq.bits.wmask := MMIOStorePkt.bits.mask

  MMIOStorePkt.ready := io.mmio.req.fire() && (state === s_mmioReq)

  io.mmio.req.bits := Mux(mmioStorePending, mmioStoreReqLatch, req(0))

  // for inst in flash, the max fetch width is 32bit
  val FlashWidth = 4 // 4 Byte
  val mmioCnt = Counter(8 / FlashWidth)
  val FlashInst = RegInit(0.U(64.W))
  val mmioReqOnce = req(0).addr(2)
  val mmioCntMax = Mux(mmioReqOnce, 0.U, 1.U)



  switch(state) {
    is(s_idle) {
      afterFirstRead := false.B

      when(((miss(0) || miss(1)) && !storeHit || mmio(0)) && !io.flush || mmioStorePending) {
        //        state := Mux(meta.dirty, s_memWriteReq, s_memReadReq)
        state := Mux(
          mmioStorePending,
          Mux(outBufferValid, s_mmioReq, s_mmio_wait),
          Mux(
            mmio(0),
            s_mmioReq,
            Mux(miss(0) && meta(0).dirty || meta(1).dirty && miss(1), s_memWriteReq, s_memReadReq)
          )
        )
      }
    }
    is(s_mmio_wait) {
      when(!mmioStorePending) {
        state := s_idle
      }.elsewhen(outBufferValid) {
        state := s_mmioReq
      }
    }
    is(s_mmioReq) {
      when(io.mmio.req.fire()) {
        state := s_mmioResp
      }
    }
    is(s_mmioResp) {
      when(io.mmio.resp.fire()) {
        state := Mux(mmio(0), s_wait_resp, s_idle)
      }
    }

    is(s_memReadReq) {
      when(io.mem.req.fire()) {
        state := s_memReadResp
        readBeatCnt.value := Mux(both_miss,Mux(both_miss_refill_at_channel1, addr(1).bankIndex, addr(0).bankIndex),Mux(miss(0),addr(0).bankIndex, addr(1).bankIndex))
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
      when(io.out(0).fire() || io.out(1).fire() ) {
        state := s_idle
      }.elsewhen(bank_conflict){
        state := s_idle
      }.elsewhen(both_miss){
        state := s_idle
      }
    }
  }

  

  val dataRefill = MaskData(
    io.mem.resp.bits.rdata,
    req(0).wdata, //cus the write always on channel 0!
    Mux(readingFirst, Mux(both_miss,Mux(both_miss_refill_at_channel1,wordMask(1), wordMask(0)),Mux(miss(0),wordMask(0),wordMask(1))), 0.U(DataBits.W))
  )
  //  dontTouch(dataRefill)
  val dataRefillWriteBus = Wire(CacheDataArrayWriteBus).apply(
    valid = (state === s_memReadResp) && io.mem.resp.fire(),
    setIdx = Cat(Mux(both_miss,Mux(both_miss_refill_at_channel1,addr(1).index,addr(0).index),Mux(miss(0),addr(0).index,addr(1).index)), readBeatCnt.value),
    data = Wire(new BankedDataBundle).apply(dataRefill),
    waymask = Mux(both_miss,Mux(both_miss_refill_at_channel1,waymask(1), waymask(0)),Mux(miss(0),waymask(0), waymask(1)))
  )

  dataWriteArb.io.in(0) <> dataHitWriteBus.req
  dataWriteArb.io.in(1) <> dataRefillWriteBus.req
  io.dataWriteBus.req <> dataWriteArb.io.out

  val metaRefillWriteBus = Wire(CacheMetaArrayWriteBus()).apply(
    valid =
      (state === s_memReadResp) && io.mem.resp.fire() && io.mem.resp.bits
        .isReadLast(),
    data = Wire(new BankedMetaBundle)
      .apply(valid = true.B, tag = Mux(miss(0),addr(0).tag, addr(1).tag), dirty = Mux(miss(0),req(0).isWrite(), req(1).isWrite())),
    setIdx = getMetaIdx(Mux(miss(0),req(0).addr,req(1).addr)),
    waymask = Mux(miss(0),waymask(0), waymask(1))
  )

  metaWriteArb.io.in(0) <> metaHitWriteBus.req
  metaWriteArb.io.in(1) <> metaRefillWriteBus.req
  io.metaWriteBus.req <> metaWriteArb.io.out
  miss(0)    := !(hitVec(0).orR) && io.in(0).valid && !process_channel1
  io.out(0).bits.user.zip(req(0).user).map { case (o, i) => o := i }
  io.out(0).bits.id.zip(req(0).id).map { case (o, i) => o := i }
  

  // out is valid when cacheline is refilled
  io.out(0).valid := io.in(0).valid && Mux(
    hit(0) || storeHit,
    Mux(process_channel0,false.B,Mux(process_channel1, io.out(1).valid, true.B)),
    Mux(bank_conflict && (state === s_wait_resp) , io.out(1).valid , Mux(both_miss,io.out(1).valid,state === s_wait_resp))
  )

  io.out(1).valid := io.in(1).valid && Mux(
    hit(1),  //
    true.B,
    Mux(bank_conflict,false.B, Mux(both_miss,both_miss_refill_at_channel1 && state === s_wait_resp,state === s_wait_resp))
  )
  // val process_channel1_finish = WireInit(Bool())
  BoringUtils.addSource(io.out(1).valid,"process_channel1_finish")

  val inRdataRegDemand = RegEnable(
    Mux(mmio(0), io.mmio.resp.bits.rdata, io.mem.resp.bits.rdata),
    Mux(mmio(0), state === s_mmioResp, readingFirst)
  )

  val inRdataRegDemand1 = RegEnable(
    Mux(mmio(0), io.mmio.resp.bits.rdata, io.mem.resp.bits.rdata),
    Mux(mmio(0), state === s_mmioResp, later_miss_bank === readBeatCnt.value)
  )
  //bankconflict channel0 readbuffer,save channel0 read data before channel1 to reading.
  val conflict_read_buffer = RegEnable(io.out(0).bits.rdata,io.release_later)
  //bothmiss channel0 readbuffer,save channel0 read data before channel1 to reading.
  val double_miss_read_buffer = RegEnable(inRdataRegDemand, both_miss && state === s_wait_resp)
  val release_later = RegInit(false.B)
  when(miss(0) && bank_conflict && state === s_wait_resp && !release_later && !process_channel1){
    release_later := true.B   //channel0 miss have been processed.
  }.elsewhen(!miss(0) && bank_conflict && !release_later && !process_channel1)(
    release_later := true.B   //channel0 not miss and fetched data,allow channel1 fetch later. 
  ).otherwise{
    release_later := false.B
  }
  io.release_later := release_later
  io.out(0).bits.rdata := Mux(process_channel1,conflict_read_buffer,Mux(hit(0), dataRead(0), Mux(both_miss,double_miss_read_buffer,inRdataRegDemand)))
  io.out(1).bits.rdata := Mux(hit(1), dataRead(1), inRdataRegDemand)

  io.out(0).bits.cmd := Mux(
    io.in(0).bits.req.isRead(),
    SimpleBusCmd.readLast,
    Mux(io.in(0).bits.req.isWrite(), SimpleBusCmd.writeResp, DontCare)
  ) // DontCare, added by lemover

  io.out(1).bits.cmd := Mux(
    io.in(1).bits.req.isRead(),
    SimpleBusCmd.readLast,
    Mux(io.in(1).bits.req.isWrite(), SimpleBusCmd.writeResp, DontCare)
  ) // DontCare, added by lemover


  // With critical-word first, the pipeline registers between
  // s2 and s3 can not be overwritten before a missing request
  // is totally handled. We use io.isFinish to indicate when the
  // request really ends.

  io.in(0).ready := io.out(0).ready && (state === s_idle && !(miss.asUInt.orR()) || io.release_later)
  io.in(1).ready := io.out(1).ready && (state === s_idle && !(miss.asUInt.orR()) || io.release_later)

  // stall when read req in s2 cant be responed or read req in s1 cant be send to s2( s1.in.ready === false.B)
     val cacheStall = WireInit(false.B)
     val s1NotReady = WireInit(false.B)
     BoringUtils.addSource(cacheStall, "cacheStall")
     BoringUtils.addSink(s1NotReady, "s1NotReady")
  val stateBusy = (miss(0) || miss(1))|| state =/= s_idle 
  BoringUtils.addSource(stateBusy, "stateBusy")
     cacheStall := ((miss(0) || miss(1))|| state =/= s_idle || s1NotReady) && !io.release_later
     BoringUtils.addSource(miss(0), "dcacheMissCycle")
     BoringUtils.addSource((miss(0) & (!RegNext(miss(0)))), "dcacheMissCnt")
     BoringUtils.addSource(s1NotReady & (!RegNext(s1NotReady)), "s1NotReadyCnt")
     BoringUtils.addSource(cacheStall & (!RegNext(cacheStall)), "cacheStallCnt")

}


// check


// class BankedflushDCache(implicit val cacheConfig: BankedCacheConfig)
//   extends BankedCacheModule {
//   class BankedflushDCacheIO extends CoreBundle {
//     //    val metaReadResp = Flipped(Vec(Ways, new BankedMetaBundle))
//     val metaReadBus = CacheMetaArrayReadBus()

//     //    val dataReadResp = Flipped(Vec(Ways, new BankedDataBundle))
//     val dataReadBus = CacheDataArrayReadBus()

//     val metaWriteBus = CacheMetaArrayWriteBus()

//     val mem = new SimpleBusUC
//   }
//   val io = IO(new BankedflushDCacheIO)
//   val BankedflushDCache = WireInit(false.B)
//   BoringUtils.addSink(BankedflushDCache, "MOUBankedflushDCache")

//   val way_idle :: way_flush :: way_cntinc :: way_done :: Nil = Enum(4)
//   val idx_idle :: dirty_check_req :: dirty_check_resp :: dirty_true :: idx_cntinc :: idx_done :: Nil =
//     Enum(6)
//   val offset_idle :: read_req :: read_resp :: wb_ok :: offset_cntinc :: offset_done :: Nil =
//     Enum(6)
//   val wayCnt = Counter(Ways)
//   val idxCnt = Counter(Sets)
//   val offsetCnt = Counter(8)
//   val way_state = RegInit(way_idle)
//   val idx_state = RegInit(idx_idle)
//   val offset_state = RegInit(offset_idle)

//   switch(way_state) {
//     is(way_idle) {
//       when(BankedflushDCache) {
//         way_state := way_flush
//       }
//     }
//     is(way_flush) {
//       when(idx_state === idx_done) {
//         way_state := way_cntinc
//       }
//     }
//     is(way_cntinc) {
//       when(wayCnt.value === 3.U) {
//         way_state := way_done
//       }.elsewhen(wayCnt.value =/= 3.U) {
//         way_state := way_flush
//       }
//       wayCnt.inc()
//     }
//     is(way_done) {
//       way_state := way_idle
//     }
//   }

//   switch(idx_state) {
//     is(idx_idle) {
//       when(way_state === way_flush) {
//         idx_state := dirty_check_req
//       }
//     }
//     is(dirty_check_req) {
//       when(io.metaReadBus.req.fire()) {
//         idx_state := dirty_check_resp
//       }
//     }
//     is(dirty_check_resp) {
//       when(
//         Mux1H(MemValid(wayCnt.value), io.metaReadBus.resp.data).dirty
//       ) {
//         idx_state := dirty_true
//       }.otherwise {
//         idx_state := idx_cntinc
//       }
//     }
//     is(dirty_true) {
//       when(offset_state === offset_done) {
//         idx_state := idx_cntinc
//       }
//     }
//     is(idx_cntinc) {
//       when(idxCnt.value === ((Sets - 1).U)) {
//         idx_state := idx_done
//       }.elsewhen(idxCnt.value =/= (Sets - 1).U) {
//         idx_state := dirty_check_req
//       }
//       idxCnt.inc()
//     }
//     is(idx_done) {
//       idx_state := idx_idle
//     }
//   }

//   switch(offset_state) {
//     is(offset_idle) {
//       when(idx_state === dirty_true) {
//         offset_state := read_req
//       }
//     }
//     is(read_req) {
//       when(io.dataReadBus.req.fire()) {
//         offset_state := read_resp
//       }
//     }
//     is(read_resp) {
//       when(io.mem.req.fire()) {
//         offset_state := wb_ok
//       }
//     }
//     is(wb_ok) {
//       offset_state := offset_cntinc
//     }
//     is(offset_cntinc) {
//       when(offsetCnt.value === 7.U) {
//         offset_state := offset_done
//       }.elsewhen(offsetCnt.value =/= 7.U) {
//         offset_state := read_req
//       }
//       offsetCnt.inc()
//     }
//     is(offset_done) {
//       offset_state := offset_idle
//     }
//   }
//   //    BoringUtils.addSource(way_state === way_done, "DCache_done")
//   def MemValid(pc: UInt) = LookupTree(
//     wayCnt.value(1, 0),
//     List(
//       "b00".U -> "b0001".U,
//       "b01".U -> "b0010".U,
//       "b10".U -> "b0100".U,
//       "b11".U -> "b1000".U
//     )
//   )

//   io.metaReadBus.apply(
//     valid = idx_state === dirty_check_req,
//     setIdx = idxCnt.value
//   )
//   io.dataReadBus.apply(
//     valid = offset_state === read_req,
//     setIdx = Cat(idxCnt.value, offsetCnt.value)
//   )
//   io.metaWriteBus.apply(
//     valid = offset_state === offset_done,
//     data = Wire(new BankedMetaBundle).apply(
//       tag = Mux1H(MemValid(wayCnt.value), io.metaReadBus.resp.data).tag,
//       valid = true.B,
//       dirty = false.B
//     ),
//     setIdx = idxCnt.value,
//     waymask = MemValid(wayCnt.value)
//   )
//   io.mem.req.bits.apply(
//     addr = Cat(
//       (Mux1H(MemValid(wayCnt.value), io.metaReadBus.resp.data).tag),
//       idxCnt.value,
//       offsetCnt.value,
//       (0.U(3.W))
//     ),
//     cmd = SimpleBusCmd.write,
//     size = "b11".U,
//     wdata = Mux1H(MemValid(wayCnt.value), io.dataReadBus.resp.data).data,
//     wmask = Fill(DataBytes, 1.U)
//   )
//   io.mem.req.valid := (offset_state === read_resp)
//   io.mem.resp.ready := true.B

//   //  val flush = WireInit(false.B)
//   //  BoringUtils.addSink(flush, "issueStall_flush")
//   //  when(flush) {
//   //    way_state := way_idle
//   //    idx_state := idx_idle
//   //    offset_state := offset_idle
//   //    wayCnt.reset()
//   //    idxCnt.reset()
//   //    offsetCnt.reset()
//   //  }
// }
class DCache(implicit val cacheConfig: BankedCacheConfig)
    extends BankedCacheModule
    with HasBankedCacheIO {
    // cache pipeline

    val s1 = Module(new BankedCacheStage1)
    val s2 = Module(new BankedCacheStage2)
    val metaArray = Module(
      new BankedMetaSRAMTemplateWithArbiter(
        nRead = 1,
        nWrite = 1,
        new BankedMetaBundle,
        set = Sets,
        way = Ways,
        shouldReset = true
      )
    )
    val dataArray = Module(
      new BankedDataArrayWithArbiter(
        nRead = 2,
        new BankedDataBundle,
        set = Sets * LineBeats,
        way = Ways
      )
    )
    // val flushDCache = Module(new BankedflushDCache)
    dataArray.io.r(0) <> s1.io.dataReadBus
    dataArray.io.r(1) <> s2.io.dataReadBus
    // dataArray.io.r(2) <> flushDCache.io.dataReadBus

    dataArray.io.w <> s2.io.dataWriteBus

    metaArray.io.r(0) <> s1.io.metaReadBus
    // metaArray.io.r(1) <> flushDCache.io.metaReadBus

    metaArray.io.w(0) <> s2.io.metaWriteBus
    // metaArray.io.w(1) <> flushDCache.io.metaWriteBus
    

    val Xbar = Module(new SimpleBusCrossbarNto1(1))
    // Xbar.io.in(0) <> flushDCache.io.mem
    Xbar.io.in(0) <> s2.io.mem

    s1.io.in(0) <> io.in(0).req
    s1.io.in(1) <> io.in(1).req

    PipelineConnect(s1.io.out(0), s2.io.in(0), s2.io.out(0).fire(), io.flush)
    PipelineConnect(s1.io.out(1), s2.io.in(1), s2.io.out(1).fire() || s2.io.release_later, io.flush)

    s1.io.release_later := s2.io.release_later

    val process_channel0 = RegInit(false.B)
    when(s1.io.out(0).bits.bank_conflict && !s2.io.release_later && s1.io.metaReadBus(0).req.ready && s1.io.dataReadBus(0).req.ready){
      process_channel0 := true.B
    }.elsewhen(s2.io.release_later){
      process_channel0 := false.B
    }
    val process_channel1 = RegInit(false.B)
    when(s2.io.release_later){
      process_channel1 := true.B
    }.elsewhen(s2.io.out(1).valid){
      process_channel1 := false.B
    }
    BoringUtils.addSource(process_channel0,"process_channel0")
    BoringUtils.addSource(process_channel1,"process_channel1")

    // PipelineVector2Connect(new BankedStage1Out, s1.io.out(0), s1.io.out(1), s2.io.in(0), s2.io.in(1),io.flush,64)
    io.in(0).resp <> s2.io.out(0)
    io.in(1).resp <> s2.io.out(1)
    s2.io.flush := io.flush
    io.out.mem <> Xbar.io.out
    io.out.coh := DontCare
    io.mmio <> s2.io.mmio



    s2.io.metaReadResp(0) := s1.io.metaReadBus(0).resp.data
    s2.io.dataReadResp(0) := s1.io.dataReadBus(0).resp.data
    s2.io.metaReadResp(1) := s1.io.metaReadBus(1).resp.data
    s2.io.dataReadResp(1) := s1.io.dataReadBus(1).resp.data
}
object DCache {
 def apply(in: Vec[SimpleBusUC], mmio: SimpleBusUC, flush: Bool)(implicit
                                                            cacheConfig: BankedCacheConfig
 ) = {
   val cache = Module(new DCache)

   cache.io.flush := flush
   cache.io.in <> in
   mmio <> cache.io.mmio
   cache.io.out
 }
}

//class CacheTest(implicit val cacheConfig: BankedCacheConfig)
//  extends BankedCacheModule
//    with HasBankedCacheIO {
//  // cache pipeline
//
//  val s1 = Module(new BankedCacheStage1)
//  val s2 = Module(new BankedCacheStage1)
//  val metaArray = Module(
//    new MetaSRAMTemplateWithArbiter(
//      nRead = 2,
//      nWrite = 2,
//      new BankedMetaBundle,
//      set = Sets,
//      way = Ways,
//      shouldReset = true
//    )
//  )
//  val dataArray = Module(
//    new ysyxSRAMTemplateWithArbiter(
//      nRead = 3,
//      new BankedDataBundle,
//      set = Sets * LineBeats,
//      way = Ways
//    )
//  )
//
//  val bankedDataArray = Module(new BankedDataArray)
//  val BankedflushDCache = Module(new BankedflushDCache)
//  metaArray.io.r(1) <> BankedflushDCache.io.metaReadBus
//  dataArray.io.r(2) <> BankedflushDCache.io.dataReadBus
//  metaArray.io.w(1) <> BankedflushDCache.io.metaWriteBus
//
//  val Xbar = Module(new SimpleBusCrossbarNto1(2))
//  Xbar.io.in(0) <> BankedflushDCache.io.mem
//  Xbar.io.in(1) <> s2.io.mem
//
//  s1.io.in <> io.in.req
//
//  PipelineConnect(s1.io.out, s2.io.in, s2.io.out.fire(), io.flush)
//
//  io.in.resp <> s2.io.out
//  s2.io.flush := io.flush
//  io.out.mem <> Xbar.io.out
//  io.out.coh := DontCare
//  io.mmio <> s2.io.mmio
//
//  metaArray.io.r(0) <> s1.io.metaReadBus
//  dataArray.io.r(0) <> s1.io.dataReadBus
//  dataArray.io.r(1) <> s2.io.dataReadBus
//
//  metaArray.io.w(0) <> s2.io.metaWriteBus
//  dataArray.io.w <> s2.io.dataWriteBus
//
//  s2.io.metaReadResp := s1.io.metaReadBus.resp.data
//  s2.io.dataReadResp := s1.io.dataReadBus.resp.data
//
//  val sdtag =
//    (s2.io.mem.req.valid && (s2.io.mem.req.bits.addr === "hfc011718".U))
//  dontTouch(sdtag)
//
//  // test tmp
//  val dataIndexTag =
//    dataArray.io.w.req.valid && dataArray.io.w.req.bits.setIdx === "h16e".U
//
//}
