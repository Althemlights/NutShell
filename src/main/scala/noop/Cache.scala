package noop

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import bus.simplebus._
import bus.axi4._
import utils._

sealed trait HasCacheConst {
  val TotalSize = 32 // Kbytes
  val LineSize = 32 // byte
  val LineBeats = LineSize / 4
  val Ways = 1
  val Sets = TotalSize * 1024 / LineSize / Ways
  val OffsetBits = log2Up(LineSize)
  val IndexBits = log2Up(Sets)
  val WordIndexBits = log2Up(LineBeats)
  val TagBits = 32 - OffsetBits - IndexBits
  val dataBits = 32

  val debug = false

  def addrBundle = new Bundle {
    val tag = UInt(TagBits.W)
    val index = UInt(IndexBits.W)
    val wordIndex = UInt(WordIndexBits.W)
    val byteOffset = UInt(2.W)
  }

  def CacheMetaArrayReadBus() = new SRAMReadBus(new MetaBundle, set = Sets, way = Ways)
  def CacheDataArrayReadBus() = new SRAMReadBus(new DataBundle, set = Sets, way = Ways * LineBeats)
  def CacheMetaArrayWriteBus() = new SRAMWriteBus(new MetaBundle, set = Sets, way = Ways)
  def CacheDataArrayWriteBus() = new SRAMWriteBus(new DataBundle, set = Sets, way = Ways * LineBeats)

  def maskExpand(m: UInt): UInt = Cat(m.toBools.map(Fill(8, _)).reverse)
  def isSameWord(a1: UInt, a2: UInt) = ((a1 >> 2) === (a2 >> 2))
  def isSetConflict(a1: UInt, a2: UInt) = (a1.asTypeOf(addrBundle).index === a2.asTypeOf(addrBundle).index)
}

sealed class MetaBundle extends Bundle with HasCacheConst {
  val tag = Output(UInt(TagBits.W))
  val valid = Output(Bool())
  val dirty = Output(Bool())
}

sealed class MetaPipelineBundle extends Bundle with HasCacheConst {
  val tag = Output(UInt(TagBits.W))
  val hit = Output(Bool())
  val dirty = Output(Bool())
}

sealed class DataBundle extends Bundle {
  val data = Output(UInt(32.W))
}

sealed class Stage1IO(userBits: Int = 0) extends Bundle with HasCacheConst {
  val req = new SimpleBusReqBundle(dataBits = dataBits, userBits = userBits)

  override def cloneType = new Stage1IO(userBits).asInstanceOf[this.type]
}

// meta read
sealed class CacheStage1(ro: Boolean, name: String, userBits: Int = 0) extends Module with HasCacheConst {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new SimpleBusReqBundle(dataBits, userBits)))
    val out = Decoupled(new Stage1IO(userBits))
    val metaReadBus = CacheMetaArrayReadBus()
    val dataReadBus = CacheDataArrayReadBus()

    val s2Req = Flipped(Valid(new SimpleBusReqBundle(dataBits)))
    val s3Req = Flipped(Valid(new SimpleBusReqBundle(dataBits)))
    val s2s3Miss = Input(Bool())
  })

  if (ro) when (io.in.fire()) { assert(!io.in.bits.isWrite()) }

  // read meta array and data array
  List(io.metaReadBus, io.dataReadBus).map { case x => {
    x.req.valid := io.in.valid && io.out.ready
    x.req.bits.idx := io.in.bits.addr.asTypeOf(addrBundle).index
  }}

  io.out.bits.req := io.in.bits

  val (addr, s2addr, s3addr) = (io.in.bits.addr, io.s2Req.bits.addr, io.s3Req.bits.addr)
  // set conflict will evict the dirty line, so we should wait
  // the victim line to be up-to-date, else we may writeback staled data
  val s2WriteSetConflict = io.s2Req.valid && isSetConflict(s2addr, addr) && io.s2Req.bits.isWrite()
  val s3WriteSetConflict = io.s3Req.valid && isSetConflict(s3addr, addr) && io.s3Req.bits.isWrite()
  val stall = s2WriteSetConflict || s3WriteSetConflict
  io.out.valid := io.in.valid && !stall && !io.s2s3Miss && io.metaReadBus.req.ready && io.dataReadBus.req.ready
  io.in.ready := (!io.in.valid || io.out.fire()) && io.metaReadBus.req.ready && io.dataReadBus.req.ready
}

sealed class Stage2IO(userBits: Int = 0) extends Bundle with HasCacheConst {
  val req = new SimpleBusReqBundle(dataBits, userBits)
  val meta = new MetaPipelineBundle

  override def cloneType = new Stage2IO(userBits).asInstanceOf[this.type]
}

// check
sealed class CacheStage2(ro: Boolean, name: String, userBits: Int = 0) extends Module with HasCacheConst {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new Stage1IO(userBits)))
    val out = Decoupled(new Stage2IO(userBits))
    val metaReadResp = Flipped(Vec(Ways, new MetaBundle))
  })

  val req = io.in.bits.req
  val addr = req.addr.asTypeOf(addrBundle)
  val meta = io.metaReadResp(0)
  val dirty = if (ro) false.B else meta.dirty

  io.out.bits.meta.hit := meta.valid && (meta.tag === addr.tag) && io.in.valid
  io.out.bits.meta.tag := meta.tag
  io.out.bits.meta.dirty := dirty && io.in.valid
  io.out.bits.req <> io.in.bits.req

  io.out.valid := io.in.valid
  io.in.ready := !io.in.valid || io.out.fire()
}

// writeback
sealed class CacheStage3(ro: Boolean, name: String, userBits: Int = 0) extends Module with HasCacheConst {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new Stage2IO(userBits)))
    val out = Decoupled(new SimpleBusRespBundle(dataBits = dataBits, userBits = userBits))
    val isFinish = Output(Bool())
    val addr = Output(UInt(32.W))
    val flush = Input(Bool())
    val dataBlock = Flipped(Vec(Ways * LineBeats, new DataBundle))
    val dataWriteBus = CacheDataArrayWriteBus()
    val metaWriteBus = CacheMetaArrayWriteBus()
    val mem = new AXI4
  })

  val req = io.in.bits.req
  val addr = req.addr.asTypeOf(addrBundle)
  val meta = io.in.bits.meta
  val hit = io.in.valid && meta.hit
  val miss = io.in.valid && !meta.hit

  val dataBlockIdx = Wire(UInt(WordIndexBits.W))
  val dataRead = io.dataBlock(dataBlockIdx).data

  val wen = if (ro) false.B else req.isWrite()
  val wmaskExpand = maskExpand(req.wmask)
  val wordMask = Mux(wen, wmaskExpand, 0.U(32.W))

  val dataHitWriteBus = WireInit(0.U.asTypeOf(CacheDataArrayWriteBus()))
  val metaHitWriteBus = WireInit(0.U.asTypeOf(CacheMetaArrayWriteBus()))
  if (!ro) {
    val update = hit && wen
    val dataMerge = (dataRead & ~wordMask) | (req.wdata & wordMask)
    dataHitWriteBus.req.valid := update
    dataHitWriteBus.req.bits.idx := addr.index
    dataHitWriteBus.req.bits.data.data := dataMerge
    dataHitWriteBus.req.bits.wordIndex := addr.wordIndex

    metaHitWriteBus.req.valid := update && !meta.dirty
    metaHitWriteBus.req.bits.idx := addr.index
    metaHitWriteBus.req.bits.data.valid := true.B
    metaHitWriteBus.req.bits.data.tag := meta.tag
    metaHitWriteBus.req.bits.data.dirty := true.B
  }

  // if miss, access memory
  io.mem := DontCare
  List(io.mem.ar.bits, io.mem.aw.bits).map { a =>
    a.size := "b10".U
    a.id    := 0.U
    a.len   := (LineBeats - 1).U
    a.burst := AXI4Parameters.BURST_INCR
    a.lock  := false.B
    a.cache := 0.U
    a.qos   := 0.U
    a.user  := 0.U
  }
  // critical word first
  io.mem.ar.bits.burst := AXI4Parameters.BURST_WRAP

  val s_idle :: s_memReadReq :: s_memReadResp :: s_memWriteReq :: s_memWriteResp :: s_wait_resp :: Nil = Enum(6)
  val state = RegInit(s_idle)
  val needFlush = Reg(Bool())
  when (io.flush && (state =/= s_idle)) { needFlush := true.B }
  when (io.out.fire() && needFlush) { needFlush := false.B }

  io.mem.ar.valid := (state === s_memReadReq)

  val wSend = Wire(Bool())
  val awAck = BoolStopWatch(io.mem.aw.fire(), wSend)
  val wAck = BoolStopWatch(io.mem.w.fire() && io.mem.w.bits.last, wSend)
  wSend := (io.mem.aw.fire() && io.mem.w.fire() && io.mem.w.bits.last) || (awAck && wAck)

  io.mem.aw.valid := (state === s_memWriteReq) && !awAck
  io.mem. w.valid := (state === s_memWriteReq) && !wAck

  // critical word first
  io.mem.ar.bits.addr := Cat(req.addr(31, 2), 0.U(2.W))
  // dirty block addr
  io.mem.aw.bits.addr := Cat(meta.tag, addr.index, 0.U(OffsetBits.W))

  io.mem.r.ready := true.B
  io.mem.b.ready := true.B

  val readBeatCnt = Counter(LineBeats)
  val writeBeatCnt = Counter(LineBeats)
  io.mem.w.bits.data := dataRead
  io.mem.w.bits.strb := 0xf.U
  io.mem.w.bits.last := (writeBeatCnt.value === (LineBeats - 1).U)

  dataBlockIdx := Mux(state === s_memWriteReq, writeBeatCnt.value, addr.wordIndex)

  val metaRefillWriteBus = WireInit(0.U.asTypeOf(CacheMetaArrayWriteBus()))
  val dataRefillWriteBus = WireInit(0.U.asTypeOf(CacheDataArrayWriteBus()))
  val afterFirstRead = Reg(Bool())
  val alreadyOutFire = RegEnable(true.B, io.out.fire())
  val readingFirst = !afterFirstRead && io.mem.r.fire()
  val inRdataRegDemand = RegEnable(io.mem.r.bits.data, readingFirst)


  switch (state) {
    is (s_idle) {
      afterFirstRead := false.B
      alreadyOutFire := false.B
      // actually this can use s2 to test
      when (miss && !io.flush) { state := Mux(if (ro) false.B else meta.dirty, s_memWriteReq, s_memReadReq) }
    }
    is (s_memReadReq) { when (io.mem.ar.fire()) {
      state := s_memReadResp
      readBeatCnt.value := addr.wordIndex
    }}

    is (s_memReadResp) {
      when (io.mem.r.fire()) {
        val rdata = io.mem.r.bits.data
        afterFirstRead := true.B

        val inRdata = if (!ro) {
          val rdataMergeWrite = (rdata & ~wordMask) | (req.wdata & wordMask)
          Mux(readingFirst, rdataMergeWrite, rdata)
        } else rdata

        dataRefillWriteBus.req.bits.data.data := inRdata
        dataRefillWriteBus.req.bits.wordIndex := readBeatCnt.value

        readBeatCnt.inc()
        when (io.mem.r.bits.last) { state := s_wait_resp }
      }
    }

    is (s_memWriteReq) {
      when (io.mem.w.fire()) { writeBeatCnt.inc() }
      when (wSend) { state := Mux(io.mem.b.fire(), s_memReadReq, s_memWriteResp) }
    }

    is (s_memWriteResp) { when (io.mem.b.fire()) { state := s_memReadReq } }
    is (s_wait_resp) { when (io.out.fire() || needFlush || alreadyOutFire) { state := s_idle } }
  }


  dataRefillWriteBus.req.valid := (state === s_memReadResp) && io.mem.r.fire()
  dataRefillWriteBus.req.bits.idx := addr.index

  val dataWriteArb = Module(new Arbiter(CacheDataArrayWriteBus().req.bits, 2))
  dataWriteArb.io.in(0) <> dataHitWriteBus.req
  dataWriteArb.io.in(1) <> dataRefillWriteBus.req
  io.dataWriteBus.req <> dataWriteArb.io.out

  metaRefillWriteBus.req.valid := (state === s_memReadResp) && io.mem.r.fire() && io.mem.r.bits.last
  metaRefillWriteBus.req.bits.idx := addr.index
  metaRefillWriteBus.req.bits.data.valid := true.B
  metaRefillWriteBus.req.bits.data.tag := addr.tag
  if (!ro) metaRefillWriteBus.req.bits.data.dirty := wen

  val metaWriteArb = Module(new Arbiter(CacheMetaArrayWriteBus().req.bits, 2))
  metaWriteArb.io.in(0) <> metaHitWriteBus.req
  metaWriteArb.io.in(1) <> metaRefillWriteBus.req
  io.metaWriteBus.req <> metaWriteArb.io.out

  io.out.bits.rdata := Mux(hit, dataRead, inRdataRegDemand)
  io.out.bits.user := io.in.bits.req.user
  io.out.valid := io.in.valid && Mux(hit, true.B, Mux(wen, state === s_wait_resp, afterFirstRead && !alreadyOutFire))
  // With critical-word first, the pipeline registers between
  // s2 and s3 can not be overwritten before a missing request
  // is totally handled. We use io.isFinish to indicate when the
  // request is really end.
  io.isFinish := Mux(hit || wen, io.out.fire(), (state === s_wait_resp) && (io.out.fire() || alreadyOutFire))

  io.addr := req.addr
  io.in.ready := io.out.ready && (state === s_idle) && !miss

  assert(!(metaHitWriteBus.req.valid && metaRefillWriteBus.req.valid))
  assert(!(dataHitWriteBus.req.valid && dataRefillWriteBus.req.valid))
  Debug(debug) {
    printf("%d: [" + name + " stage3]: in.ready = %d, in.valid = %d, state = %d, addr = %x\n",
      GTimer(), io.in.ready, io.in.valid, state, req.addr)
  }
}

class Cache(ro: Boolean, name: String, dataBits: Int = 32, userBits: Int = 0) extends Module with HasCacheConst {
  val io = IO(new Bundle {
    val in = Flipped(new SimpleBus(dataBits, userBits))
    val addr = Output(UInt(32.W))
    val flush = Input(UInt(2.W))
    val mem = new AXI4
  })

  val s1 = Module(new CacheStage1(ro, name, userBits))
  val s2 = Module(new CacheStage2(ro, name, userBits))
  val s3 = Module(new CacheStage3(ro, name, userBits))
  val metaArray = Module(new SRAMTemplate(new MetaBundle, set = Sets, way = Ways, shouldReset = true, singlePort = true))
  val dataArray = Module(new SRAMTemplate(new DataBundle, set = Sets, way = Ways * LineBeats, shouldReset = true, singlePort = true))

  s1.io.in <> io.in.req
  PipelineConnect(s1.io.out, s2.io.in, s2.io.out.fire(), io.flush(0))
  PipelineConnect(s2.io.out, s3.io.in, s3.io.isFinish, io.flush(1))
  io.in.resp <> s3.io.out

  s3.io.flush := io.flush(1)
  io.addr := s3.io.addr
  io.mem <> s3.io.mem

  // stalling
  s1.io.s2Req.valid := s2.io.in.valid
  s1.io.s2Req.bits := s2.io.in.bits.req
  s1.io.s3Req.valid := s3.io.in.valid
  s1.io.s3Req.bits := s3.io.in.bits.req
  s1.io.s2s3Miss := s3.io.in.valid && !s3.io.in.bits.meta.hit

  metaArray.io.r <> s1.io.metaReadBus
  metaArray.io.w <> s3.io.metaWriteBus
  dataArray.io.r <> s1.io.dataReadBus
  dataArray.io.w <> s3.io.dataWriteBus
  s2.io.metaReadResp := metaArray.io.r.resp.data
  s3.io.dataBlock := RegEnable(dataArray.io.r.resp.data, s2.io.out.fire())

  BoringUtils.addSource(s3.io.in.valid && s3.io.in.bits.meta.hit, "perfCntCondM" + name + "Hit")

  Debug(debug) {
    io.in.dump(name + ".in")
    printf("%d: s1:(%d,%d), s2:(%d,%d), s2:(%d,%d)\n",
      GTimer(), s1.io.in.valid, s1.io.in.ready, s2.io.in.valid, s2.io.in.ready, s3.io.in.valid, s3.io.in.ready)
    when (s1.io.in.valid) { printf(p"[${name}.S1]: ${s1.io.in.bits}\n") }
    when (s2.io.in.valid) { printf(p"[${name}.S2]: ${s2.io.in.bits.req}\n") }
    when (s3.io.in.valid) { printf(p"[${name}.S3]: ${s3.io.in.bits.req}\n") }
  }
}
