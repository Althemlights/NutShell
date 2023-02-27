package SSDbackend

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import chisel3.experimental.IO
import utils._
import bus.simplebus._

import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.ClientMetadata
import chipsalliance.rocketchip.config.Parameters

sealed class AcquireAccess(edge: TLEdgeOut)(implicit val p: Parameters) extends DCacheModule{
  val io = IO(new Bundle {
    val isMMIO = Input(Bool())
    val req = Flipped(Decoupled(new SimpleBusReqBundle(userBits = userBits, idBits = idBits)))
    val resp = Decoupled(new SimpleBusRespBundle(userBits = userBits, idBits = idBits))    //out to cpu, corresponding to req
    val mem_getPutAcquire = DecoupledIO(new TLBundleA(edge.bundle))
    val mem_grantAck = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))
    val mem_finish = DecoupledIO(new TLBundleE(edge.bundle))
    val waymask = Input(UInt(Ways.W))
    val hitTag = Input(Bool())
    val cohOld = Input(new ClientMetadata)    //tag hit + meta miss 
    val metaWriteBus = CacheMetaArrayWriteBus()
    val dataWriteBus = Vec(sramNum, CacheDataArrayWriteBus())
    val tagWriteBus = CacheTagArrayWriteBus()
  })    

  //condition machine: mmio s_get | s_putFullData | s_putPartialData | s_accessAckData | s_AccessAck    s_wait_resp与cpu交互
  //                   miss acquire acquireBlock | acquirePerm | Grant | GrantData | GrantAck
  val s_idle :: s_get :: s_accessAD :: s_put :: s_accessA :: s_waitResp :: s_acqB :: s_acqP :: s_grant :: s_grantD :: s_grantA :: Nil = Enum(11)
  val state = RegInit(s_idle)

  val mmio = io.req.valid && io.isMMIO
  val acquire = io.req.valid && !io.isMMIO
  val req = io.req.bits
  val addr = req.addr.asTypeOf(addrBundle)
  val cohOld = io.cohOld
  val hitTag = io.hitTag

  //miss calculate acquire.param | newCoh
    //acquire block
  val (_, acBParam, _) = (ClientMetadata.onReset).onAccess(req.cmd)
    //acquire perm
  val (_, acPParam, newPCoh) = cohOld.onAccess(req.cmd)

  val (grant_first, _, grant_done, grant_count) = edge.count(io.mem_grantAck)

  val isGrant = io.mem_grantAck.bits.opcode === TLMessages.Grant || io.mem_grantAck.bits.opcode === TLMessages.GrantData

  val newBCoh = ((ClientMetadata.onReset).onGrant(req.cmd, io.mem_grantAck.bits.param)).asTypeOf(new ClientMetadata)
  val newCoh = Mux(hitTag, newPCoh, newBCoh)
  val metaWrValid = (state === s_grant || (state === s_grantD && grant_first)) && isGrant && io.mem_grantAck.fire
  val metaWriteBus = Wire(CacheMetaArrayWriteBus()).apply(
    valid = metaWrValid , setIdx = addr.index, waymask = io.waymask,
    data = Wire(new DMetaBundle).apply(coh = newCoh)
  )
  io.metaWriteBus.req <> metaWriteBus.req

  //val dataWrValid = (state === s_grantD || state === s_grant) && isGrant && io.mem_grantAck.fire 
  val isFullData = state === s_grantD && isGrant && io.mem_grantAck.fire 
  val isOneData = state === s_grant && isGrant && io.mem_grantAck.fire
  val wordMask = Mux(req.isWrite(), MaskExpand(req.wmask), 0.U(DataBits.W))
  val dataRefill = Wire(Vec(sramNum, UInt(XLEN.W)))
  for (w <- 0 until sramNum) {
    dataRefill(w) := MaskData(io.mem_grantAck.bits.data(((w + 1) * XLEN) - 1, w * XLEN), req.wdata, Mux(addr.wordIndex === grant_count && addr.bankIndex === w.U, wordMask, 0.U(DataBits.W)))
  }
  //val dataRefill = MaskData(io.mem_grantAck.bits.data, req.wdata, Mux(addr.wordIndex === grant_count, 0.U(DataBits.W), wordMask))
  val wdata = dataRefill.map{Mux(state === s_grant, req.wdata, _)}
  //val wdata = Mux(state === s_grant, req.wdata, dataRefill)
  val wordIndex = Mux(state === s_grant, addr.wordIndex, grant_count)
  for (w <- 0 until sramNum) {
    io.dataWriteBus(w).apply(
      data = Wire(new DDataBundle).apply(wdata(w)),
      valid = isFullData || (isOneData && addr.wordIndex === grant_count && addr.bankIndex === w.U), setIdx = Cat(addr.index, wordIndex), waymask = io.waymask)
  }
  /*val dataWriteBus = Wire(CacheDataArrayWriteBus()).apply(
    data = Wire(new DDataBundle).apply(wdata),
    valid = dataWrValid, setIdx = Cat(addr.index, wordIndex), waymask = io.waymask)*/
  //io.dataWriteBus.req <> dataWriteBus.req

  val tagWrValid = state === s_grantD && isGrant && grant_first && io.mem_grantAck.fire
  val tagWriteBus = Wire(CacheTagArrayWriteBus()).apply(
    valid = tagWrValid, setIdx = addr.index, waymask = io.waymask,
    data = Wire(new DTagBundle).apply(tag = addr.tag)
  )
  io.tagWriteBus.req <> tagWriteBus.req

  def genWmask(sizeEncode: UInt): UInt = {
    LookupTree(sizeEncode, List(
      "b00".U -> 0x1.U, //0001 << addr(2:0)
      "b01".U -> 0x3.U, //0011
      "b10".U -> 0xf.U, //1111
      "b11".U -> 0xff.U //11111111
    ))
  }

  val acParam = Mux(hitTag, acPParam, acBParam)
  
  val idPutGet = 0.U(srcBits.W)
  val idAcquire = 1.U(srcBits.W)

  /*val putFullData = edge.Put(
    fromSource = idPutGet,
    toAddress = req.addr,
    lgSize = req.size,
    data = req.wdata)._2

  val putPartialData = edge.Put(
    fromSource = idPutGet,
    toAddress = req.addr,
    lgSize = req.size,
    data = req.wdata,
    mask = req.wmask)._2

  val get = edge.Get(
    fromSource = idPutGet,
    toAddress = req.addr,
    lgSize = req.size)._2*/

  val a_address = req.addr & ("hfffffffffffffffff".U << log2Ceil(LineSize).U)

  val acquireBlock = edge.AcquireBlock(
    fromSource = idAcquire, 
    toAddress = a_address, 
    lgSize = log2Ceil(LineSize).U, 
    growPermissions = acParam)._2

  val acquirePerm = edge.AcquirePerm(
    fromSource = idAcquire, 
    toAddress = a_address, 
    lgSize = log2Ceil(LineSize).U, 
    growPermissions = acParam)._2

  val isFullPut = genWmask(req.size) === req.wmask
  /*val pkg = LookupTree(state, List(
      s_get -> get.asUInt,
      s_acqB -> acquireBlock.asUInt, 
      s_acqP -> acquirePerm.asUInt, 
      s_put -> putFullData.asUInt
    ))*/
  val pkg = LookupTree(state, List(
      //s_get -> get.asUInt,
      s_acqB -> acquireBlock.asUInt, 
      s_acqP -> acquirePerm.asUInt 
      //s_put -> putFullData.asUInt
    ))

  val sinkD = RegEnable(io.mem_grantAck.bits.sink, io.mem_grantAck.fire)
  val grantAck = edge.GrantAck(
    toSink = sinkD
  )

  //xxxxxxxx
  val putPartialData = acquireBlock
  val putFullData = acquireBlock

  io.mem_getPutAcquire.valid := Mux(state === s_put || state === s_get || state === s_acqB || state === s_acqP, true.B, false.B)
  io.mem_getPutAcquire.bits := Mux(state === s_put, Mux(isFullPut, putFullData, putPartialData), pkg.asTypeOf(new TLBundleA(edge.bundle)))
  io.mem_grantAck.ready := Mux((state === s_grant || state === s_grantD) && io.tagWriteBus.req.ready && io.metaWriteBus.req.ready, true.B, false.B)
  io.mem_finish.bits := grantAck
  io.mem_finish.valid := state === s_grantA

  switch (state) {
    is (s_idle) {
      when (acquire) {
        state := Mux(hitTag, s_acqP, s_acqB)
      }
    }
    is (s_acqP) {
      when (io.mem_getPutAcquire.fire) {
        state := s_grant
      }      
    }
    is (s_grant) {
      when (io.mem_grantAck.fire && isGrant) {
        state := s_grantA
      }      
    }
    is (s_acqB) {
      when (io.mem_getPutAcquire.fire) {
        state := s_grantD
      }      
    }
    is (s_grantD) {
      when (grant_done && isGrant) {
        state := s_grantA
      }      
    }
    is (s_grantA) {
      when (io.mem_finish.fire) {
        state := s_waitResp
      }      
    }
    is (s_waitResp) {
      when (io.resp.fire) {
        state := s_idle
      }      
    }
  }

  io.req.ready := io.resp.ready && state === s_idle

  val dataRead = Wire(Vec(sramNum, UInt(XLEN.W)))
  for (w <- 0 until sramNum) {
    dataRead(w) := io.mem_grantAck.bits.data(((w + 1) * XLEN) - 1, w * XLEN)
  }
  val bankHitVec = BankHitVec(addr.asUInt)
  val rData = RegEnable(Mux1H(bankHitVec, dataRead), isGrant && io.mem_grantAck.fire && (state === s_accessAD || (state === s_grantD && addr.wordIndex === grant_count)))
  io.resp.valid := io.req.valid && state === s_waitResp
  io.resp.bits.rdata := rData
  io.resp.bits.cmd := SimpleBusCmd.readBurst
  io.resp.bits.user.zip(io.req.bits.user).map { case (o, i) => o := i }
}

sealed class IAcquireAccess(edge: TLEdgeOut)(implicit val p: Parameters) extends ICacheModule{
  val io = IO(new Bundle {
    val isMMIO = Input(Bool())
    val req = Flipped(Decoupled(new SimpleBusReqBundle(userBits = userBits, idBits = idBits)))
    val resp = Decoupled(new SimpleBusRespBundle(userBits = userBits, idBits = idBits))    //out to cpu, corresponding to req
    val mem_getPutAcquire = DecoupledIO(new TLBundleA(edge.bundle))
    val mem_grantAck = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))
    val mem_finish = DecoupledIO(new TLBundleE(edge.bundle))
    val waymask = Input(UInt(Ways.W))
    val hitTag = Input(Bool())
    val cohOld = Input(new ClientMetadata)    //tag hit + meta miss 
    val metaWriteBus = CacheMetaArrayWriteBus()
    val dataWriteBus = Vec(sramNum, CacheDataArrayWriteBus())
    val tagWriteBus = CacheTagArrayWriteBus()
    val needFlush = Input(Bool())
  })    

  //condition machine: mmio s_get | s_putFullData | s_putPartialData | s_accessAckData | s_AccessAck    s_wait_resp与cpu交互
  //                   miss acquire acquireBlock | acquirePerm | Grant | GrantData | GrantAck
  val s_idle :: s_get :: s_accessAD :: s_put :: s_accessA :: s_waitResp :: s_acqB :: s_acqP :: s_grant :: s_grantD :: s_grantA :: Nil = Enum(11)
  val state = RegInit(s_idle)

  val mmio = io.req.valid && io.isMMIO
  val acquire = io.req.valid && !io.isMMIO
  val req = io.req.bits
  val addr = req.addr.asTypeOf(addrBundle)
  val cohOld = io.cohOld
  val hitTag = io.hitTag

  //miss calculate acquire.param | newCoh
    //acquire block
  val (_, acBParam, _) = (ClientMetadata.onReset).onAccess(req.cmd)
    //acquire perm
  val (_, acPParam, newPCoh) = cohOld.onAccess(req.cmd)

  val (grant_first, _, grant_done, grant_count) = edge.count(io.mem_grantAck)

  val isGrant = io.mem_grantAck.bits.opcode === TLMessages.Grant || io.mem_grantAck.bits.opcode === TLMessages.GrantData

  val newBCoh = ((ClientMetadata.onReset).onGrant(req.cmd, io.mem_grantAck.bits.param)).asTypeOf(new ClientMetadata)
  val newCoh = Mux(hitTag, newPCoh, newBCoh)
  val metaWrValid = (state === s_grant || (state === s_grantD && grant_first)) && isGrant && io.mem_grantAck.fire
  val metaWriteBus = Wire(CacheMetaArrayWriteBus()).apply(
    valid = metaWrValid , setIdx = addr.index, waymask = io.waymask,
    data = Wire(new DMetaBundle).apply(coh = newCoh)
  )
  io.metaWriteBus.req <> metaWriteBus.req

  //val dataWrValid = (state === s_grantD || state === s_grant) && isGrant && io.mem_grantAck.fire 
  val isFullData = state === s_grantD && isGrant && io.mem_grantAck.fire 
  val isOneData = state === s_grant && isGrant && io.mem_grantAck.fire
  val wordMask = Mux(req.isWrite(), MaskExpand(req.wmask), 0.U(DataBits.W))
  val dataRefill = Wire(Vec(sramNum, UInt(XLEN.W)))
  for (w <- 0 until sramNum) {
    dataRefill(w) := MaskData(io.mem_grantAck.bits.data(((w + 1) * XLEN) - 1, w * XLEN), req.wdata, Mux(addr.wordIndex === grant_count && addr.bankIndex === w.U, wordMask, 0.U(DataBits.W)))
  }
  //val dataRefill = MaskData(io.mem_grantAck.bits.data, req.wdata, Mux(addr.wordIndex === grant_count, 0.U(DataBits.W), wordMask))
  val wdata = dataRefill.map{Mux(state === s_grant, req.wdata, _)}
  //val wdata = Mux(state === s_grant, req.wdata, dataRefill)
  val wordIndex = Mux(state === s_grant, addr.wordIndex, grant_count)
  for (w <- 0 until sramNum) {
    io.dataWriteBus(w).apply(
      data = Wire(new DDataBundle).apply(wdata(w)),
      valid = isFullData || (isOneData && addr.wordIndex === grant_count && addr.bankIndex === w.U), setIdx = Cat(addr.index, wordIndex), waymask = io.waymask)
  }
  /*val dataWriteBus = Wire(CacheDataArrayWriteBus()).apply(
    data = Wire(new DDataBundle).apply(wdata),
    valid = dataWrValid, setIdx = Cat(addr.index, wordIndex), waymask = io.waymask)*/
  //io.dataWriteBus.req <> dataWriteBus.req

  val tagWrValid = state === s_grantD && isGrant && grant_first && io.mem_grantAck.fire
  val tagWriteBus = Wire(CacheTagArrayWriteBus()).apply(
    valid = tagWrValid, setIdx = addr.index, waymask = io.waymask,
    data = Wire(new DTagBundle).apply(tag = addr.tag)
  )
  io.tagWriteBus.req <> tagWriteBus.req

  def genWmask(sizeEncode: UInt): UInt = {
    LookupTree(sizeEncode, List(
      "b00".U -> 0x1.U, //0001 << addr(2:0)
      "b01".U -> 0x3.U, //0011
      "b10".U -> 0xf.U, //1111
      "b11".U -> 0xff.U //11111111
    ))
  }

  val acParam = Mux(hitTag, acPParam, acBParam)
  
  val idPutGet = 0.U(srcBits.W)
  val idAcquire = 1.U(srcBits.W)

  /*val putFullData = edge.Put(
    fromSource = idPutGet,
    toAddress = req.addr,
    lgSize = req.size,
    data = req.wdata)._2

  val putPartialData = edge.Put(
    fromSource = idPutGet,
    toAddress = req.addr,
    lgSize = req.size,
    data = req.wdata,
    mask = req.wmask)._2

  val get = edge.Get(
    fromSource = idPutGet,
    toAddress = req.addr,
    lgSize = req.size)._2*/

  val a_address = req.addr & ("hfffffffffffffffff".U << log2Ceil(LineSize).U)
  
  val acquireBlock = edge.AcquireBlock(
    fromSource = idAcquire, 
    toAddress = a_address, 
    lgSize = log2Ceil(LineSize).U, 
    growPermissions = acParam)._2

  val acquirePerm = edge.AcquirePerm(
    fromSource = idAcquire, 
    toAddress = a_address, 
    lgSize = log2Ceil(LineSize).U, 
    growPermissions = acParam)._2

  val isFullPut = genWmask(req.size) === req.wmask
  /*val pkg = LookupTree(state, List(
      s_get -> get.asUInt,
      s_acqB -> acquireBlock.asUInt, 
      s_acqP -> acquirePerm.asUInt, 
      s_put -> putFullData.asUInt
    ))*/
  val pkg = LookupTree(state, List(
      //s_get -> get.asUInt,
      s_acqB -> acquireBlock.asUInt, 
      s_acqP -> acquirePerm.asUInt 
      //s_put -> putFullData.asUInt
    ))

  val sinkD = RegEnable(io.mem_grantAck.bits.sink, io.mem_grantAck.fire)
  val grantAck = edge.GrantAck(
    toSink = sinkD
  )

  //xxxxxxxx
  val putPartialData = acquireBlock
  val putFullData = acquireBlock

  io.mem_getPutAcquire.valid := Mux(state === s_put || state === s_get || state === s_acqB || state === s_acqP, true.B, false.B)
  io.mem_getPutAcquire.bits := Mux(state === s_put, Mux(isFullPut, putFullData, putPartialData), pkg.asTypeOf(new TLBundleA(edge.bundle)))
  io.mem_grantAck.ready := Mux(state === s_accessA || state === s_grant || state === s_grantD, true.B, false.B)
  io.mem_finish.bits := grantAck
  io.mem_finish.valid := state === s_grantA

  switch (state) {
    is (s_idle) {
      when (mmio) {
        state := Mux(req.isRead(), s_get, s_put)
      }
      when (acquire) {
        state := Mux(hitTag, s_acqP, s_acqB)
      }
    }
    is (s_put) {
      when (io.mem_getPutAcquire.fire) {
        state := s_accessA
      }
    }
    is (s_accessA) {
      when (io.mem_grantAck.fire) {
        state := s_waitResp
      }
    }
    is (s_get) {
      when (io.mem_getPutAcquire.fire) {
        state := s_accessAD
      }
    }
    is (s_accessAD) {
      when (io.mem_grantAck.fire) {
        state := s_waitResp
      }      
    }
    is (s_acqP) {
      when (io.mem_getPutAcquire.fire) {
        state := s_grant
      }      
    }
    is (s_grant) {
      when (io.mem_grantAck.fire && isGrant) {
        state := s_grantA
      }      
    }
    is (s_acqB) {
      when (io.mem_getPutAcquire.fire) {
        state := s_grantD
      }      
    }
    is (s_grantD) {
      when (grant_done && isGrant) {
        state := s_grantA
      }      
    }
    is (s_grantA) {
      when (io.mem_finish.fire) {
        state := s_waitResp
      }      
    }
    is (s_waitResp) {
      when (io.resp.fire || io.needFlush) {
        state := s_idle
      }      
    }
  }

  io.req.ready := io.resp.ready && state === s_idle

  val dataRead = Wire(Vec(sramNum, UInt(XLEN.W)))
  for (w <- 0 until sramNum) {
    dataRead(w) := io.mem_grantAck.bits.data(((w + 1) * XLEN) - 1, w * XLEN)
  }
  val bankHitVec = BankHitVec(addr.asUInt)
  val rData = RegEnable(Mux1H(bankHitVec, dataRead), isGrant && io.mem_grantAck.fire && (state === s_accessAD || (state === s_grantD && addr.wordIndex === grant_count)))
  io.resp.valid := io.req.valid && state === s_waitResp
  io.resp.bits.rdata := rData
  io.resp.bits.cmd := SimpleBusCmd.readBurst
  io.resp.bits.user.zip(io.req.bits.user).map { case (o, i) => o := i }
}