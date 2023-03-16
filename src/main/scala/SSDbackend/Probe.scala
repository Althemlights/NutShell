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

/*class ProbeReq(edge: TLEdgeOut)(implicit val p: Parameters) extends DCacheBundle{
  val opcode = Wire(UInt(3.W))
  val param  = Wire(UInt(TLPermissions.bdWidth.W))
  val source = Wire(UInt(4.W))
  val addr   = Wire(UInt(36.W))
  val needData = Wire(Bool())

  /*def dump() = {
    Debug("ProbeReq source: %d opcode: %d addr: %x param: %d\n",
      source, opcode, addr, param)
  }*/
}*/

class Probe(edge: TLEdgeOut)(implicit val p: Parameters) extends DCacheModule {
  val io = IO(new Bundle {
    val mem_probe = Flipped(DecoupledIO(new TLBundleB(edge.bundle)))
    val mem_probeAck = DecoupledIO(new TLBundleC(edge.bundle))
    val metaReadBus = CacheMetaArrayReadBus()
    val metaWriteBus = CacheMetaArrayWriteBus()
    val tagReadBus = CacheTagArrayReadBus()
    val dataReadBus = Vec(sramNum, CacheDataArrayReadBus())
  })

  def ProbeReq = new Bundle {
    val opcode = UInt(3.W)
    val param  = UInt(3.W)
    val source = UInt(4.W)
    val addr   = UInt(36.W)
    val needData = Bool()
  }

  // translate to inner req
  val needData = io.mem_probe.bits.data(0)
  val req = io.mem_probe.bits

  //reg for req
  val reqReg = RegEnable(req, io.mem_probe.fire).asTypeOf(new TLBundleB(edge.bundle))
  val addr = reqReg.address.asTypeOf(addrBundle)

  //condition machine: s_probePerm+s_probeBlock|s_probeAck||s_probeAckData|   
  val s_idle :: s_probePB :: s_probeA :: s_probeAD :: Nil = Enum(4)

  val state = RegInit(s_idle)

  io.metaReadBus.apply(io.mem_probe.fire && state === s_idle, setIdx = getMetaIdx(req.address))
  io.tagReadBus.apply(io.mem_probe.fire && state === s_idle, setIdx = getMetaIdx(req.address))

  val metaWay = io.metaReadBus.resp.data 
  val tagWay = io.tagReadBus.resp.data

  //hit and select coh
  val waymask = VecInit(tagWay.map(t => (t.tag === addr.tag))).asUInt
  //val coh = Mux1H(waymask, metaWay).asTypeOf(new ClientMetadata)
  val coh = Mux1H(waymask, metaWay).coh.asTypeOf(new ClientMetadata)
  val (probe_has_dirty_data, probe_shrink_param, probe_new_coh) = coh.onProbe(reqReg.param)
  //val dataRead = Mux1H(waymask, io.dataReadBus(1).resp.data).data

  //refill_count代表c线上refill到第几个了，读应该比它早一拍，比如它在refill第n个时应该读第n+1个
  val (_, _, release_done, refill_count) = edge.count(io.mem_probeAck)
  val count = WireInit(0.U((WordIndexBits - BankBits).W))
  count := Mux(state === s_probePB, 0.U, refill_count + 1.U)
  val probe_block = state === s_probePB && probe_has_dirty_data
  for (w <- 0 until sramNum) {
    io.dataReadBus(w).apply(valid = probe_block || state === s_probeAD, setIdx = Cat(addr.index, count))
  }

  val dataRead = Wire(Vec(sramNum, UInt(XLEN.W)))
  for (w <- 0 until sramNum) {
    dataRead(w) := Mux1H(waymask, io.dataReadBus(w).resp.data).data
    //dataRead(w) := io.mem_grantAck.bits.data(((w + 1) * XLEN) - 1, w * XLEN)
  }
  val metaWriteBus = Wire(CacheMetaArrayWriteBus()).apply(
    valid = state === s_probePB, setIdx = addr.index, waymask = waymask,
    data = Wire(new DMetaBundle).apply(coh = probe_new_coh)
  )
  io.metaWriteBus.req <> metaWriteBus.req

  io.mem_probe.ready := Mux(state === s_idle, true.B, false.B) 
  io.mem_probeAck.valid := Mux(state === s_probeA || state === s_probeAD, true.B, false.B)

  val probeResponse = edge.ProbeAck(
    fromSource = reqReg.source,
    toAddress = reqReg.address,
    lgSize = log2Ceil(LineSize).U,
    reportPermissions = probe_shrink_param
  )

  val probeResponseData = edge.ProbeAck(
    fromSource = reqReg.source,
    toAddress = reqReg.address,
    lgSize = log2Ceil(LineSize).U,
    reportPermissions = probe_shrink_param,
    data = dataRead.asUInt
  )

  io.mem_probeAck.bits := Mux(state === s_probeA, probeResponse, probeResponseData)

  switch(state) {
    //request for meta and data(id needdata)
    is(s_idle) {
      when(io.mem_probe.valid) {
        state := s_probePB
      }
    }
    is (s_probePB) {
      state := Mux(probe_has_dirty_data, s_probeAD, s_probeA)
    }
    is (s_probeA) {
      when(io.mem_probeAck.fire) {
        state := s_idle
      }
    }
    is (s_probeAD) {
      when(release_done) {
        state := s_idle
      }
    }
  }
}