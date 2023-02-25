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


class UnCache()(implicit p: Parameters) extends LazyModule with HasNutCoreParameter with HasDCacheParameters with HasNutCoreParameters {

  val clientParameters = TLMasterPortParameters.v1(
    clients = Seq(TLMasterParameters.v1(
      "uncache",
      sourceId = IdRange(0, 1 << srcBits)
    ))
  )
  val clientNode = TLClientNode(Seq(clientParameters))

  lazy val module = new UncacheImp(this)
}

class UncacheImp(outer: UnCache)extends LazyModuleImp(outer) with HasDCacheIO with HasNutCoreParameter with HasDCacheParameters with HasNutCoreParameters{
  
  val (bus, edge) = outer.clientNode.out.head

  val s_invalid :: s_refill_req :: s_refill_resp :: s_send_resp :: Nil = Enum(4)
  val state = RegInit(s_invalid)

  //Optimal handling when there is mmio store
  val outBufferValid = WireInit(false.B)
  val mmioStorePending = WireInit(false.B)
  val MMIOStorePkt = Wire(Flipped(Decoupled(new StoreBufferEntry)))
  MMIOStorePkt.valid := false.B
  MMIOStorePkt.bits := 0.U.asTypeOf(new StoreBufferEntry)
  BoringUtils.addSink(outBufferValid,"MMIOStorePktValid")
  BoringUtils.addSink(MMIOStorePkt.bits,"MMIOStorePktBits")
  BoringUtils.addSource(MMIOStorePkt.ready,"MMIOStorePktReady")
  BoringUtils.addSink(mmioStorePending,"MMIOStorePending")
  MMIOStorePkt.valid := outBufferValid && (state === s_invalid) && !io.in.req.valid
  val mmioStoreReq = Wire(new SimpleBusReqBundle(userBits = userBits, idBits = idBits))
  //val mmioStoreReqLatch = RegEnable(mmioStoreReq.bits,mmioStoreReq.fire)
  //mmioStoreReq.ready := false.B
  //mmioStoreReq.valid := MMIOStorePkt.valid
  mmioStoreReq.cmd := SimpleBusCmd.write
  mmioStoreReq.addr := MMIOStorePkt.bits.paddr
  mmioStoreReq.wdata := MMIOStorePkt.bits.data
  mmioStoreReq.size := MMIOStorePkt.bits.size
  mmioStoreReq.wmask := MMIOStorePkt.bits.mask

  MMIOStorePkt.ready := Mux(MMIOStorePkt.valid, true.B, false.B)

  //io.mmio.req.bits := Mux(mmioStorePending,mmioStoreReqLatch,req)

  //val req  = Mux(mmioStoreReq.valid, mmioStoreReq, io.in.req)
  val req = Wire(Flipped(Decoupled(new SimpleBusReqBundle(userBits = userBits, idBits = idBits))))
  req := io.in.req
  val resp = io.in.resp
  val mem_acquire = bus.a
  val mem_grant   = bus.d

  //  Assign default values to output signals.
  req.ready := false.B
  io.in.req.ready := false.B
  resp.valid := false.B
  resp.bits := DontCare
  when (MMIOStorePkt.valid) {
    req.bits := mmioStoreReq
    req.valid := true.B
  }

  mem_acquire.valid := false.B
  mem_acquire.bits := DontCare
  mem_grant.ready := false.B

  //  ================================================
  //  FSM state description:
  //  s_invalid     : Entry is invalid.
  //  s_refill_req  : Send Acquire request.
  //  s_refill_resp : Wait for Grant response.
  //  s_send_resp   : Send Uncache response.

  val req_reg = Reg(new SimpleBusReqBundle(userBits = userBits, idBits = idBits))
  val resp_data = Reg(UInt(DataBits.W))

  def storeReq = req_reg.cmd === SimpleBusCmd.write
  
  val load = edge.Get(
    fromSource      = 3.U,
    toAddress       = req_reg.addr,
    lgSize          = req_reg.size
  )._2

  val store = edge.Put(
    fromSource      = 3.U,
    toAddress       = req_reg.addr,
    lgSize          = req_reg.size,
    data            = req_reg.wdata,
    mask            = req_reg.wmask
  )._2

  val (_, _, refill_done, _) = edge.addr_inc(mem_grant)

  switch (state) {
    is (s_invalid) {
      //req.ready := true.B
      io.in.req.ready := true.B
      req.ready := true.B
      
      when (req.fire) {
        req_reg := req.bits
        state := s_refill_req
      }
    }
    is (s_refill_req) {
      mem_acquire.valid := true.B
      mem_acquire.bits := Mux(storeReq, store, load)

      when (mem_acquire.fire) {
        state := s_refill_resp
      }
    }
    is (s_refill_resp) {
      mem_grant.ready := true.B

      when (mem_grant.fire) {
        resp_data := mem_grant.bits.data
        state := Mux(storeReq, s_invalid, s_send_resp)
      }
    }
    is (s_send_resp) {
      resp.valid := true.B
      resp.bits.rdata   := resp_data

      when (resp.fire()) {
        state := s_invalid
      }
    }
  }
}