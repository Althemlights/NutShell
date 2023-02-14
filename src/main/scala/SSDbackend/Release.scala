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
import freechips.rocketchip.tilelink.MemoryOpCategories._

class Release(edge: TLEdgeOut)(implicit val p: Parameters) extends DCacheModule {
  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new SimpleBusReqBundle(userBits = userBits, idBits = idBits)))
    val release_ok = Output(Bool())
    val mem_release = DecoupledIO(new TLBundleC(edge.bundle))
    val mem_releaseAck = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))
    val victimCoh = Input(new ClientMetadata)
    val waymask = Input(UInt(Ways.W))
    val dataReadBus = Vec(sramNum, CacheDataArrayReadBus())
  })    

  
  val req = io.req.bits
  val addr = req.addr.asTypeOf(addrBundle)

  //condition machine: release| releaseData| releaseAck
  val s_idle :: s_release :: s_releaseD :: s_releaseA :: Nil = Enum(4)
  val state = RegInit(s_idle)
  
  io.req.ready := state === s_idle

  val (rel_first, _, rel_done, rel_count) = edge.count(io.mem_release)
  val rCnt = Mux(state === s_idle || (state === s_releaseD && rel_first && !io.mem_release.fire), 0.U(WordIndexBits.W), rel_count + 1.U)
  
  val isRelAck = io.mem_releaseAck.bits.opcode === TLMessages.ReleaseAck

  for (w <- 0 until sramNum) {
    io.dataReadBus(w).apply(valid = (state === s_idle && io.req.valid) || state === s_releaseD,
    setIdx = Cat(addr.index, rCnt))
  }

  //val dataWay = io.dataReadBus.resp.data
  val dataWay = io.dataReadBus.map(_.resp.data)
  //val rData = Mux1H(io.waymask, dataWay).data
  val rData = VecInit(dataWay.map(Mux1H(io.waymask, _).data)).asUInt

  val victimCoh = io.victimCoh
  val (release_has_dirty_data, release_shrink_param, release_new_coh) = victimCoh.onCacheControl(M_FLUSH)

  val idRel = 0.U(srcBits.W)

  val release = edge.Release(
    fromSource = idRel, 
    toAddress = req.addr, 
    lgSize = log2Ceil(LineSize).U, 
    shrinkPermissions = release_shrink_param)._2

  val releaseData = edge.Release(
    fromSource = idRel, 
    toAddress = req.addr, 
    lgSize = log2Ceil(LineSize).U, 
    shrinkPermissions = release_shrink_param, 
    data = rData)._2

  io.release_ok := false.B

  switch (state) {
    is (s_idle) {
      when (io.req.valid) {
        state := Mux(release_has_dirty_data, s_releaseD, s_release)
      }
    }
    is (s_release) {
      when (io.mem_release.fire) {
        state := s_releaseA
      }
    }
    is (s_releaseD) {
      when (rel_done) {
        state := s_releaseA
      }
    }
    is (s_releaseA) {
      when (io.mem_releaseAck.fire && isRelAck) {
        state := s_idle
        io.release_ok := true.B
      }
    }
  }

  io.mem_release.bits := Mux(state === s_release, release, releaseData)
  io.mem_release.valid := state === s_release || state === s_releaseD
  io.mem_releaseAck.ready := state === s_releaseA
}


class IRelease(edge: TLEdgeOut)(implicit val p: Parameters) extends ICacheModule {
  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new SimpleBusReqBundle(userBits = userBits, idBits = idBits)))
    val release_ok = Output(Bool())
    val mem_release = DecoupledIO(new TLBundleC(edge.bundle))
    val mem_releaseAck = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))
    val victimCoh = Input(new ClientMetadata)
    val waymask = Input(UInt(Ways.W))
    val dataReadBus = Vec(sramNum, CacheDataArrayReadBus())
  })    

  
  val req = io.req.bits
  val addr = req.addr.asTypeOf(addrBundle)

  //condition machine: release| releaseData| releaseAck
  val s_idle :: s_release :: s_releaseD :: s_releaseA :: Nil = Enum(4)
  val state = RegInit(s_idle)
  
  io.req.ready := state === s_idle

  val (rel_first, _, rel_done, rel_count) = edge.count(io.mem_release)
  val rCnt = Mux(state === s_idle || (state === s_releaseD && rel_first && !io.mem_release.fire), 0.U(WordIndexBits.W), rel_count + 1.U)
  
  val isRelAck = io.mem_releaseAck.bits.opcode === TLMessages.ReleaseAck

  for (w <- 0 until sramNum) {
    io.dataReadBus(w).apply(valid = (state === s_idle && io.req.valid) || state === s_releaseD,
    setIdx = Cat(addr.index, rCnt))
  }

  //val dataWay = io.dataReadBus.resp.data
  val dataWay = io.dataReadBus.map(_.resp.data)
  //val rData = Mux1H(io.waymask, dataWay).data
  val rData = VecInit(dataWay.map(Mux1H(io.waymask, _).data)).asUInt

  val victimCoh = io.victimCoh
  val (release_has_dirty_data, release_shrink_param, release_new_coh) = victimCoh.onCacheControl(M_FLUSH)

  val idRel = 0.U(srcBits.W)

  val release = edge.Release(
    fromSource = idRel, 
    toAddress = req.addr, 
    lgSize = log2Ceil(LineSize).U, 
    shrinkPermissions = release_shrink_param)._2

  val releaseData = edge.Release(
    fromSource = idRel, 
    toAddress = req.addr, 
    lgSize = log2Ceil(LineSize).U, 
    shrinkPermissions = release_shrink_param, 
    data = rData)._2

  io.release_ok := false.B

  switch (state) {
    is (s_idle) {
      when (io.req.valid) {
        state := Mux(release_has_dirty_data, s_releaseD, s_release)
      }
    }
    is (s_release) {
      when (io.mem_release.fire) {
        state := s_releaseA
      }
    }
    is (s_releaseD) {
      when (rel_done) {
        state := s_releaseA
      }
    }
    is (s_releaseA) {
      when (io.mem_releaseAck.fire && isRelAck) {
        state := s_idle
        io.release_ok := true.B
      }
    }
  }

  io.mem_release.bits := Mux(state === s_release, release, releaseData)
  io.mem_release.valid := state === s_release || state === s_releaseD
  io.mem_releaseAck.ready := state === s_releaseA
}