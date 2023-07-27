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
import XiaoHe._
import system._

import freechips.rocketchip.tilelink._
import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp, TransferSizes}
import freechips.rocketchip.tilelink.{ClientMetadata, ClientStates}
import chipsalliance.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.rocket.constants.MemoryOpConstants
import freechips.rocketchip.tilelink.MemoryOpCategories._
import freechips.rocketchip.config.Parameters
case object DCacheParamsKey extends Field[DCacheParameters]

// sealed abstract class DTCMBundle(implicit cacheConfig: SSDCacheConfig) extends Bundle with HasNutCoreParameter with HasCacheConst
sealed abstract class DTCMModule(implicit cacheConfig: SSDCacheConfig) extends Module with HasNutCoreParameter with HasCacheConst with HasNutCoreLog
sealed class DTCMStage1IO(implicit val cacheConfig: SSDCacheConfig) extends CacheBundle {
  val in = Flipped(Decoupled(new SimpleBusReqBundle(userBits = userBits, idBits = idBits)))
  val out = Decoupled(new SimpleBusReqBundle(userBits = userBits, idBits = idBits))
  val dataReadBus = CacheDataArrayReadBus()
}
sealed class DTCMStage1(implicit val cacheConfig: SSDCacheConfig) extends DTCMModule {
  val io = IO(new DTCMStage1IO)
  val readBusValid = io.in.fire
  io.dataReadBus.apply(valid = readBusValid, setIdx = getDataIdx(io.in.bits.addr))
  io.out.bits.req := io.in.bits
  io.out.valid := io.in.valid
  io.in.ready := io.out.ready
}

sealed class DTCMStage2IO(implicit val cacheConfig: SSDCacheConfig) extends DTCMBundle {
  val in = Flipped(Decoupled(new SimpleBusReqBundle(userBits = userBits, idBits = idBits)))
  val out = Decoupled(new SimpleBusReqBundle(userBits = userBits, idBits = idBits))
  val dataReadResp = Flipped(Vec(Ways, new DataBundle))
  val mem = new SimpleBusUC
}

sealed class DTCMStage2(implicit val cacheConfig: SSDCacheConfig) extends DTCMModule {
  val io = IO(new DTCMStage2IO)

}

class DTCMIO extends Bundle with HasNutCoreParameter with HasDCacheParameters {
  val in = Flipped(new SimpleBusUC(userBits = userBits, idBits = idBits))
  val out = new SimpleBusC
  val flush = Input(Bool())
}
class DTCM(implicit val cacheConfig: SSDCacheConfig) extends DTCMModule {
  val io = IO(new DTCMIO)
  val dtcm_s1 = Module(new DTCMStage1)
  val dtcm_s2 = Module(new DTCMStage2)
  val dataArray = Module(new DataSRAMTemplate(new DataBundle, set = Sets * LineBeats, way = Ways))

  dtcm_s1.io.in <> io.in.req
  PipelineConnect(s1.io.out, s2.io.in, s2.io.out.fire, io.flush)
  io.in.resp <> dtcm_s2.io.out
  io.out.mem <> s2.io.mem

  dataArray.io.r <> dtcm_s1.io.dataReadBus
  dataArray.io.w <> dtcm_s2.io.dataWriteBus
  
  dtcm_s2.io.dataReadResp := dtcm_s1.io.dataReadBus.resp.data
  
}
