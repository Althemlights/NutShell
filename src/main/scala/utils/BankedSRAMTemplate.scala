/***************************************************************************************
 * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
 * Copyright (c) 2020-2021 Peng Cheng Laboratory
 *
 * XiangShan is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 *          http://license.coscl.org.cn/MulanPSL2
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
 * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
 * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
 *
 * See the Mulan PSL v2 for more details.
 ***************************************************************************************/

/**************************************************************************************
 * Copyright (c) 2020 Institute of Computing Technology, CAS
 * Copyright (c) 2020 University of Chinese Academy of Sciences
 *
 * WuKong is licensed under Mulan PSL v2.
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

package utils

import WuKong.Backend.{BankedCacheBundle, BankedCacheModule, BankedCacheConfig,BankedDataBundle,BankedMetaBundle}
import chisel3._
import chisel3.util._
import chisel3.experimental.ExtModule
class S011HD1P_X32Y2D128_BW_tmp extends ExtModule with HasExtModuleResource {
  //  val io = IO(new Bundle {
  val Q =    IO(Output(UInt(128.W)))
  val Q1 =    IO(Output(UInt(128.W)))
  val CLK =  IO(Input(Clock()))
  val CEN =  IO(Input(Bool()))
  val WEN =  IO(Input(Bool()))
  val BWEN = IO(Input(UInt(128.W)))
  val A =    IO(Input(UInt(6.W)))
  val A1 =    IO(Input(UInt(6.W)))
  val D =    IO(Input(UInt(128.W)))
  //  })
  addResource("/vsrc/S011HD1P_X32Y2D128_BW_tmp.v")
}

class BankedSRAMBundleA(val set: Int) extends Bundle {
  val setIdx = Output(UInt(log2Up(set).W))

  def apply(setIdx: UInt) = {
    this.setIdx := setIdx
    this
  }
}

class BankedSRAMBundleAW[T <: Data](private val gen: T, set: Int, val way: Int = 1) extends SRAMBundleA(set) {
  val data = Output(Vec(way, gen))
  val waymask = if (way > 1) Some(Output(UInt(way.W))) else None

  def apply(data: Vec[T], setIdx: UInt, waymask: UInt): BankedSRAMBundleAW[T] = {
    super.apply(setIdx)
    this.data := data
    this.waymask.map(_ := waymask)
    this
  }
  // this could only be used when waymask is onehot or nway is 1
  def apply(data: T, setIdx: UInt, waymask: UInt): BankedSRAMBundleAW[T] = {
    apply(VecInit(Seq.fill(way)(data)), setIdx, waymask)
    this
  }
}

class BankedSRAMBundleR[T <: Data](private val gen: T, val way: Int = 1) extends Bundle {
  val data = Output(Vec(way, gen))
}

class BankedSRAMReadBus[T <: Data](private val gen: T, val set: Int, val way: Int = 1) extends Bundle {
  val req = Decoupled(new BankedSRAMBundleA(set))
  val resp = Flipped(new BankedSRAMBundleR(gen, way))

  def apply(valid: Bool, setIdx: UInt) = {
    this.req.bits.apply(setIdx)
    this.req.valid := valid
    this
  }
}

class BankedSRAMWriteBus[T <: Data](private val gen: T, val set: Int, val way: Int = 1) extends Bundle {
  val req = Decoupled(new BankedSRAMBundleAW(gen, set, way))

  def apply(valid: Bool, data: Vec[T], setIdx: UInt, waymask: UInt): BankedSRAMWriteBus[T] = {
    this.req.bits.apply(data = data, setIdx = setIdx, waymask = waymask)
    this.req.valid := valid
    this
  }
  def apply(valid: Bool, data: T, setIdx: UInt, waymask: UInt): BankedSRAMWriteBus[T] = {
    apply(valid, VecInit(Seq.fill(way)(data)), setIdx, waymask)
    this
  }
}



class L1BankedDataReadReq(implicit val cacheConfig: BankedCacheConfig) extends BankedCacheBundle {
  val way_en = Bits(DCacheWays.W)
  val addr = Bits(PAddrBits.W)

  def apply(addr: UInt){
    this.addr := addr
    this
  }
}

class L1BankedDataReadLineReq(implicit val p: BankedCacheConfig) extends L1BankedDataReadReq {
  val rmask = Bits(DCacheBanks.W)
}

// Now, we can write a cache-block in a single cycle
class L1BankedDataWriteReq(implicit val p: BankedCacheConfig) extends L1BankedDataReadReq {
  val wmask = Bits(DCacheBanks.W)
  // val data = Vec(DCacheBanks, Bits(LineSize.W))
  val data = Bits(LineSize.W)

  def apply(addr: UInt, wmask: UInt, data: UInt){
    this.addr := addr
    this.wmask := wmask
    this.data := data
    this
  }
}

class L1BankedDataReadResult(implicit val cacheConfig: BankedCacheConfig) extends BankedCacheBundle {
  // you can choose which bank to read to save power
  val raw_data = Bits(LineSize.W)
//  val error = Bool() // slow to generate, use it with care
}

//                     Banked DCache Data
// -----------------------------------------------------------------
// | Bank0 | Bank1 | Bank2 | Bank3 | Bank4 | Bank5 | Bank6 | Bank7 |
// -----------------------------------------------------------------
// | Way0  | Way0  | Way0  | Way0  | Way0  | Way0  | Way0  | Way0  |
// | Way1  | Way1  | Way1  | Way1  | Way1  | Way1  | Way1  | Way1  |
// | ....  | ....  | ....  | ....  | ....  | ....  | ....  | ....  |
// -----------------------------------------------------------------
class BankedSRAMTemplate[T <: Data](gen: T, set: Int, way: Int = 1,
                              shouldReset: Boolean = false, holdRead: Boolean = false, singlePort: Boolean = false, bypassWrite: Boolean = false) extends Module {
  val io = IO(new Bundle {
    val r = Flipped(new BankedSRAMReadBus(gen, set, way))
    val w = Flipped(new BankedSRAMWriteBus(gen, set, way))
  })

  val wordType = UInt(gen.getWidth.W)
  val array = SyncReadMem(set, Vec(way, wordType))
  val (resetState, resetSet) = (WireInit(false.B), WireInit(0.U))

  if (shouldReset) {
    val _resetState = RegInit(true.B)
    val (_resetSet, resetFinish) = Counter(_resetState, set)
    when (resetFinish) { _resetState := false.B }

    resetState := _resetState
    resetSet := _resetSet
  }

  val (ren, wen) = (io.r.req.valid, io.w.req.valid || resetState)
  val realRen = (if (singlePort) ren && !wen else ren)

  val setIdx = Mux(resetState, resetSet, io.w.req.bits.setIdx)
  val wdata = VecInit(Mux(resetState, 0.U.asTypeOf(Vec(way, gen)), io.w.req.bits.data).map(_.asTypeOf(wordType)))
  val waymask = Mux(resetState, Fill(way, "b1".U), io.w.req.bits.waymask.getOrElse("b1".U))
  when (wen) { array.write(setIdx, wdata, waymask.asBools) }

  val raw_rdata = array.read(io.r.req.bits.setIdx, realRen)

  // bypass for dual-port SRAMs
  require(!bypassWrite || bypassWrite && !singlePort)
  def need_bypass(wen: Bool, waddr: UInt, wmask: UInt, ren: Bool, raddr: UInt) : UInt = {
    val need_check = RegNext(ren && wen)
    val waddr_reg = RegNext(waddr)
    val raddr_reg = RegNext(raddr)
    require(wmask.getWidth == way)
    val bypass = Fill(way, need_check && waddr_reg === raddr_reg) & RegNext(wmask)
    bypass.asTypeOf(UInt(way.W))
  }
  val bypass_wdata = if (bypassWrite) VecInit(RegNext(io.w.req.bits.data).map(_.asTypeOf(wordType)))
  else VecInit((0 until way).map(_ => LFSR64().asTypeOf(wordType)))
  val bypass_mask = need_bypass(io.w.req.valid, io.w.req.bits.setIdx, io.w.req.bits.waymask.getOrElse("b1".U), io.r.req.valid, io.r.req.bits.setIdx)
  val mem_rdata = {
    if (singlePort) raw_rdata
    else VecInit(bypass_mask.asBools.zip(raw_rdata).zip(bypass_wdata).map {
      case ((m, r), w) => Mux(m, w, r)
    })
  }

  // hold read data for SRAMs
  val rdata = (if (holdRead) HoldUnless(mem_rdata, RegNext(realRen))
  else mem_rdata).map(_.asTypeOf(gen))

  io.r.resp.data := VecInit(rdata)
  io.r.req.ready := !resetState && (if (singlePort) !wen else true.B)
  io.w.req.ready := true.B

}

abstract class AbstractBankedDataArray(implicit val cacheConfig: BankedCacheConfig) extends BankedCacheModule {
  val ReadlinePortErrorIndex = LoadPipelineWidth
  val io = IO(new Bundle {
    // load pipeline read word req
    val read = Vec(LoadPipelineWidth, Flipped(DecoupledIO(new L1BankedDataReadReq)))
    val write = Flipped(DecoupledIO(new L1BankedDataWriteReq))
    // data bank read resp (all banks)
    val resp = Output(Vec(2, Vec(nWays,new L1BankedDataReadResult())))
    // val nacks = Output(Vec(LoadPipelineWidth, Bool()))
    // when bank_conflict, read (1) port should be ignored

    // val bank_conflict_slow = Output(Vec(LoadPipelineWidth, Bool()))
    // val bank_conflict_fast = Output(Vec(LoadPipelineWidth, Bool()))

  })
  assert(LoadPipelineWidth <= 2) // BankedDataArray is designed for no more than 2 read ports

  def pipeMap[T <: Data](f: Int => T) = VecInit((0 until LoadPipelineWidth).map(f))

  def addr_to_dcache_bank(addr: UInt) = {
    require(addr.getWidth >= DCacheSetOffset)
    addr(DCacheSetOffset - 1, DCacheBankOffset)
  }

  def addr_to_dcache_set(addr: UInt) = {
    require(addr.getWidth >= DCacheAboveIndexOffset)
    addr(DCacheAboveIndexOffset - 1, DCacheSetOffset)
  }

  def get_data_of_bank(bank: Int, data: UInt) = {
    require(data.getWidth >= (bank + 1) * DCacheSRAMRowBits)
    data(DCacheSRAMRowBits * (bank + 1) - 1, DCacheSRAMRowBits * bank)
  }

  def get_mask_of_bank(bank: Int, data: UInt) = {
    require(data.getWidth >= (bank + 1) * DCacheSRAMRowBytes)
    data(DCacheSRAMRowBytes * (bank + 1) - 1, DCacheSRAMRowBytes * bank)
  }
}





class BankedDataArray(implicit val p: BankedCacheConfig) extends AbstractBankedDataArray {


  val ReduceReadlineConflict = false

  io.write.ready := true.B

  // wrap data rows of 8 ways
  class DataSRAMBank(index: Int) extends Module {
    val io = IO(new Bundle() {
      val w = new Bundle() {
        val en = Input(Bool())
        val addr = Input(UInt())
        val way_en = Input(UInt(nWays.W))
        val data = Input(UInt(DCacheSRAMRowBits.W))
      }

      val r = new Bundle() {
        val en = Input(Bool())
        val addr = Input(UInt())
        val data = Output(Vec(nWays,UInt(DCacheSRAMRowBits.W)))
      }
      val rw_conflict = Output(Vec(4, Bool()))
    })



    // multiway data bank
    val data_bank = Array.fill(nWays) {
      Module(new BankedSRAMTemplate(
        Bits(DCacheSRAMRowBits.W),
        set = DCacheSets,
        way = 1,
        shouldReset = false,
        holdRead = false,
        singlePort = true
      ))
    }

    for (w <- 0 until nWays) {
      val wen = io.w.en && io.w.way_en(w)
      data_bank(w).io.w.req.valid := wen
      data_bank(w).io.w.req.bits.apply(
        setIdx = io.w.addr,
        data = io.w.data,
        waymask = 1.U
      )
      data_bank(w).io.r.req.valid := io.r.en
      data_bank(w).io.r.req.bits.apply(setIdx = io.r.addr)
      io.r.data(w) := data_bank(w).io.r.resp.data(0)
      io.rw_conflict(w) := data_bank(w).io.r.req.ready
    }

    // io.r.data := row_data

  }

  val bank_rw_conflict = Wire(Vec(8, Bool()))

  val data_banks = List.tabulate(DCacheBanks)(i => Module(new DataSRAMBank(i)))



  val set_addrs = Wire(Vec(LoadPipelineWidth, UInt()))
  val bank_addrs = Wire(Vec(LoadPipelineWidth, UInt()))
  (0 until LoadPipelineWidth).map(rport_index => {
    // set_addrs(rport_index) := addr_to_dcache_set(io.read(rport_index).bits.addr)
    // bank_addrs(rport_index) := addr_to_dcache_bank(io.read(rport_index).bits.addr)
    set_addrs(rport_index) := io.read(rport_index).bits.addr(8,3)
    bank_addrs(rport_index) := io.read(rport_index).bits.addr(2,0)

    

    io.read(rport_index).ready := bank_rw_conflict(bank_addrs(rport_index))

  })


  // read each bank, get bank result
  val bank_result = Wire(Vec(DCacheBanks, Vec(nWays,new L1BankedDataReadResult())))


  
  for (bank_index <- 0 until DCacheBanks) {
    //     Set Addr & Read Way Mask
    //
    //      Pipe 0      Pipe 1
    //        +           +
    //        |           |
    // +------+-----------+-------+
    //  X                        X
    //   X                      +------+ Bank Addr Match
    //    +---------+----------+
    //              |
    //     +--------+--------+
    //     |    Data Bank    |
    //     +-----------------+
    val bank_addr_matchs = WireInit(VecInit(List.tabulate(LoadPipelineWidth)(i => {
      bank_addrs(i) === bank_index.U && io.read(i).valid
    })))
    val bank_set_addr =  Mux(bank_addr_matchs(0), set_addrs(0), set_addrs(1))
    

    // read raw data
    val data_bank = data_banks(bank_index)
    data_bank.io.r.en := bank_addr_matchs.asUInt.orR 
    data_bank.io.r.addr := bank_set_addr
    bank_result(bank_index) := data_bank.io.r.data.asTypeOf(Vec(nWays,new L1BankedDataReadResult))

    bank_rw_conflict(bank_index) := data_bank.io.rw_conflict.asUInt.andR()
  }

  val two_bank_result = VecInit(bank_result(RegNext(bank_addrs(0))),bank_result(RegNext(bank_addrs(1))))
  // read result: expose banked read result
  io.resp := two_bank_result



  // write data_banks & ecc_banks
  val sram_waddr = io.write.bits.addr(8,3)
  val sram_waddr_bank = io.write.bits.addr(2,0)
  for (bank_index <- 0 until DCacheBanks) {
    // data write
    val data_bank = data_banks(bank_index)
    // data_bank.io.w.en := io.write.valid && io.write.bits.wmask(bank_index)
    data_bank.io.w.en := io.write.valid && (bank_index.U === sram_waddr_bank)

    data_bank.io.w.way_en := io.write.bits.way_en
    data_bank.io.w.addr := sram_waddr
    data_bank.io.w.data := io.write.bits.data

  }

}

class BankedDataArrayWithArbiter[T <: Data](nRead: Int, gen: T, set: Int, way: Int = 1,
                                         shouldReset: Boolean = false) (implicit val cacheConfig: BankedCacheConfig)extends Module {
  val io = IO(new Bundle {
    val r = Flipped(Vec(nRead, Vec(2, new BankedSRAMReadBus(gen, set, way))))
    val w = Flipped(new BankedSRAMWriteBus(gen, set, way))
  })

  val ram = Module(new BankedDataArray)

  ram.io.write.bits.wmask := io.w.req.bits.waymask.getOrElse(0.U)
  ram.io.write.bits.data := io.w.req.bits.data.asUInt()
  ram.io.write.bits.addr := io.w.req.bits.setIdx
  ram.io.write.bits.way_en := io.w.req.bits.waymask.getOrElse(0.U)
  ram.io.write.valid := io.w.req.valid

  io.w.req.ready := ram.io.write.ready
  
  val req00 = io.r(0)(0).req.bits
  val req01 = io.r(0)(1).req.bits
  val req10 = io.r(1)(0).req.bits
  val req11 = io.r(1)(1).req.bits

  val readArb = Module(new Arbiter(Vec(2,chiselTypeOf(io.r(0)(0).req.bits)), 2))
  val sadf = VecInit(req00, req01)
  val arbIn0 = Wire(Decoupled(Vec(2,chiselTypeOf(req00))))
  val arbIn1 = Wire(Decoupled(Vec(2,chiselTypeOf(req00))))
  arbIn0.bits := VecInit(req00,req01)
  arbIn1.bits := VecInit(req10,req11)
  arbIn0.valid :=  io.r(0)(0).req.valid || io.r(0)(1).req.valid 
  arbIn1.valid :=  io.r(1)(0).req.valid || io.r(1)(1).req.valid 
  
  readArb.io.in(0) <> arbIn1
  readArb.io.in(1) <> arbIn0

  val ramIn = Wire(Decoupled(Vec(2, chiselTypeOf(io.r(0)(0).req.bits))))
  ramIn <> readArb.io.out
  ramIn.ready := ram.io.read(1).ready && ram.io.read(0).ready 

  ram.io.read(0).bits.way_en := "b1111".U //?haha
  ram.io.read(0).bits.addr := ramIn.bits(0).setIdx
  ram.io.read(0).valid := Mux(arbIn1.valid, io.r(1)(0).req.valid, io.r(0)(0).req.valid)

  io.r(0)(0).req.ready := arbIn0.ready
  io.r(0)(1).req.ready := arbIn0.ready

  io.r(1)(0).req.ready := arbIn1.ready
  io.r(1)(1).req.ready := arbIn1.ready


  ram.io.read(1).bits.way_en := "b1111".U //?haha
  ram.io.read(1).bits.addr := ramIn.bits(1).setIdx
  ram.io.read(1).valid := Mux(arbIn1.valid, io.r(1)(1).req.valid, io.r(0)(1).req.valid)

    // latch read results
  io.r.map{ case r => {
    r.zipWithIndex.map{ case (t,i)=> {
      t.resp.data := HoldUnless(ram.io.resp(i), RegNext(t.req.fire())).asTypeOf(Vec(way, new BankedDataBundle))
    }}
  }}
}
class BankedMetaSRAMTemplateWithArbiter[T <: Data](nRead: Int, nWrite: Int = 1, gen: T, set: Int, way: Int = 1,
                                             shouldReset: Boolean = false) (implicit val cacheConfig: BankedCacheConfig)extends Module {
  val io = IO(new Bundle {
    val r = Flipped(Vec(nRead, Vec(2, new BankedSRAMReadBus(gen, set, way))))
    val w = Flipped(Vec(nWrite, new BankedSRAMWriteBus(gen, set, way)))
  })


  val ram = Module(new BankedMetaSRAMTemplate(gen, set, way, shouldReset, holdRead = false, singlePort = true))

  // val writeArb = Module(new Arbiter(chiselTypeOf(io.w(0).req.bits), nWrite))
  // writeArb.io.in <> io.w.map(_.req)
  // ram.io.w.req <> writeArb.io.out

  ram.io.w.req <> io.w(0).req

  ram.io.r <> io.r(0)

  // val readArb = Module(new Arbiter(chiselTypeOf(io.r(0).req.bits), nRead))
  // readArb.io.in <> io.r.map(_.req)
  // ram.io.r.req <> readArb.io.out

  // latch read results
  // io.r.map{ case r => {
  //   r.resp.data := HoldUnless(ram.io.r.resp.data, RegNext(r.req.fire()))
  // }}

  io.r.map{ case r => {
    r.zipWithIndex.map{ case (t,i)=> {
      t.resp.data := HoldUnless(ram.io.r(i).resp.data, RegNext(t.req.fire())).asTypeOf(Vec(way, new BankedMetaBundle))
    }}
  }}
}

class BankedMetaSRAMTemplate[T <: Data](gen: T, set: Int, way: Int = 1,
                                  shouldReset: Boolean = false, holdRead: Boolean = false, singlePort: Boolean = false) extends Module {
  val io = IO(new Bundle {
    val r = Flipped(Vec(2, new BankedSRAMReadBus(gen, set, way)))
    val w = Flipped(new BankedSRAMWriteBus(gen, set, way))
  })
  require(!holdRead)
  val wordType = UInt(gen.getWidth.W)
  // val array = SyncReadMem(set, Vec(way, wordType))
  val sram = Module(new S011HD1P_X32Y2D128_BW_tmp())
  val (resetState, resetSet) = (WireInit(false.B), WireInit(0.U))

  if (shouldReset) {
    val _resetState = RegInit(true.B)
    val (_resetSet, resetFinish) = Counter(_resetState, set)
    when (resetFinish) { _resetState := false.B }

    resetState := _resetState
    resetSet := _resetSet
  }

  val (ren0, wen) = (io.r(0).req.valid, io.w.req.valid || resetState)
  val ren1 = io.r(1).req.valid

  val realRen = (if (singlePort) (ren0 || ren1) && !wen else ren0)

  val setIdx = Mux(resetState, resetSet, io.w.req.bits.setIdx)
  val wdataword = Mux(resetState, 0.U.asTypeOf(wordType), io.w.req.bits.data.asUInt)
  val waymask = Mux(resetState, Fill(way, "b1".U), io.w.req.bits.waymask.getOrElse("b1".U))
  // val wdata = VecInit(Seq.fill(way)(Cat(0.U((128/way-gen.getWidth).W), wdataword)))
  val wdata = Cat(0.U(128 - 4*gen.getWidth), wdataword)
  // when (wen) { array.write(setIdx, wdata, waymask.asBools) }

  sram.CLK := clock
  sram.A := Mux(wen, setIdx, io.r(0).req.bits.setIdx)
  sram.CEN := ~(wen || realRen)
  sram.WEN := ~wen
  sram.BWEN := ~Cat(0.U((128 - 4*gen.getWidth).W), 
  VecInit(Seq.fill(gen.getWidth)(waymask(3))).asUInt,
  VecInit(Seq.fill(gen.getWidth)(waymask(2))).asUInt,
  VecInit(Seq.fill(gen.getWidth)(waymask(1))).asUInt,
  VecInit(Seq.fill(gen.getWidth)(waymask(0))).asUInt
  )
  sram.D := wdata

  sram.A1 :=  io.r(1).req.bits.setIdx
  // sram.CEN := ~(wen || realRen)
  // sram.WEN := ~wen
  // sram.BWEN := ~FillInterleaved(128/way, waymask)
  // sram.D := Cat(wdata)


  val rdata0 = sram.Q((4*gen.getWidth)-1,0).asTypeOf(Vec(way, UInt((gen.getWidth).W))).map(_.asTypeOf(gen))
  val rdata1 = sram.Q1((4*gen.getWidth)-1,0).asTypeOf(Vec(way, UInt((gen.getWidth).W))).map(_.asTypeOf(gen))
  io.r(0).resp.data := VecInit(rdata0)
  io.r(1).resp.data := VecInit(rdata1)
  

  io.r(0).req.ready := !resetState && (if (singlePort) !wen else true.B)
  io.r(1).req.ready := !resetState && (if (singlePort) !wen else true.B)
  
  io.w.req.ready := true.B


}