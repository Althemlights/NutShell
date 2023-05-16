package WuKong.Backend.fu.LoadPipe

import _root_.WuKong.Backend.fu.{
    HasStoreBufferConst,
    LSUOpType,
    LoadPipe,
    StoreBufferEntry,
    storePipeEntry
}
import bus.simplebus.{SimpleBusCmd, SimpleBusReqBundle, SimpleBusUC}
import WuKong._
import WuKong.Backend._
import utils._
import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import _root_.WuKong.Backend.fu.StoreBuffer
import utils.stallPointConnect
import WuKong.isa.FuOpType
import WuKong.{CoreModule, CoreBundle, AddressSpace}

class loadPipeEntry extends CoreBundle {
    val paddr = Output(UInt(VAddrBits.W))
    val mask = Output(UInt((XLEN / 8).W))
    val size = Output(UInt(2.W))
    val isMMIO = Output(Bool()) // load or store
    val func = Output(UInt(7.W))
    val pc = Output(UInt(VAddrBits.W))
}
class StoreHitCtrl extends Bundle {
    val hit = Output(Bool())
    val hitData = Output(UInt(64.W))
    val hitMask = Output(UInt(8.W))
}
class LoadPipeIO extends CoreBundle with HasStoreBufferConst {
    val in = Flipped(Decoupled(new Bundle {
        val src1 = Output(UInt(XLEN.W))
        val offset = Output(UInt(XLEN.W))
        val func = Output(FuOpType())
    }))
    val out = Decoupled(Output(UInt(XLEN.W)))
    // store pipe forward check
    val storePipeE3 = Flipped(Decoupled(new storePipeEntry))
    val storePipeE4 = Flipped(Decoupled(new storePipeEntry))
    // storebuffer forward check
    val storebuffer = Flipped(Vec(StoreBufferSize, new StoreBufferEntry))
    val writePtr = Input(UInt((log2Up(StoreBufferSize) + 1).W))
    val readPtr = Input(UInt((log2Up(StoreBufferSize) + 1).W))
    // val storebuffer = Flipped(Vec(StoreBufferSize, new StoreBufferEntry))

    // d$ read
    val dmem = new SimpleBusUC(addrBits = VAddrBits)
    // invalid
    val invalid = Input(Bool())
    val stall = Input(Bool())
    // debug
    val pc = Input(UInt(VAddrBits.W))
    //s2 valid
    val loadS2Valid = Output(Bool())
    //load mmio
    val mmio = Output(Bool())

}
class LoadPipe (implicit val lname:String)extends CoreModule with HasStoreBufferConst {
    def hitCheck(
        headAddr: UInt,
        tailAddr: UInt,
        vecChecked: Vec[Bool]
    ): Vec[Bool] = {
        val outVec = WireInit(VecInit(Seq.fill(2 * StoreBufferSize)(false.B)))
        for (i <- 0 to StoreBufferSize * 2 - 1) {
            when(i.U < headAddr && i.U >= tailAddr) {
                outVec(i.U) := vecChecked(i.U(log2Up(StoreBufferSize) - 1, 0))
            }.otherwise {
                outVec(i.U) := false.B
            }
        }
        outVec
    }
    def genWmask(addr: UInt, sizeEncode: UInt): UInt = {
        LookupTree(
          sizeEncode,
          List(
            "b00".U -> 0x1.U, // 0001 << addr(2:0)
            "b01".U -> 0x3.U, // 0011
            "b10".U -> 0xf.U, // 1111
            "b11".U -> 0xff.U // 11111111
          )
        ) << addr(2, 0)
    }
    def genWdata(data: UInt, sizeEncode: UInt): UInt = {
        LookupTree(
          sizeEncode,
          List(
            "b00".U -> Fill(8, data(7, 0)),
            "b01".U -> Fill(4, data(15, 0)),
            "b10".U -> Fill(2, data(31, 0)),
            "b11".U -> data
          )
        )
    }
    val io = IO(new LoadPipeIO)
    io.in.ready := !io.stall
    val storeBuffer = io.storebuffer
    val reqAddr = io.in.bits.src1 + io.in.bits.offset
    val size = io.in.bits.func(1, 0)
    val s1Mask = genWmask(reqAddr, size)

    // io.dmem.req.bits.addr := reqAddr
    // io.dmem.req.bits.size := size
    // io.dmem.req.bits.wdata := 0.U
    // io.dmem.req.bits.wmask := 0.U
    // io.dmem.req.bits.cmd := SimpleBusCmd.read

    val s2StoreValid = io.storePipeE3.valid
    val s3StoreValid = io.storePipeE4.valid

    io.storePipeE3.ready := DontCare
    io.storePipeE4.ready := DontCare

    // sb check
    // store buffer pointer
    val readAddr, writeAddr = Wire(UInt(log2Up(StoreBufferSize).W))
    val headAddr, tailAddr = Wire(UInt((log2Up(StoreBufferSize) + 1).W))
    val readFlag, writeFlag = WireInit(false.B)
    readFlag := io.readPtr(log2Up(StoreBufferSize))
    readAddr := io.readPtr(log2Up(StoreBufferSize) - 1, 0)
    writeFlag := io.writePtr(log2Up(StoreBufferSize))
    writeAddr := io.writePtr(log2Up(StoreBufferSize) - 1, 0)
    headAddr := Mux(
      readFlag =/= writeFlag,
      Cat(1.U(1.W), writeAddr),
      Cat(0.U(1.W), writeAddr)
    )
    tailAddr := Cat(0.U(1.W), readAddr)

    val sbAddrHitVec = Wire(Vec(StoreBufferSize, Bool()))
    val sbDataVec = Wire(Vec(2 * StoreBufferSize, UInt(XLEN.W)))
    val sbMaskVec = Wire(Vec(2 * StoreBufferSize, UInt((XLEN / 8).W)))

    for (i <- 0 to StoreBufferSize - 1) {
        sbAddrHitVec(i) := reqAddr(PAddrBits - 1, 3) === storeBuffer(
          i.U(log2Up(StoreBufferSize) - 1, 0)
        ).paddr(PAddrBits - 1, 3)
    }
    for (i <- 0 to 2 * StoreBufferSize - 1) {
        sbDataVec(i) := storeBuffer(i.U(log2Up(StoreBufferSize) - 1, 0)).data
        sbMaskVec(i) := storeBuffer(i.U(log2Up(StoreBufferSize) - 1, 0)).mask
    }

    val s2Hit = reqAddr(PAddrBits - 1, 3) === io.storePipeE3.bits
        .paddr(PAddrBits - 1, 3) && s2StoreValid
    val s3Hit = reqAddr(PAddrBits - 1, 3) === io.storePipeE4.bits
        .paddr(PAddrBits - 1, 3) && s3StoreValid

    val sbAddrHitVecExpand = hitCheck(headAddr, tailAddr, sbAddrHitVec)

    // s2
    val s2Data = io.storePipeE3.bits.data
    val s2Mask = io.storePipeE3.bits.mask & VecInit(Seq.fill(XLEN/8)(s2StoreValid)).asUInt

    // s3
    val s3Data = io.storePipeE4.bits.data
    val s3Mask = io.storePipeE4.bits.mask & VecInit(Seq.fill(XLEN/8)(s3StoreValid)).asUInt

    // sb
    val sbHit = sbAddrHitVecExpand.asUInt.orR
    val sbHitData = Mux1H(sbAddrHitVecExpand, sbDataVec)
    val sbHitMask = Mux1H(sbAddrHitVecExpand, sbMaskVec)
    //

    val s1isHit = Wire(Bool())
    s1isHit := s2Hit || s3Hit || sbHit
    val s1HitData = PriorityMux(
      Seq(s2Hit, s3Hit, sbHit),
      Seq(s2Data, s3Data, sbHitData)
    )
    val s1HitMask = PriorityMux(
      Seq(s2Hit, s3Hit, sbHit),
      Seq(s2Mask | s3Mask | sbHitMask, s3Mask | sbHitMask, sbHitMask)
    )

    val s1FullHit = !(s1Mask & ~s1HitMask).orR
    val s1PartialHit = (s1Mask & s1HitMask).orR && (s1Mask & ~s1HitMask).orR
    val s1CheckMiss = !s1isHit || (!s1FullHit && !s1PartialHit)
    val s1Hit = !s1CheckMiss

    val loadS1In = Wire(Flipped(Decoupled(new loadPipeEntry)))
    val loadS2 = Wire(Flipped(Decoupled(new loadPipeEntry)))
    loadS2.ready := !io.stall && io.dmem.req.ready

    // store hit signal buffer
    val storeHitCtrl = Module(new stallPointConnect(new StoreHitCtrl))
    storeHitCtrl.left.valid := io.in.valid
    storeHitCtrl.right.ready := loadS2.ready
    storeHitCtrl.rightOutFire := loadS2.fire()
    storeHitCtrl.left.bits.hit := s1Hit
    storeHitCtrl.left.bits.hitMask := s1HitMask
    storeHitCtrl.left.bits.hitData := s1HitData
    storeHitCtrl.isStall := io.stall
    storeHitCtrl.io.inValid := io.invalid

    val hitCtrlSignal = storeHitCtrl.right.bits

    // merge and shift -> final data (E3 means in stage3)
    val addrS1 = loadS2.bits.paddr
    val addrHitS2 = storeHitCtrl.right.valid && storeHitCtrl.right.bits.hit
    val mergedDataS2 =
        MaskExpand(hitCtrlSignal.hitMask) & hitCtrlSignal.hitData | ~MaskExpand(
          hitCtrlSignal.hitMask
        ) & io.dmem.resp.bits.rdata
    val hitDataS2 = Mux(addrHitS2, mergedDataS2, io.dmem.resp.bits.rdata)

    loadS1In.valid := io.in.valid
    loadS1In.bits.paddr := reqAddr
    loadS1In.bits.mask := s1Mask
    loadS1In.bits.size := size
    loadS1In.bits.isMMIO := AddressSpace.isMMIO(reqAddr) 
    loadS1In.bits.func := io.in.bits.func
    loadS1In.bits.pc := io.pc

    val lsuPipeStage1 =
        Module(new stallPointConnect(new loadPipeEntry)).suggestName("loadS1")

    lsuPipeStage1.io.left <> loadS1In
    lsuPipeStage1.io.right <> loadS2
    lsuPipeStage1.io.inValid <> io.invalid
    lsuPipeStage1.io.isStall <> io.stall
    lsuPipeStage1.io.rightOutFire <> loadS2.fire()
    io.mmio := loadS2.bits.isMMIO && loadS2.valid

    // d$ req
    io.dmem.req.bits.addr := reqAddr
    io.dmem.req.bits.size := size
    io.dmem.req.bits.wdata := 0.U
    io.dmem.req.bits.wmask := s1Mask
    io.dmem.req.bits.cmd := SimpleBusCmd.read
    io.dmem.req.valid := io.in.valid
    io.dmem.resp.ready := true.B

    // val fname = if (lname.equals("load0")) "loads2valid0" else "loads2valid1"
    // BoringUtils.addSource(loadS2.valid,fname)
    // print(fname)
    io.loadS2Valid := loadS2.valid
    val addrS2 = loadS2.bits.paddr
    val rdataSel = LookupTree(
      addrS2(2, 0),
      List(
        "b000".U -> hitDataS2(63, 0),
        "b001".U -> hitDataS2(63, 8),
        "b010".U -> hitDataS2(63, 16),
        "b011".U -> hitDataS2(63, 24),
        "b100".U -> hitDataS2(63, 32),
        "b101".U -> hitDataS2(63, 40),
        "b110".U -> hitDataS2(63, 48),
        "b111".U -> hitDataS2(63, 56)
      )
    )
    val rdataFinal = LookupTree(
      loadS2.bits.func,
      List(
        LSUOpType.lb -> SignExt(rdataSel(7, 0), XLEN),
        LSUOpType.lh -> SignExt(rdataSel(15, 0), XLEN),
        LSUOpType.lw -> SignExt(rdataSel(31, 0), XLEN),
        LSUOpType.lbu -> ZeroExt(rdataSel(7, 0), XLEN),
        LSUOpType.lhu -> ZeroExt(rdataSel(15, 0), XLEN),
        LSUOpType.lwu -> ZeroExt(rdataSel(31, 0), XLEN)
      )
    )

    // LSU out
    val bufferFullStall = WireInit(false.B)
    BoringUtils.addSink(bufferFullStall, "bufferFullStall")
    val dmemFireLatch = RegInit(false.B)
    when(io.stall && io.dmem.resp.fire()) { dmemFireLatch := true.B }
        .elsewhen(!bufferFullStall) { dmemFireLatch := false.B }
    val rdataValid =
        (io.dmem.resp.fire() || dmemFireLatch || addrHitS2) && loadS2.valid
    val partialLoad = (loadS2.bits.func =/= LSUOpType.ld) && loadS2.valid
    val out = Mux(
      partialLoad,
      rdataFinal,
      Mux(addrHitS2, mergedDataS2, io.dmem.resp.bits.rdata)
    )

    val outLatch = RegInit(0.U(64.W))
    val outValidLatch = RegInit(false.B)
    when(rdataValid && !io.out.ready && !outValidLatch) {
        outLatch := out
        outValidLatch := true.B
    }
    when(outValidLatch && io.out.ready) {
        outLatch := 0.U
        outValidLatch := false.B
    }

    io.out.valid := Mux(
      outValidLatch && io.out.ready,
      outValidLatch,
      rdataValid
    )
    io.out.bits := Mux(!RegNext(io.out.ready) && io.out.ready, outLatch, out)

}
