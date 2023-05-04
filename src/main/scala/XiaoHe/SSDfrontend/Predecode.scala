package XiaoHe.SSDfrontend
import XiaoHe._
import chisel3.util._
import chisel3._
import XiaoHe.isa.RV32I_BRUInstr._
import XiaoHe.isa.RV32I_ALUInstr._
import XiaoHe.isa.RV64IInstr._
import XiaoHe.isa.BranchDecodeUnitConst
import _root_.XiaoHe.isa.MaskLower
import _root_.XiaoHe.isa.MaskUpper
import chisel3.util.experimental.BoringUtils
class PredecodeInfoIO extends Bundle with HasNutCoreParameter {
    val is_call = Bool()
    val is_ret = Bool()
    val target = UInt(VAddrBits.W)
    val cfi_type = UInt(2.W)
    val sfb_offset = Valid(UInt(6.W))
    val shadowable = Bool()
}
class BranchDecodeUnit extends NutCoreModule with BranchDecodeUnitConst {
    val io = IO(new Bundle {
        val instr = Input(UInt(32.W))
        val pc = Input(UInt(VAddrBits.W))
        val out = Output(new PredecodeInfoIO)
    })

    val cs_is_br :: cs_is_jal :: cs_is_jalr :: cs_is_shadowable :: cs_has_rs2 :: Nil =
        ListLookup(
          io.instr,
          List(N, N, N, N, N),
          Array(
////                    is br?
////                    |  is jal?
////                    |  |  is jalr?
////                    |  |  |
////                    |  |  |  shadowable
////                    |  |  |  |  has_rs2
////                    |  |  |  |  |
            JAL -> List(N, Y, N, N, N),
            JALR -> List(N, N, Y, N, N),
            BEQ -> List(Y, N, N, N, N),
            BNE -> List(Y, N, N, N, N),
            BGE -> List(Y, N, N, N, N),
            BGEU -> List(Y, N, N, N, N),
            BLT -> List(Y, N, N, N, N),
            BLTU -> List(Y, N, N, N, N),
            SLLI -> List(N, N, N, Y, N),
            SRLI -> List(N, N, N, Y, N),
            SRAI -> List(N, N, N, Y, N),
            ADDIW -> List(N, N, N, Y, N),
            SLLIW -> List(N, N, N, Y, N),
            SRAIW -> List(N, N, N, Y, N),
            SRLIW -> List(N, N, N, Y, N),
            ADDW -> List(N, N, N, Y, Y),
            SUBW -> List(N, N, N, Y, Y),
            SLLW -> List(N, N, N, Y, Y),
            SRAW -> List(N, N, N, Y, Y),
            SRLW -> List(N, N, N, Y, Y),
            LUI -> List(N, N, N, Y, N),
            ADDI -> List(N, N, N, Y, N),
            ANDI -> List(N, N, N, Y, N),
            ORI -> List(N, N, N, Y, N),
            XORI -> List(N, N, N, Y, N),
            SLTI -> List(N, N, N, Y, N),
            SLTIU -> List(N, N, N, Y, N),
            SLL -> List(N, N, N, Y, Y),
            ADD -> List(N, N, N, Y, Y),
            SUB -> List(N, N, N, Y, Y),
            SLT -> List(N, N, N, Y, Y),
            SLTU -> List(N, N, N, Y, Y),
            AND -> List(N, N, N, Y, Y),
            OR -> List(N, N, N, Y, Y),
            XOR -> List(N, N, N, Y, Y),
            SRA -> List(N, N, N, Y, Y),
            SRL -> List(N, N, N, Y, Y)
          )
        )
    val br_offset = Cat(
      io.instr(7),
      io.instr(30, 25),
      io.instr(11, 8),
      0.U(1.W)
    )
    // Is a sfb if it points forwards (offset is positive)
    io.out.sfb_offset.valid := cs_is_br && !io.instr(
      31
    ) && br_offset =/= 0.U && (br_offset >> log2Ceil(64)) === 0.U
    io.out.sfb_offset.bits := br_offset
    io.out.shadowable := cs_is_shadowable && (
      !cs_has_rs2 ||
          (GetRs1(io.instr) === GetRd(io.instr)) ||
          (io.instr === ADD && GetRs1(io.instr) === "b0".U)
    )

    io.out.is_call := (cs_is_jal || cs_is_jalr) && GetRd(io.instr) === "b01".U
    io.out.is_ret := cs_is_jalr && GetRs1(io.instr) === BitPat(
      "b00?01"
    ) && GetRd(
      io.instr
    ) === "b0".U

    io.out.target := Mux(
      cs_is_br,
      ComputeBranchTarget(io.pc, io.instr),
      ComputeJALTarget(io.pc, io.instr)
    )
    io.out.cfi_type :=
        Mux(
          cs_is_jalr,
          CFI_JALR,
          Mux(cs_is_jal, CFI_JAL, Mux(cs_is_br, CFI_BR, CFI_X))
        )
}
class FetchBundle extends NutCoreBundle {
    val sfbs = Vec(FetchSize, Bool())
    val sfb_mask = Vec(FetchSize, UInt(4.W))
    val shadowable = Vec(FetchSize, Bool())
    val brIdx = UInt(4.W)
}
class PredecodeAtF2 extends NutCoreModule {
    val io = IO(new Bundle {
        // val pc = Input(UInt(VAddrBits.W))
        val inInstr = Flipped(Decoupled(Output(UInt(64.W))))
        val inPredictPkt = Flipped(Decoupled(Output(new PredictPkt)))
        val flush = Input(Bool())
        val sfb = Output(Vec(2, Bool()))
        val outInstr = Decoupled(Output(UInt(64.W)))
        val outPredictPkt = Decoupled(Output(new PredictPkt))
    })

    val f2FetchBundle = Wire(new FetchBundle)

    io.inInstr.ready := io.outInstr.ready
    io.inPredictPkt.ready := io.outInstr.ready
    for (i <- 0 until 2) {
        val bpd = Module(new BranchDecodeUnit)

        val f2Valid = io.inPredictPkt.bits.instValid
        bpd.io.instr := io.inInstr.bits(32 * (i + 1)-1, 32 * i)
        bpd.io.pc := io.inPredictPkt.bits.pc + (i << 2).U

        val offsetFromAlignedPc = bpd.io.out.sfb_offset.bits + (i << 2).U

        f2FetchBundle.sfbs(i) := f2Valid(2 * i) && f2Valid(2 * i + 1) &&
            bpd.io.out.sfb_offset.valid &&
            offsetFromAlignedPc <= 16.U // 4 instrs

        val lower_mask = Wire(UInt((2 * FetchSize).W))
        val upper_mask = Wire(UInt((2 * FetchSize).W))
        lower_mask := UIntToOH((i << 2).U) //without compress instr
        upper_mask := UIntToOH(
          offsetFromAlignedPc(5, 2)
        )

        f2FetchBundle.sfb_mask(i) := ~MaskLower(lower_mask) & ~MaskUpper(
          upper_mask
        )
        f2FetchBundle.shadowable(i) := bpd.io.out.shadowable && f2Valid(
          2 * i
        ) && f2Valid(2 * i + 1)

    }
    f2FetchBundle.brIdx := io.inPredictPkt.bits.brIdx
    // val flush = WireInit(Bool())

    val f3Q = withReset(reset.asBool || io.flush) {
        Module(new Queue(new FetchBundle, 1, pipe = true, flow = false))
    }
    val outInstr = withReset(reset.asBool || io.flush) {
        Module(new Queue(UInt(64.W), 1, pipe = true, flow = false))
    }
    val outPredictPkt = withReset(reset.asBool || io.flush) {
        Module(new Queue(new PredictPkt, 1, pipe = true, flow = false))
    }

    f3Q.io.enq.bits := f2FetchBundle
    f3Q.io.enq.valid := io.inInstr.valid
    f3Q.io.deq.ready := io.outInstr.ready
    // f3Q.io.deq.ready := io.outInstr.ready

    outInstr.io.enq <> io.inInstr
    // outInstr.io.deq.ready := io.outInstr.ready

    outPredictPkt.io.enq <> io.inPredictPkt
    // outPredictPkt.io.enq.bits.sfb := Mux(~io.outInstr.ready, outPredictPkt.io.deq.bits.sfb, outPredictPkt.io.enq.bits.sfb)
    // outPredictPkt.io.deq.ready := io.outInstr.ready

    
    io.outInstr <> outInstr.io.deq
    io.outPredictPkt <> outPredictPkt.io.deq

    io.outPredictPkt.bits.sfb := VecInit(io.sfb(0),io.sfb(0),io.sfb(1),io.sfb(1)).asUInt
    // sfb check logic
    val f3_shadowable_masks = VecInit((0 until FetchSize) map { i =>
        f3Q.io.deq.bits.shadowable.asUInt |
            ~f3Q.io.deq.bits.sfb_mask(i)(FetchSize - 1, 0)
    })
    val f2_shadowable_masks = VecInit((0 until FetchSize) map { i =>
        Mux(f3Q.io.enq.valid, f3Q.io.enq.bits.shadowable.asUInt, 0.U) |
            ~f3Q.io.deq.bits.sfb_mask(i)(2 * FetchSize - 1, FetchSize)
    })

    val f3HaveSfb = VecInit(
      (0 until FetchSize).map { i => {
        (~f3_shadowable_masks(i)).asUInt === 0.U &&
        (~f2_shadowable_masks(i)).asUInt === 0.U &&
        f3Q.io.deq.bits.sfbs(i) &&
        ~f3Q.io.deq.bits.brIdx((i << 1))

      }}
    )
    
    io.sfb(0) := f3HaveSfb(0)
    io.sfb(1) := f3HaveSfb(1)
    
    
    val sfbHigh = f3HaveSfb.reduce(_||_)
    BoringUtils.addSource(sfbHigh,"sfbHigh")
}
