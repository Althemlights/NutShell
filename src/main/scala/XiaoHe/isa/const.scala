package XiaoHe.isa
import chisel3._
import chisel3.util._
trait BranchDecodeUnitConst {

    // Note: Accepts only EXPANDED rvc instructions
  def ComputeBranchTarget(pc: UInt, inst: UInt): UInt = {
    val b_imm32 = Cat(Fill(20,inst(31)), inst(7), inst(30,25), inst(11,8), 0.U(1.W))
    ((pc.asSInt + b_imm32.asSInt).asSInt & (-2).S).asUInt
  }

  // Note: Accepts only EXPANDED rvc instructions
  def ComputeJALTarget(pc: UInt, inst: UInt): UInt = {
    val j_imm32 = Cat(Fill(12,inst(31)), inst(19,12), inst(20), inst(30,25), inst(24,21), 0.U(1.W))
    ((pc.asSInt + j_imm32.asSInt).asSInt & (-2).S).asUInt
  }

  val Y = true.B
  val N = false.B
  val RD_MSB  = 11
  val RD_LSB  = 7
  val RS1_MSB = 19
  val RS1_LSB = 15
  val RS2_MSB = 24
  val RS2_LSB = 20
  val RS3_MSB = 31
  val RS3_LSB = 27
  val CFI_SZ    = 2
    val CFI_X    = 0.U(CFI_SZ.W) // Not a CFI instruction
  val CFI_BR   = 1.U(CFI_SZ.W) // Branch
  val CFI_JAL  = 2.U(CFI_SZ.W) // JAL
  val CFI_JALR = 3.U(CFI_SZ.W) // JALR
  def GetRd (inst: UInt): UInt = inst(RD_MSB,RD_LSB)
  def GetRs1(inst: UInt): UInt = inst(RS1_MSB,RS1_LSB)
}
object MaskLower
{
  def apply(in: UInt) = {
    val n = in.getWidth
    (0 until n).map(i => in >> i.U).reduce(_|_)
  }
}

/**
 * Set all bits at or above the lowest order '1'.
 */
object MaskUpper
{
  def apply(in: UInt) = {
    val n = in.getWidth
    (0 until n).map(i => (in << i.U)(n-1,0)).reduce(_|_)
  }
}