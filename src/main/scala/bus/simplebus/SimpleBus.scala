package bus.simplebus

import chisel3._
import chisel3.util._

import noop.HasNOOPParameter
import utils._
import bus.axi4._

sealed abstract class SimpleBusBundle extends Bundle with HasNOOPParameter

object SimpleBusCmd {
  // req
                                 //   hit    |    miss
  def read           = "b0000".U //  read    |   refill
  def write          = "b0001".U //  write   |   refill
  def readBurst      = "b0010".U //  read    |   refill
  def writeBurst     = "b0011".U //  write   |   refill
  def writeLast      = "b0111".U //  write   |   refill
  def probe          = "b1000".U //  read    | do nothing

  // resp
  def readLast       = "b0110".U
  def probeHit       = "b1100".U
  def probeMiss      = "b1000".U

  def apply() = UInt(4.W)
}

class SimpleBusReqBundle(userBits: Int = 0) extends SimpleBusBundle {
  val addr = Output(UInt(64.W))
  val size = Output(UInt(3.W))
  val cmd = Output(SimpleBusCmd())
  val wmask = Output(UInt((DataBits / 8).W))
  val wdata = Output(UInt(DataBits.W))
  val user = Output(UInt(userBits.W))

  override def cloneType = new SimpleBusReqBundle(userBits).asInstanceOf[this.type]
  override def toPrintable: Printable = {
    p"addr = 0x${Hexadecimal(addr)}, cmd = ${cmd}, size = ${size}, " +
    p"wmask = 0x${Hexadecimal(wmask)}, wdata = 0x${Hexadecimal(wdata)}"
  }

  def isRead() = !cmd(0) && !cmd(3)
  def isWrite() = cmd(0)
  def isBurst() = cmd(1)
  def isWriteLast() = cmd === SimpleBusCmd.writeLast
  def isProbe() = cmd === SimpleBusCmd.probe
}

class SimpleBusRespBundle(userBits: Int = 0) extends SimpleBusBundle {
  val cmd = Output(SimpleBusCmd())
  val rdata = Output(UInt(DataBits.W))
  val user = Output(UInt(userBits.W))

  override def cloneType = new SimpleBusRespBundle(userBits).asInstanceOf[this.type]
  override def toPrintable: Printable = p"rdata = ${Hexadecimal(rdata)}, cmd = ${cmd}"

  def isReadLast() = cmd === SimpleBusCmd.readLast
  def isProbeHit() = cmd === SimpleBusCmd.probeHit
  def isProbeMiss() = cmd === SimpleBusCmd.probeMiss
}

// Uncache
class SimpleBusUC(userBits: Int = 0) extends SimpleBusBundle {
  val req = Decoupled(new SimpleBusReqBundle(userBits))
  val resp = Flipped(Decoupled(new SimpleBusRespBundle(userBits)))

  override def cloneType = new SimpleBusUC(userBits).asInstanceOf[this.type]
  def isWrite() = req.valid && req.bits.isWrite()
  def isRead()  = req.valid && req.bits.isRead()
  def toAXI4Lite() = SimpleBus2AXI4Converter(this, new AXI4Lite)
  def toAXI4() = SimpleBus2AXI4Converter(this, new AXI4)

  def dump(name: String) = {
    when (req.fire()) { printf(p"${GTimer()},[${name}] ${req.bits}\n") }
    when (resp.fire()) { printf(p"${GTimer()},[${name}] ${resp.bits}\n") }
  }
}

// Cache
class SimpleBusC(userBits: Int = 0) extends SimpleBusBundle {
  val mem = new SimpleBusUC(userBits)
  val coh = Flipped(new SimpleBusUC(userBits))

  override def cloneType = new SimpleBusC(userBits).asInstanceOf[this.type]
}
