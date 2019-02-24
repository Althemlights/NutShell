// See LICENSE.SiFive for license details.

package memory

import chisel3._
import chisel3.util._
import utils.LFSR64

// q is the probability to delay a request
class AXI4Delayer(q: Double) extends Module {
  val io = IO(new Bundle{
    val in = Flipped(new AXI4)
    val out = new AXI4
  })

  require (0.0 <= q && q < 1)

  def feed[T <: Data](sink: DecoupledIO[T], source: DecoupledIO[T]) {
    // irrevocable requires that we not lower valid
    val hold = RegInit(false.B)
    when (sink.valid)  { hold := true.B }
    when (sink.fire()) { hold := false.B }

    val allow = hold || ((q * 65535.0).toInt).U <= LFSR64(source.valid)(15, 0)
    sink.valid := source.valid && allow
    source.ready := sink.ready && allow
    sink.bits := source.bits
  }

  feed(io.out.ar, io.in.ar)
  feed(io.out.aw, io.in.aw)
  feed(io.out.w,  io.in.w )
  feed(io.in.b,   io.out.b)
  feed(io.in.r,   io.out.r)
}
