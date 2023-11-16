package device

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import bus.axi4._
import utils._
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp, RegionType, TransferSizes}
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.amba.axi4.{AXI4Parameters, AXI4SlaveNode, AXI4SlaveParameters, AXI4SlavePortParameters}


class riscv_cpu_io extends Bundle {
  val master = new ysyxAXI4IO()
}

class AXI4Standard(addressSpace: (Long, Long))(implicit p: Parameters) extends LazyModule {
  val totalmemRange = Seq(AddressSet(addressSpace._1, addressSpace._2))
  val node = AXI4SlaveNode(Seq(AXI4SlavePortParameters(
    Seq(AXI4SlaveParameters(
      address = totalmemRange,
      regionType = RegionType.UNCACHED,
      executable = true,
      supportsWrite = TransferSizes(1, 64),
      supportsRead = TransferSizes(1, 64),
      interleavedId = Some(0)
    )),
    beatBytes = 8
  )))

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle{
      val master = new ysyxAXI4IO()
    })
    val (in, edge) = node.in.head

    // translate to standard axi
    in.ar.ready     := io.master.arready
    io.master.arvalid         := in.ar.valid
    io.master.araddr          := in.ar.bits.addr
    io.master.arid            := in.ar.bits.id
    io.master.arlen           := in.ar.bits.len
    io.master.arsize          := in.ar.bits.size
    io.master.arburst         := in.ar.bits.burst

    in.aw.ready     := io.master.awready
    io.master.awvalid         := in.aw.valid
    io.master.awaddr          := in.aw.bits.addr
    io.master.awid            := in.aw.bits.id
    io.master.awlen           := in.aw.bits.len
    io.master.awsize          := in.aw.bits.size
    io.master.awburst         := in.aw.bits.burst

    in.r.valid      := io.master.rvalid
    in.r.ready          := io.master.rready
    in.r.bits.resp  := io.master.rresp
    in.r.bits.data  := io.master.rdata
    in.r.bits.last  := io.master.rlast
    in.r.bits.id    := io.master.rid

    in.w.ready      := io.master.wready
    io.master.wvalid          := in.w.valid
    io.master.wdata           := in.w.bits.data
    io.master.wlast           := in.w.bits.last
    io.master.wstrb           := in.w.bits.strb

    io.master.bready          := in.b.ready
    in.b.valid      := io.master.bvalid
    in.b.bits.resp  := io.master.bresp
    in.b.bits.id    := io.master.bid
  }
}