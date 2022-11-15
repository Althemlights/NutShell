package XiaoHe.SSDbackend.fu

import XiaoHe._
import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
class SSDMOUIO extends NutCoreBundle {
  val pipelinevalid = Input(Bool())
  val in = Flipped(Decoupled(new Bundle {
    val func = Output(FuOpType())
  }))
  val out = Decoupled(Output(UInt(XLEN.W)))
  val flush = Input(Bool())
}

class SSDMOU extends NutCoreModule{
  val io = IO(new SSDMOUIO)
  val sbIsempty = WireInit(false.B)
  BoringUtils.addSink(sbIsempty, "sbIsempty")
  val DCache_done = WireInit(false.B)
  BoringUtils.addSink(DCache_done, "DCache_done")
  val s1_idle :: pipeline_check :: sbcheck :: flush_dcache :: flush_icache :: flush_done :: Nil = Enum(6)
  val state1 = RegInit(s1_idle)
  switch(state1) {
    is(s1_idle) {
      when(io.in.valid) {
        state1 := pipeline_check
      }
    }
    is(pipeline_check) {
      when(io.flush) {
        state1 := s1_idle
      }.elsewhen(!io.pipelinevalid) {
        state1 := sbcheck
      }
    }
    is(sbcheck) {
      when(io.flush) {
        state1 := s1_idle
      }.elsewhen(sbIsempty) {
        state1 := flush_dcache
      }
    }
    is(flush_dcache) {
      when(DCache_done) {
        state1 := flush_icache
      }.elsewhen(io.flush) {
        state1 := s1_idle
      }
    }
    is(flush_icache) {
      state1 := flush_done
    }
    is(flush_done) {
      state1 := s1_idle
    }
  }
//  when (state1 === s1_idle && io.in.valid) { state1 := pipeline_check }
//  when (state1 === pipeline_check && (!io.pipelinevalid)) {state1 := sbcheck}
//  when (state1 === sbcheck && sbIsempty) {state1 := flush_dcache}
//  when (state1 === flush_dcache && DCache_done) {state1 := flush_icache}
//  when (state1 === flush_icache) {state1 := flush_done}
//  when (state1 === flush_done) {state1 := s1_idle}
//  when (io.flush) {state1 := s1_idle}

  val flushICache = (state1 === flush_icache)
  BoringUtils.addSource(flushICache, "MOUFlushICache")
  val flushDCache = (state1 === flush_dcache)
  BoringUtils.addSource(flushDCache, "MOUFlushDCache")

  io.out.bits := 0.U
  io.in.ready := state1 === s1_idle
  io.out.valid := state1 === flush_done

}
