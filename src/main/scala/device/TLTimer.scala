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

package device

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._
import chipsalliance.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper.RegField

class TLTimer(address: Seq[AddressSet], sim: Boolean)(implicit p: Parameters) extends LazyModule {

  val device = new SimpleDevice("clint", Seq("XiangShan", "clint"))
  val node = TLRegisterNode(address, device, beatBytes = 8)

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle() {
      val mtip = Output(Bool())
      val msip = Output(Bool())
    })

    val mtime = RegInit(0.U(64.W))  // unit: us
    val mtimecmp = RegInit(200.U(64.W))
    val msip = RegInit(0.U(64.W))

    val clk = (if (!sim) 40 /* 40MHz / 1000000 */ else 10000)
    val freq = RegInit(clk.U(64.W))
    val inc = RegInit(1.U(64.W))

    val cnt = RegInit(0.U(64.W))
    val nextCnt = cnt + 1.U
    cnt := Mux(nextCnt < freq, nextCnt, 0.U)
    val tick = (nextCnt === freq)
    when (tick) { mtime := mtime + inc }


    node.regmap( 
      0x0    -> Seq(RegField(64,msip)),
      0x4000 -> Seq(RegField(64,mtimecmp)),
      0x8000 -> Seq(RegField(64,freq)),
      0x8008 -> Seq(RegField(64,inc)),
      0xbff8 -> Seq(RegField(64,mtime))
   )

    io.mtip := RegNext(mtime >= mtimecmp)
    io.msip := RegNext(msip =/= 0.U)
    
  }
}