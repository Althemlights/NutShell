/**************************************************************************************
* Copyright (c) 2020 Institute of Computing Technology, CAS
* Copyright (c) 2020 University of Chinese Academy of Sciences
* 
* NutShell is licensed under Mulan PSL v2.
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

package top

object DefaultSettings {
  def apply() = Map(
    "MemMapBase" -> 0x0000000000000000L,
    "MemMapRegionBits" -> 0,
    "MMIOBase" -> 0x0000000040000000L,
    "MMIOSize" -> 0x0000000040000000L,
    "UnCacheBase" -> 0x0000000000000000L,
    "UnCacheSize" -> 0x0000000060000000L,
    "MemBase" -> 0x0000000080000000L,
    "MemSize" -> 0x0000000080000000L,
    "ResetVector" -> 0x30000000L,
    "NrExtIntr" -> 1,

    "HasL2cache" -> false,
    "HasPrefetch" -> false,
    "EnableMultiIssue" -> true,
    "EnableOutOfOrderExec" -> true,
    "HasDTLB" -> true,
    "HasITLB" -> true,
    "HasDcache" -> true,
    "HasIcache" -> true,
    "MmodeOnly" -> false,
    "IsRV32" -> false,

    "FPGAPlatform" -> false,
    "EnableILA" -> false,
    "EnableDebug" -> false,
    "EnableRVC" -> false,

    "CoreNums" -> 0x1,
    "SoCTest" -> false,
    "CLINTBase" -> 0x38000000L,               // for bin compiled by AM
    //"CLINTBase" -> 0x2000000L,              // for rtthread
    "CLINTSize" -> 0x00010000L,
    "PLICBase" -> 0x000000003c000000L
  )
}

object PynqSettings {
  def apply() = Map(
    "FPGAPlatform" -> true,
    "NrExtIntr" -> 3,
    "ResetVector" -> 0x60000000L,
    "MemMapBase" -> 0x0000000010000000L,
    "MemMapRegionBits" -> 28,
    "MMIOBase" -> 0x00000000e0000000L,
    "MMIOSize" -> 0x0000000020000000L
  )
}

object WkSettings {
  def apply() = Map(
    "FPGAPlatform" -> true,
    "NrExtIntr" -> 0,
    "ResetVector" -> 0x30000000L,
    "EnableDebug" -> false,
    "EnableDifftest" -> false
  )
}

object Axu3cgSettings {
  def apply() = Map(
    "FPGAPlatform" -> true,
    "NrExtIntr" -> 2
  )
}

object PXIeSettings {
  def apply() = Map(
    "FPGAPlatform" -> true,
    "NrExtIntr" -> 5
  )
}

object OOOSettings {
  def apply() = Map(
    "EnableMultiIssue" -> true,
    "EnableOutOfOrderExec" -> true
  )
}

object SoCTestSettings {
  def apply() = Map(
    "NrExtIntr" -> 1,
    "EnableILA" -> false,
    "SoCTest" -> true,
    "ResetVector" -> 0x30000000L,
    "MMIOBase" -> 0x0000000010000000L,
    "MMIOSize" -> 0x0000000070000000L,
    "CLINTBase"-> 0x0000000002000000L,
    "PLICBase" -> 0x0000000002010000L,
    "EnableDebug" -> false,
    "EnableDifftest" -> false,
    "FPGAPlatform" -> false,
    "HasL2cache" -> false,
    "HasPrefetch" -> false,

    "MemMapBase" -> 0x0000000000000000L,
    "MemMapRegionBits" -> 0,
//    "MMIOBase" -> 0x0000000040000000L,
//    "MMIOSize" -> 0x0000000040000000L,
//    "ResetVector" -> 0x80000000L,
//    "NrExtIntr" -> 1,
//
//    "HasL2cache" -> false,
//    "HasPrefetch" -> false,
    "EnableMultiIssue" -> true,
    "EnableOutOfOrderExec" -> true,
    "HasDTLB" -> true,
    "HasITLB" -> true,
    "HasDcache" -> true,
    "HasIcache" -> true,
    "MmodeOnly" -> false,
    "IsRV32" -> false,

//    "FPGAPlatform" -> false,
//    "EnableILA" -> true,
    "EnableRVC" -> true
//    "SoCTest" -> false
  )
}

object InOrderSettings {
  def apply() = Map()
}

object EmbededSettings {
  def apply() = Map(
    "HasL2cache" -> false,
    "HasPrefetch" -> false,
    "HasDTLB" -> false,
    "HasITLB" -> false,
    "HasDcache" -> false,
    "HasIcache" -> false,
    "MmodeOnly" -> true,
    "IsRV32" -> true,
    "EnableRVC" -> false
  )
}

object Settings {
  var settings: Map[String, AnyVal] = DefaultSettings()
  def get(field: String) = {
    settings(field).asInstanceOf[Boolean]
  }
  def getLong(field: String) = {
    settings(field).asInstanceOf[Long]
  }
  def getInt(field: String) = {
    settings(field).asInstanceOf[Int]
  }
}

//****************************************
// Generate RV32 core
//****************************************

// 1. Enable IsRV32
// 2. Set DATAWIDTH = 32 in Makefile
// 3. make a fresh build


//****************************************
// Instructions to boot on axu3cg
//****************************************

// 1. Set FPGAmode = "axu3cg"
// 2. Create a project in fpga/
// 3. Generate bitstram using vivado
// 4. Copy bit-file to server:/tftpboot/axu3cg/, handle soft link of fpga.bit and power on


//****************************************
// Instructions to boot on pynq
//****************************************

// 1. Set FPGAmode = "pynq"
// 2. Create a project in fpga/ with arg "STANDALONE=true" (no "-" in PRJ name)
// 3. Generate bitstram using vivado and export a hardware description file
// 4. Handle soft links in fpga/boot/build/zynq/
//       fpga.bit  ->  bitstream generated by vivado
//       ps.hdf    ->  PRJ_PATH/PRJ.sdk/system_top_wrapper.hdf
// 5. Configure FSBL_LOC variable in fpga/resource/fsbl-loader/Makefile
// 6. Run bootgen, copy BOOT.BIN to SD card and power on
