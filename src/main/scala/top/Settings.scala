package top

object DefaultSettings {
  def apply() = Map(
    "MemMapBase" -> 0x0000000000000000L,
    "MemMapRegionBits" -> 0,
    "MMIOBase" -> 0x0000000040000000L,
    "MMIOSize" -> 0x0000000020000000L,
    "ResetVector" -> 0x80000000L,
    "NrExtIntr" -> 1,

    "HasL2cache" -> true,
    "HasPrefetch" -> true,
    "EnableMultiIssue" -> false,
    "EnableSuperScalarExec" -> false,
    "EnableOutOfOrderExec" -> false,
    "HasDTLB" -> true,
    "HasITLB" -> true,
    "HasDcache" -> true,
    "HasIcache" -> true,
    "MmodeOnly" -> false,
    "IsRV32" -> false,

    "EnableILA" -> true,
    "EnableDebug" -> false
  )
}

object PynqSettings {
  def apply() = Map(
    "NrExtIntr" -> 3,
    "ResetVector" -> 0x60000000L,
    "MemMapBase" -> 0x0000000010000000L,
    "MemMapRegionBits" -> 28,
    "MMIOBase" -> 0x00000000e0000000L,
    "MMIOSize" -> 0x0000000020000000L
  )
}

object Axu3cgSettings {
  def apply() = Map()
}

object OOOSettings {
  def apply() = Map(
    "EnableMultiIssue" -> true,
    "EnableSuperScalarExec" -> true,
    "EnableOutOfOrderExec" -> true
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
    "IsRV32" -> true
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
