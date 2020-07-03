
################################################################
# This is a generated script based on design: noop
#
# Though there are limitations about the generated script,
# the main purpose of this utility is to make learning
# IP Integrator Tcl commands easier.
################################################################

namespace eval _tcl {
proc get_script_folder {} {
   set script_path [file normalize [info script]]
   set script_folder [file dirname $script_path]
   return $script_folder
}
}
variable script_folder
set script_folder [_tcl::get_script_folder]

################################################################
# Check if script is running in correct Vivado version.
################################################################
set scripts_vivado_version 2019.1
set current_vivado_version [version -short]

if { [string first $scripts_vivado_version $current_vivado_version] == -1 } {
   puts ""
   catch {common::send_msg_id "BD_TCL-109" "ERROR" "This script was generated using Vivado <$scripts_vivado_version> and is being run in <$current_vivado_version> of Vivado. Please run the script in Vivado <$scripts_vivado_version> then open the design in Vivado <$current_vivado_version>. Upgrade the design by running \"Tools => Report => Report IP Status...\", then run write_bd_tcl to create an updated script."}

   return 1
}

################################################################
# START
################################################################

# To test this script, run the following commands from Vivado Tcl console:
# source noop_script.tcl


# The design that will be created by this Tcl script contains the following 
# module references:
# NutShellSoC, AXI4VGA

# Please add the sources of those modules before sourcing this Tcl script.

# If there is no project opened, this script will create a
# project, but make sure you do not have an existing project
# <./myproj/project_1.xpr> in the current working folder.

set list_projs [get_projects -quiet]
if { $list_projs eq "" } {
   create_project project_1 myproj -part xczu19eg-ffvc1760-2-i
}


# CHANGE DESIGN NAME HERE
variable design_name
set design_name noop

# If you do not already have an existing IP Integrator design open,
# you can create a design using the following command:
#    create_bd_design $design_name

# Creating design if needed
set errMsg ""
set nRet 0

set cur_design [current_bd_design -quiet]
set list_cells [get_bd_cells -quiet]

if { ${design_name} eq "" } {
   # USE CASES:
   #    1) Design_name not set

   set errMsg "Please set the variable <design_name> to a non-empty value."
   set nRet 1

} elseif { ${cur_design} ne "" && ${list_cells} eq "" } {
   # USE CASES:
   #    2): Current design opened AND is empty AND names same.
   #    3): Current design opened AND is empty AND names diff; design_name NOT in project.
   #    4): Current design opened AND is empty AND names diff; design_name exists in project.

   if { $cur_design ne $design_name } {
      common::send_msg_id "BD_TCL-001" "INFO" "Changing value of <design_name> from <$design_name> to <$cur_design> since current design is empty."
      set design_name [get_property NAME $cur_design]
   }
   common::send_msg_id "BD_TCL-002" "INFO" "Constructing design in IPI design <$cur_design>..."

} elseif { ${cur_design} ne "" && $list_cells ne "" && $cur_design eq $design_name } {
   # USE CASES:
   #    5) Current design opened AND has components AND same names.

   set errMsg "Design <$design_name> already exists in your project, please set the variable <design_name> to another value."
   set nRet 1
} elseif { [get_files -quiet ${design_name}.bd] ne "" } {
   # USE CASES: 
   #    6) Current opened design, has components, but diff names, design_name exists in project.
   #    7) No opened design, design_name exists in project.

   set errMsg "Design <$design_name> already exists in your project, please set the variable <design_name> to another value."
   set nRet 2

} else {
   # USE CASES:
   #    8) No opened design, design_name not in project.
   #    9) Current opened design, has components, but diff names, design_name not in project.

   common::send_msg_id "BD_TCL-003" "INFO" "Currently there is no design <$design_name> in project, so creating one..."

   create_bd_design $design_name

   common::send_msg_id "BD_TCL-004" "INFO" "Making design <$design_name> as current_bd_design."
   current_bd_design $design_name

}

common::send_msg_id "BD_TCL-005" "INFO" "Currently the variable <design_name> is equal to \"$design_name\"."

if { $nRet != 0 } {
   catch {common::send_msg_id "BD_TCL-114" "ERROR" $errMsg}
   return $nRet
}

set bCheckIPsPassed 1
##################################################################
# CHECK IPs
##################################################################
set bCheckIPs 1
if { $bCheckIPs == 1 } {
   set list_check_ips "\ 
xilinx.com:ip:axi_protocol_converter:2.1\
xilinx.com:ip:system_ila:1.1\
xilinx.com:ip:util_vector_logic:2.0\
"

   set list_ips_missing ""
   common::send_msg_id "BD_TCL-006" "INFO" "Checking if the following IPs exist in the project's IP catalog: $list_check_ips ."

   foreach ip_vlnv $list_check_ips {
      set ip_obj [get_ipdefs -all $ip_vlnv]
      if { $ip_obj eq "" } {
         lappend list_ips_missing $ip_vlnv
      }
   }

   if { $list_ips_missing ne "" } {
      catch {common::send_msg_id "BD_TCL-115" "ERROR" "The following IPs are not found in the IP Catalog:\n  $list_ips_missing\n\nResolution: Please add the repository containing the IP(s) to the project." }
      set bCheckIPsPassed 0
   }

}

##################################################################
# CHECK Modules
##################################################################
set bCheckModules 1
if { $bCheckModules == 1 } {
   set list_check_mods "\ 
NutShellSoC\
AXI4VGA\
"

   set list_mods_missing ""
   common::send_msg_id "BD_TCL-006" "INFO" "Checking if the following modules exist in the project's sources: $list_check_mods ."

   foreach mod_vlnv $list_check_mods {
      if { [can_resolve_reference $mod_vlnv] == 0 } {
         lappend list_mods_missing $mod_vlnv
      }
   }

   if { $list_mods_missing ne "" } {
      catch {common::send_msg_id "BD_TCL-115" "ERROR" "The following module(s) are not found in the project: $list_mods_missing" }
      common::send_msg_id "BD_TCL-008" "INFO" "Please add source files for the missing module(s) above."
      set bCheckIPsPassed 0
   }
}

if { $bCheckIPsPassed != 1 } {
  common::send_msg_id "BD_TCL-1003" "WARNING" "Will not continue with creation of design due to the error(s) above."
  return 3
}

##################################################################
# DESIGN PROCs
##################################################################


# Hierarchical cell: hier_devices
proc create_hier_cell_hier_devices { parentCell nameHier } {

  variable script_folder

  if { $parentCell eq "" || $nameHier eq "" } {
     catch {common::send_msg_id "BD_TCL-102" "ERROR" "create_hier_cell_hier_devices() - Empty argument(s)!"}
     return
  }

  # Get object for parentCell
  set parentObj [get_bd_cells $parentCell]
  if { $parentObj == "" } {
     catch {common::send_msg_id "BD_TCL-100" "ERROR" "Unable to find parent cell <$parentCell>!"}
     return
  }

  # Make sure parentObj is hier blk
  set parentType [get_property TYPE $parentObj]
  if { $parentType ne "hier" } {
     catch {common::send_msg_id "BD_TCL-101" "ERROR" "Parent <$parentObj> has TYPE = <$parentType>. Expected to be <hier>."}
     return
  }

  # Save current instance; Restore later
  set oldCurInst [current_bd_instance .]

  # Set parent object as current
  current_bd_instance $parentObj

  # Create cell and set as current instance
  set hier_obj [create_bd_cell -type hier $nameHier]
  current_bd_instance $hier_obj

  # Create interface pins
  create_bd_intf_pin -mode Master -vlnv xilinx.com:interface:aximm_rtl:1.0 M_MMIO

  create_bd_intf_pin -mode Slave -vlnv xilinx.com:interface:aximm_rtl:1.0 S00_AXI


  # Create pins
  create_bd_pin -dir I -type clk coreclk
  create_bd_pin -dir I -from 0 -to 0 -type rst corerstn
  create_bd_pin -dir O io_hsync
  create_bd_pin -dir O -from 23 -to 0 io_rgb
  create_bd_pin -dir O io_videovalid
  create_bd_pin -dir O io_vsync
  create_bd_pin -dir I -type clk uncoreclk
  create_bd_pin -dir I -type rst uncorerstn

  # Create instance: AXI4VGA_0, and set properties
  set block_name AXI4VGA
  set block_cell_name AXI4VGA_0
  if { [catch {set AXI4VGA_0 [create_bd_cell -type module -reference $block_name $block_cell_name] } errmsg] } {
     catch {common::send_msg_id "BD_TCL-105" "ERROR" "Unable to add referenced block <$block_name>. Please add the files for ${block_name}'s definition into the project."}
     return 1
   } elseif { $AXI4VGA_0 eq "" } {
     catch {common::send_msg_id "BD_TCL-106" "ERROR" "Unable to referenced block <$block_name>. Please add the files for ${block_name}'s definition into the project."}
     return 1
   }
  
  # Create instance: axi_interconnect_0, and set properties
  set axi_interconnect_0 [ create_bd_cell -type ip -vlnv xilinx.com:ip:axi_interconnect:2.1 axi_interconnect_0 ]
  set_property -dict [ list \
   CONFIG.NUM_MI {1} \
   CONFIG.NUM_SI {1} \
 ] $axi_interconnect_0

  # Create instance: util_vector_logic_0, and set properties
  set util_vector_logic_0 [ create_bd_cell -type ip -vlnv xilinx.com:ip:util_vector_logic:2.0 util_vector_logic_0 ]
  set_property -dict [ list \
   CONFIG.C_OPERATION {not} \
   CONFIG.C_SIZE {1} \
   CONFIG.LOGO_FILE {data/sym_notgate.png} \
 ] $util_vector_logic_0

  # Create interface connections
  connect_bd_intf_net -intf_net Conn2 [get_bd_intf_pins S00_AXI] [get_bd_intf_pins axi_interconnect_0/S00_AXI]
  connect_bd_intf_net -intf_net axi_interconnect_0_M00_AXI [get_bd_intf_pins M_MMIO] [get_bd_intf_pins axi_interconnect_0/M00_AXI]

  # Create port connections
  connect_bd_net -net AXI4VGA_0_io_vga_hsync [get_bd_pins io_hsync] [get_bd_pins AXI4VGA_0/io_vga_hsync]
  connect_bd_net -net AXI4VGA_0_io_vga_rgb [get_bd_pins io_rgb] [get_bd_pins AXI4VGA_0/io_vga_rgb]
  connect_bd_net -net AXI4VGA_0_io_vga_valid [get_bd_pins io_videovalid] [get_bd_pins AXI4VGA_0/io_vga_valid]
  connect_bd_net -net AXI4VGA_0_io_vga_vsync [get_bd_pins io_vsync] [get_bd_pins AXI4VGA_0/io_vga_vsync]
  connect_bd_net -net M01_ACLK_1 [get_bd_pins uncoreclk] [get_bd_pins AXI4VGA_0/clock] [get_bd_pins axi_interconnect_0/ACLK] [get_bd_pins axi_interconnect_0/M00_ACLK]
  connect_bd_net -net M01_ARESETN_1 [get_bd_pins uncorerstn] [get_bd_pins axi_interconnect_0/ARESETN] [get_bd_pins axi_interconnect_0/M00_ARESETN] [get_bd_pins util_vector_logic_0/Op1]
  connect_bd_net -net coreclk_1 [get_bd_pins coreclk] [get_bd_pins axi_interconnect_0/S00_ACLK]
  connect_bd_net -net uncorerstn_1 [get_bd_pins corerstn] [get_bd_pins axi_interconnect_0/S00_ARESETN]
  connect_bd_net -net util_vector_logic_0_Res [get_bd_pins AXI4VGA_0/reset] [get_bd_pins util_vector_logic_0/Res]

  # Restore current instance
  current_bd_instance $oldCurInst
}


# Procedure to create entire design; Provide argument to make
# procedure reusable. If parentCell is "", will use root.
proc create_root_design { parentCell } {

  variable script_folder
  variable design_name

  if { $parentCell eq "" } {
     set parentCell [get_bd_cells /]
  }

  # Get object for parentCell
  set parentObj [get_bd_cells $parentCell]
  if { $parentObj == "" } {
     catch {common::send_msg_id "BD_TCL-100" "ERROR" "Unable to find parent cell <$parentCell>!"}
     return
  }

  # Make sure parentObj is hier blk
  set parentType [get_property TYPE $parentObj]
  if { $parentType ne "hier" } {
     catch {common::send_msg_id "BD_TCL-101" "ERROR" "Parent <$parentObj> has TYPE = <$parentType>. Expected to be <hier>."}
     return
  }

  # Save current instance; Restore later
  set oldCurInst [current_bd_instance .]

  # Set parent object as current
  current_bd_instance $parentObj


  # Create interface ports
  set AXI_DMA [ create_bd_intf_port -mode Slave -vlnv xilinx.com:interface:aximm_rtl:1.0 AXI_DMA ]
  set_property -dict [ list \
   CONFIG.ADDR_WIDTH {32} \
   CONFIG.ARUSER_WIDTH {0} \
   CONFIG.AWUSER_WIDTH {0} \
   CONFIG.BUSER_WIDTH {0} \
   CONFIG.CLK_DOMAIN {/clk_wiz_0_clk_out1} \
   CONFIG.DATA_WIDTH {64} \
   CONFIG.HAS_BRESP {1} \
   CONFIG.HAS_BURST {1} \
   CONFIG.HAS_CACHE {1} \
   CONFIG.HAS_LOCK {1} \
   CONFIG.HAS_PROT {1} \
   CONFIG.HAS_QOS {1} \
   CONFIG.HAS_REGION {1} \
   CONFIG.HAS_RRESP {1} \
   CONFIG.HAS_WSTRB {1} \
   CONFIG.ID_WIDTH {16} \
   CONFIG.MAX_BURST_LENGTH {256} \
   CONFIG.NUM_READ_OUTSTANDING {2} \
   CONFIG.NUM_READ_THREADS {1} \
   CONFIG.NUM_WRITE_OUTSTANDING {2} \
   CONFIG.NUM_WRITE_THREADS {1} \
   CONFIG.PHASE {0.0} \
   CONFIG.PROTOCOL {AXI4} \
   CONFIG.READ_WRITE_MODE {READ_WRITE} \
   CONFIG.RUSER_BITS_PER_BYTE {0} \
   CONFIG.RUSER_WIDTH {0} \
   CONFIG.SUPPORTS_NARROW_BURST {1} \
   CONFIG.WUSER_BITS_PER_BYTE {0} \
   CONFIG.WUSER_WIDTH {0} \
   ] $AXI_DMA

  set AXI_MEM [ create_bd_intf_port -mode Master -vlnv xilinx.com:interface:aximm_rtl:1.0 AXI_MEM ]
  set_property -dict [ list \
   CONFIG.ADDR_WIDTH {32} \
   CONFIG.CLK_DOMAIN {/clk_wiz_0_clk_out1} \
   CONFIG.DATA_WIDTH {64} \
   CONFIG.NUM_READ_OUTSTANDING {2} \
   CONFIG.NUM_WRITE_OUTSTANDING {2} \
   CONFIG.PHASE {0.0} \
   CONFIG.PROTOCOL {AXI4} \
   ] $AXI_MEM

  set AXI_MMIO [ create_bd_intf_port -mode Master -vlnv xilinx.com:interface:aximm_rtl:1.0 AXI_MMIO ]
  set_property -dict [ list \
   CONFIG.ADDR_WIDTH {32} \
   CONFIG.CLK_DOMAIN {/clk_wiz_0_clk_out1} \
   CONFIG.DATA_WIDTH {64} \
   CONFIG.NUM_READ_OUTSTANDING {2} \
   CONFIG.NUM_WRITE_OUTSTANDING {2} \
   CONFIG.PHASE {0.0} \
   CONFIG.PROTOCOL {AXI4} \
   ] $AXI_MMIO


  # Create ports
  set VGA_hsync [ create_bd_port -dir O VGA_hsync ]
  set VGA_rgb [ create_bd_port -dir O -from 23 -to 0 VGA_rgb ]
  set VGA_videovalid [ create_bd_port -dir O VGA_videovalid ]
  set VGA_vsync [ create_bd_port -dir O VGA_vsync ]
  set coreclk [ create_bd_port -dir I -type clk coreclk ]
  set_property -dict [ list \
   CONFIG.ASSOCIATED_RESET {corerstn} \
   CONFIG.FREQ_HZ {100000000} \
 ] $coreclk
  set corerstn [ create_bd_port -dir I -type data corerstn ]
  set intrs [ create_bd_port -dir I -from 4 -to 0 intrs ]
  set uncoreclk [ create_bd_port -dir I -type clk uncoreclk ]
  set_property -dict [ list \
   CONFIG.ASSOCIATED_RESET {uncorerstn} \
   CONFIG.CLK_DOMAIN {/clk_wiz_0_clk_out1} \
   CONFIG.FREQ_HZ {100000000} \
   CONFIG.PHASE {0.0} \
 ] $uncoreclk
  set uncorerstn [ create_bd_port -dir I -type rst uncorerstn ]

  # Create instance: NutShellSoC_0, and set properties
  set block_name NutShellSoC
  set block_cell_name NutShellSoC_0
  if { [catch {set NutShellSoC_0 [create_bd_cell -type module -reference $block_name $block_cell_name] } errmsg] } {
     catch {common::send_msg_id "BD_TCL-105" "ERROR" "Unable to add referenced block <$block_name>. Please add the files for ${block_name}'s definition into the project."}
     return 1
   } elseif { $NutShellSoC_0 eq "" } {
     catch {common::send_msg_id "BD_TCL-106" "ERROR" "Unable to referenced block <$block_name>. Please add the files for ${block_name}'s definition into the project."}
     return 1
   }
  
  # Create instance: axi_interconnect_0, and set properties
  set axi_interconnect_0 [ create_bd_cell -type ip -vlnv xilinx.com:ip:axi_interconnect:2.1 axi_interconnect_0 ]
  set_property -dict [ list \
   CONFIG.NUM_MI {1} \
 ] $axi_interconnect_0

  # Create instance: axi_interconnect_2, and set properties
  set axi_interconnect_2 [ create_bd_cell -type ip -vlnv xilinx.com:ip:axi_interconnect:2.1 axi_interconnect_2 ]
  set_property -dict [ list \
   CONFIG.NUM_MI {1} \
   CONFIG.NUM_SI {1} \
 ] $axi_interconnect_2

  # Create instance: axi_protocol_convert_0, and set properties
  set axi_protocol_convert_0 [ create_bd_cell -type ip -vlnv xilinx.com:ip:axi_protocol_converter:2.1 axi_protocol_convert_0 ]
  set_property -dict [ list \
   CONFIG.MI_PROTOCOL {AXI4LITE} \
   CONFIG.SI_PROTOCOL {AXI4} \
 ] $axi_protocol_convert_0

  # Create instance: axi_protocol_convert_1, and set properties
  set axi_protocol_convert_1 [ create_bd_cell -type ip -vlnv xilinx.com:ip:axi_protocol_converter:2.1 axi_protocol_convert_1 ]

  # Create instance: hier_devices
  create_hier_cell_hier_devices [current_bd_instance .] hier_devices

  # Create instance: system_ila_0, and set properties
  set system_ila_0 [ create_bd_cell -type ip -vlnv xilinx.com:ip:system_ila:1.1 system_ila_0 ]
  set_property -dict [ list \
   CONFIG.C_BRAM_CNT {0.5} \
   CONFIG.C_MON_TYPE {NATIVE} \
   CONFIG.C_NUM_MONITOR_SLOTS {1} \
   CONFIG.C_NUM_OF_PROBES {6} \
 ] $system_ila_0

  # Create instance: util_vector_logic_0, and set properties
  set util_vector_logic_0 [ create_bd_cell -type ip -vlnv xilinx.com:ip:util_vector_logic:2.0 util_vector_logic_0 ]
  set_property -dict [ list \
   CONFIG.C_OPERATION {not} \
   CONFIG.C_SIZE {1} \
   CONFIG.LOGO_FILE {data/sym_notgate.png} \
 ] $util_vector_logic_0

  # Create interface connections
  connect_bd_intf_net -intf_net M_AXI_DMA_1 [get_bd_intf_ports AXI_DMA] [get_bd_intf_pins axi_protocol_convert_0/S_AXI]
  connect_bd_intf_net -intf_net NutShellSoC_0_io_mem [get_bd_intf_pins NutShellSoC_0/io_mem] [get_bd_intf_pins axi_interconnect_2/S00_AXI]
  connect_bd_intf_net -intf_net NutShellSoC_0_io_mmio [get_bd_intf_pins NutShellSoC_0/io_mmio] [get_bd_intf_pins hier_devices/S00_AXI]
  connect_bd_intf_net -intf_net S00_AXI_2 [get_bd_intf_pins axi_interconnect_0/S00_AXI] [get_bd_intf_pins axi_protocol_convert_1/M_AXI]
  connect_bd_intf_net -intf_net axi_clock_converter_0_M_AXI [get_bd_intf_ports AXI_MEM] [get_bd_intf_pins axi_interconnect_2/M00_AXI]
  connect_bd_intf_net -intf_net axi_interconnect_0_M00_AXI [get_bd_intf_pins NutShellSoC_0/io_frontend] [get_bd_intf_pins axi_interconnect_0/M00_AXI]
  connect_bd_intf_net -intf_net axi_protocol_convert_0_M_AXI [get_bd_intf_pins axi_protocol_convert_0/M_AXI] [get_bd_intf_pins axi_protocol_convert_1/S_AXI]
  connect_bd_intf_net -intf_net hier_devices_M_MMIO [get_bd_intf_ports AXI_MMIO] [get_bd_intf_pins hier_devices/M_MMIO]

  # Create port connections
  connect_bd_net -net NutShellSoC_0_io_ila_InstrCnt [get_bd_pins NutShellSoC_0/io_ila_InstrCnt] [get_bd_pins system_ila_0/probe5]
  connect_bd_net -net NutShellSoC_0_io_ila_WBUpc [get_bd_pins NutShellSoC_0/io_ila_WBUpc] [get_bd_pins system_ila_0/probe0]
  connect_bd_net -net NutShellSoC_0_io_ila_WBUrfData [get_bd_pins NutShellSoC_0/io_ila_WBUrfData] [get_bd_pins system_ila_0/probe4]
  connect_bd_net -net NutShellSoC_0_io_ila_WBUrfDest [get_bd_pins NutShellSoC_0/io_ila_WBUrfDest] [get_bd_pins system_ila_0/probe3]
  connect_bd_net -net NutShellSoC_0_io_ila_WBUrfWen [get_bd_pins NutShellSoC_0/io_ila_WBUrfWen] [get_bd_pins system_ila_0/probe2]
  connect_bd_net -net NutShellSoC_0_io_ila_WBUvalid [get_bd_pins NutShellSoC_0/io_ila_WBUvalid] [get_bd_pins system_ila_0/probe1]
  connect_bd_net -net c_shift_ram_0_Q [get_bd_ports corerstn] [get_bd_pins axi_interconnect_0/M00_ARESETN] [get_bd_pins axi_interconnect_2/ARESETN] [get_bd_pins axi_interconnect_2/S00_ARESETN] [get_bd_pins hier_devices/corerstn] [get_bd_pins util_vector_logic_0/Op1]
  connect_bd_net -net coreclk_1 [get_bd_ports coreclk] [get_bd_pins NutShellSoC_0/clock] [get_bd_pins axi_interconnect_0/M00_ACLK] [get_bd_pins axi_interconnect_2/ACLK] [get_bd_pins axi_interconnect_2/S00_ACLK] [get_bd_pins hier_devices/coreclk] [get_bd_pins system_ila_0/clk]
  connect_bd_net -net hier_devices_io_hsync [get_bd_ports VGA_hsync] [get_bd_pins hier_devices/io_hsync]
  connect_bd_net -net hier_devices_io_rgb [get_bd_ports VGA_rgb] [get_bd_pins hier_devices/io_rgb]
  connect_bd_net -net hier_devices_io_videovalid [get_bd_ports VGA_videovalid] [get_bd_pins hier_devices/io_videovalid]
  connect_bd_net -net hier_devices_io_vsync [get_bd_ports VGA_vsync] [get_bd_pins hier_devices/io_vsync]
  connect_bd_net -net intrs_1 [get_bd_ports intrs] [get_bd_pins NutShellSoC_0/io_meip]
  connect_bd_net -net uncoreclk_1 [get_bd_ports uncoreclk] [get_bd_pins axi_interconnect_0/ACLK] [get_bd_pins axi_interconnect_0/S00_ACLK] [get_bd_pins axi_interconnect_2/M00_ACLK] [get_bd_pins axi_protocol_convert_0/aclk] [get_bd_pins axi_protocol_convert_1/aclk] [get_bd_pins hier_devices/uncoreclk]
  connect_bd_net -net uncorerstn_2 [get_bd_ports uncorerstn] [get_bd_pins axi_interconnect_0/ARESETN] [get_bd_pins axi_interconnect_0/S00_ARESETN] [get_bd_pins axi_interconnect_2/M00_ARESETN] [get_bd_pins axi_protocol_convert_0/aresetn] [get_bd_pins axi_protocol_convert_1/aresetn] [get_bd_pins hier_devices/uncorerstn]
  connect_bd_net -net util_vector_logic_0_Res [get_bd_pins NutShellSoC_0/reset] [get_bd_pins util_vector_logic_0/Res]

  # Create address segments
  create_bd_addr_seg -range 0x80000000 -offset 0x80000000 [get_bd_addr_spaces NutShellSoC_0/io_mem] [get_bd_addr_segs AXI_MEM/Reg] SEG_AXI_MEM_Reg
  create_bd_addr_seg -range 0x40000000 -offset 0x40000000 [get_bd_addr_spaces NutShellSoC_0/io_mmio] [get_bd_addr_segs AXI_MMIO/Reg] SEG_AXI_MMIO_Reg
  create_bd_addr_seg -range 0x000100000000 -offset 0x00000000 [get_bd_addr_spaces AXI_DMA] [get_bd_addr_segs NutShellSoC_0/io_frontend/reg0] SEG_NutShellSoC_0_reg0


  # Restore current instance
  current_bd_instance $oldCurInst

  validate_bd_design
  save_bd_design
}
# End of create_root_design()


##################################################################
# MAIN FLOW
##################################################################

create_root_design ""


