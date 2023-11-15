
################################################################
# This is a generated script based on design: system_top
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
# source system_top_script.tcl


# The design that will be created by this Tcl script contains the following 
# module references:
# WuKong

# Please add the sources of those modules before sourcing this Tcl script.

# If there is no project opened, this script will create a
# project, but make sure you do not have an existing project
# <./myproj/project_1.xpr> in the current working folder.

set list_projs [get_projects -quiet]
puts "i   am  board  outside"
puts $list_projs



# CHANGE DESIGN NAME HERE
variable design_name
set design_name system_top

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
xilinx.com:ip:processing_system7:5.5\
xilinx.com:ip:xlconcat:2.1\
xilinx.com:ip:axi_gpio:2.0\
xilinx.com:ip:clk_wiz:6.0\
xilinx.com:ip:proc_sys_reset:5.0\
xilinx.com:ip:axi_protocol_converter:2.1\
xilinx.com:ip:c_shift_ram:12.0\
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
WuKong\
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


# Hierarchical cell: rv_system
proc create_hier_cell_rv_system { parentCell nameHier } {

  variable script_folder

  if { $parentCell eq "" || $nameHier eq "" } {
     catch {common::send_msg_id "BD_TCL-102" "ERROR" "create_hier_cell_rv_system() - Empty argument(s)!"}
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

  create_bd_intf_pin -mode Master -vlnv xilinx.com:interface:aximm_rtl:1.0 io_master

  create_bd_intf_pin -mode Master -vlnv xilinx.com:interface:aximm_rtl:1.0 io_mmio


  # Create pins
  create_bd_pin -dir I -type clk coreclk
  create_bd_pin -dir I -from 0 -to 0 -type data corerstn
  create_bd_pin -dir I -from 2 -to 0 io_meip

  # Create instance: WuKong_0, and set properties
  set block_name WuKong
  set block_cell_name WuKong_0
  if { [catch {set WuKong_0 [create_bd_cell -type module -reference $block_name $block_cell_name] } errmsg] } {
     catch {common::send_msg_id "BD_TCL-105" "ERROR" "Unable to add referenced block <$block_name>. Please add the files for ${block_name}'s definition into the project."}
     return 1
   } elseif { $WuKong_0 eq "" } {
     catch {common::send_msg_id "BD_TCL-106" "ERROR" "Unable to referenced block <$block_name>. Please add the files for ${block_name}'s definition into the project."}
     return 1
   }
  
  # Create instance: axi_protocol_convert_0, and set properties
  set axi_protocol_convert_0 [ create_bd_cell -type ip -vlnv xilinx.com:ip:axi_protocol_converter:2.1 axi_protocol_convert_0 ]
  set_property -dict [ list \
   CONFIG.MI_PROTOCOL {AXI4} \
   CONFIG.SI_PROTOCOL {AXI4LITE} \
   CONFIG.TRANSLATION_MODE {2} \
 ] $axi_protocol_convert_0

  # Create instance: axi_protocol_convert_1, and set properties
  set axi_protocol_convert_1 [ create_bd_cell -type ip -vlnv xilinx.com:ip:axi_protocol_converter:2.1 axi_protocol_convert_1 ]
  set_property -dict [ list \
   CONFIG.MI_PROTOCOL {AXI4LITE} \
   CONFIG.SI_PROTOCOL {AXI4} \
 ] $axi_protocol_convert_1

  # Create instance: c_shift_ram_0, and set properties
  set c_shift_ram_0 [ create_bd_cell -type ip -vlnv xilinx.com:ip:c_shift_ram:12.0 c_shift_ram_0 ]
  set_property -dict [ list \
   CONFIG.AsyncInitVal {0} \
   CONFIG.DefaultData {0} \
   CONFIG.Depth {1} \
   CONFIG.SyncInitVal {0} \
   CONFIG.Width {1} \
 ] $c_shift_ram_0

  # Create instance: c_shift_ram_1, and set properties
  set c_shift_ram_1 [ create_bd_cell -type ip -vlnv xilinx.com:ip:c_shift_ram:12.0 c_shift_ram_1 ]
  set_property -dict [ list \
   CONFIG.AsyncInitVal {0} \
   CONFIG.DefaultData {0} \
   CONFIG.Depth {1} \
   CONFIG.SyncInitVal {0} \
   CONFIG.Width {1} \
 ] $c_shift_ram_1

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
  connect_bd_intf_net -intf_net WuKong_0_io_master [get_bd_intf_pins io_master] [get_bd_intf_pins WuKong_0/io_master]
  connect_bd_intf_net -intf_net WuKong_0_io_mmio [get_bd_intf_pins io_mmio] [get_bd_intf_pins WuKong_0/io_mmio]
  connect_bd_intf_net -intf_net axi_protocol_convert_1_M_AXI [get_bd_intf_pins axi_protocol_convert_0/S_AXI] [get_bd_intf_pins axi_protocol_convert_1/M_AXI]

  # Create port connections
  connect_bd_net -net WuKong_0_io_ila_InstrCnt [get_bd_pins WuKong_0/io_ila_InstrCnt] [get_bd_pins system_ila_0/probe5]
  connect_bd_net -net WuKong_0_io_ila_WBUpc [get_bd_pins WuKong_0/io_ila_WBUpc] [get_bd_pins system_ila_0/probe0]
  connect_bd_net -net WuKong_0_io_ila_WBUrfData [get_bd_pins WuKong_0/io_ila_WBUrfData] [get_bd_pins system_ila_0/probe4]
  connect_bd_net -net WuKong_0_io_ila_WBUrfDest [get_bd_pins WuKong_0/io_ila_WBUrfDest] [get_bd_pins system_ila_0/probe3]
  connect_bd_net -net WuKong_0_io_ila_WBUrfWen [get_bd_pins WuKong_0/io_ila_WBUrfWen] [get_bd_pins system_ila_0/probe2]
  connect_bd_net -net WuKong_0_io_ila_WBUvalid [get_bd_pins WuKong_0/io_ila_WBUvalid] [get_bd_pins system_ila_0/probe1]
  connect_bd_net -net axi_gpio_0_gpio_io_o [get_bd_pins corerstn] [get_bd_pins axi_protocol_convert_0/aresetn] [get_bd_pins axi_protocol_convert_1/aresetn] [get_bd_pins c_shift_ram_1/D]
  connect_bd_net -net c_shift_ram_0_Q [get_bd_pins c_shift_ram_0/Q] [get_bd_pins util_vector_logic_0/Op1]
  connect_bd_net -net c_shift_ram_1_Q [get_bd_pins c_shift_ram_0/D] [get_bd_pins c_shift_ram_1/Q]
  connect_bd_net -net clk_wiz_0_coreclk [get_bd_pins coreclk] [get_bd_pins WuKong_0/clock] [get_bd_pins axi_protocol_convert_0/aclk] [get_bd_pins axi_protocol_convert_1/aclk] [get_bd_pins c_shift_ram_0/CLK] [get_bd_pins c_shift_ram_1/CLK] [get_bd_pins system_ila_0/clk]
  connect_bd_net -net io_meip_1 [get_bd_pins io_meip] [get_bd_pins WuKong_0/io_meip]
  connect_bd_net -net util_vector_logic_0_Res [get_bd_pins WuKong_0/reset] [get_bd_pins util_vector_logic_0/Res]

  # Restore current instance
  current_bd_instance $oldCurInst
}

# Hierarchical cell: hier_clkrst
proc create_hier_cell_hier_clkrst { parentCell nameHier } {

  variable script_folder

  if { $parentCell eq "" || $nameHier eq "" } {
     catch {common::send_msg_id "BD_TCL-102" "ERROR" "create_hier_cell_hier_clkrst() - Empty argument(s)!"}
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
  create_bd_intf_pin -mode Master -vlnv xilinx.com:interface:gpio_rtl:1.0 GPIO2

  create_bd_intf_pin -mode Slave -vlnv xilinx.com:interface:aximm_rtl:1.0 S_AXI


  # Create pins
  create_bd_pin -dir I -type clk clk_in1
  create_bd_pin -dir O -type clk coreclk
  create_bd_pin -dir O -from 0 -to 0 corerstn
  create_bd_pin -dir O -from 0 -to 0 -type rst interconnect_aresetn
  create_bd_pin -dir I -type rst resetn
  create_bd_pin -dir O -type clk uncoreclk

  # Create instance: axi_gpio_0, and set properties
  set axi_gpio_0 [ create_bd_cell -type ip -vlnv xilinx.com:ip:axi_gpio:2.0 axi_gpio_0 ]
  set_property -dict [ list \
   CONFIG.C_GPIO_WIDTH {1} \
   CONFIG.C_GPIO2_WIDTH {2} \
   CONFIG.C_IS_DUAL {1} \
   CONFIG.C_ALL_INPUTS_2 {1} \
   CONFIG.C_ALL_OUTPUTS {1} \
   #CONFIG.GPIO2_BOARD_INTERFACE {Custom} \
 ] $axi_gpio_0
  # Create instance: clk_wiz_0, and set properties
  set clk_wiz_0 [ create_bd_cell -type ip -vlnv xilinx.com:ip:clk_wiz:6.0 clk_wiz_0 ]
  set_property -dict [ list \
   CONFIG.CLKIN1_JITTER_PS {100.0} \
   CONFIG.CLKOUT1_JITTER {153.276} \
   CONFIG.CLKOUT1_PHASE_ERROR {105.461} \
   CONFIG.CLKOUT1_REQUESTED_OUT_FREQ {60.000} \
   CONFIG.CLKOUT2_JITTER {137.681} \
   CONFIG.CLKOUT2_PHASE_ERROR {105.461} \
   CONFIG.CLKOUT2_REQUESTED_OUT_FREQ {100.000} \
   CONFIG.CLKOUT2_USED {true} \
   CONFIG.CLKOUT3_JITTER {159.475} \
   CONFIG.CLKOUT3_PHASE_ERROR {105.461} \
   CONFIG.CLKOUT3_REQUESTED_OUT_FREQ {100.000} \
   CONFIG.CLKOUT3_USED {false} \
   CONFIG.CLKOUT4_REQUESTED_OUT_FREQ {100.000} \
   CONFIG.CLKOUT4_USED {false} \
   CONFIG.CLK_OUT1_PORT {coreclk} \
   CONFIG.CLK_OUT2_PORT {uncoreclk} \
   CONFIG.CLK_OUT3_PORT {clk_out3} \
   CONFIG.MMCM_CLKFBOUT_MULT_F {9.000} \
   CONFIG.MMCM_CLKIN1_PERIOD {10.000} \
   CONFIG.MMCM_CLKIN2_PERIOD {10.000} \
   CONFIG.MMCM_CLKOUT0_DIVIDE_F {15.000} \
   CONFIG.MMCM_CLKOUT1_DIVIDE {9} \
   CONFIG.MMCM_CLKOUT2_DIVIDE {1} \
   CONFIG.MMCM_CLKOUT3_DIVIDE {1} \
   CONFIG.MMCM_DIVCLK_DIVIDE {1} \
   CONFIG.NUM_OUT_CLKS {2} \
   CONFIG.RESET_PORT {resetn} \
   CONFIG.RESET_TYPE {ACTIVE_LOW} \
 ] $clk_wiz_0

   puts "yyyyyyyyyyyyyyyyyyyyy"
  # Create instance: uncorerst, and set properties
  set uncorerst [ create_bd_cell -type ip -vlnv xilinx.com:ip:proc_sys_reset:5.0 uncorerst ]

  # Create interface connections
  connect_bd_intf_net -intf_net Conn1 [get_bd_intf_pins GPIO2] [get_bd_intf_pins axi_gpio_0/GPIO2]
  connect_bd_intf_net -intf_net axi_crossbar_0_M00_AXI [get_bd_intf_pins S_AXI] [get_bd_intf_pins axi_gpio_0/S_AXI]

  # Create port connections
  connect_bd_net -net armv7_processing_system_FCLK_RESET0_N [get_bd_pins resetn] [get_bd_pins clk_wiz_0/resetn] [get_bd_pins uncorerst/ext_reset_in]
  connect_bd_net -net axi_gpio_0_gpio_io_o [get_bd_pins corerstn] [get_bd_pins axi_gpio_0/gpio_io_o]
  connect_bd_net -net clk_wiz_0_coreclk [get_bd_pins coreclk] [get_bd_pins clk_wiz_0/coreclk]
  connect_bd_net -net clk_wiz_0_locked [get_bd_pins clk_wiz_0/locked] [get_bd_pins uncorerst/dcm_locked]
  connect_bd_net -net clk_wiz_0_uncoreclk [get_bd_pins uncoreclk] [get_bd_pins axi_gpio_0/s_axi_aclk] [get_bd_pins clk_wiz_0/uncoreclk] [get_bd_pins uncorerst/slowest_sync_clk]
  connect_bd_net -net proc_sys_reset_1_interconnect_aresetn [get_bd_pins interconnect_aresetn] [get_bd_pins uncorerst/interconnect_aresetn]
  connect_bd_net -net proc_sys_reset_1_peripheral_aresetn [get_bd_pins axi_gpio_0/s_axi_aresetn] [get_bd_pins uncorerst/peripheral_aresetn]
  connect_bd_net -net processing_system7_0_FCLK_CLK0 [get_bd_pins clk_in1] [get_bd_pins clk_wiz_0/clk_in1]

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
  set DDR [ create_bd_intf_port -mode Master -vlnv xilinx.com:interface:ddrx_rtl:1.0 DDR ]

  set FIXED_IO [ create_bd_intf_port -mode Master -vlnv xilinx.com:display_processing_system7:fixedio_rtl:1.0 FIXED_IO ]

  set SW [ create_bd_intf_port -mode Master -vlnv xilinx.com:interface:gpio_rtl:1.0 SW ]


  # Create ports

  # Create instance: axi_interconnect_0, and set properties
  set axi_interconnect_0 [ create_bd_cell -type ip -vlnv xilinx.com:ip:axi_interconnect:2.1 axi_interconnect_0 ]
  set_property -dict [ list \
   CONFIG.NUM_MI {1} \
   CONFIG.NUM_SI {1} \
 ] $axi_interconnect_0

  # Create instance: axi_interconnect_1, and set properties
  set axi_interconnect_1 [ create_bd_cell -type ip -vlnv xilinx.com:ip:axi_interconnect:2.1 axi_interconnect_1 ]
  set_property -dict [ list \
   CONFIG.NUM_MI {2} \
 ] $axi_interconnect_1

  # Create instance: axi_interconnect_2, and set properties
  set axi_interconnect_2 [ create_bd_cell -type ip -vlnv xilinx.com:ip:axi_interconnect:2.1 axi_interconnect_2 ]
  set_property -dict [ list \
   CONFIG.NUM_MI {1} \
   CONFIG.NUM_SI {1} \
 ] $axi_interconnect_2

  # Create instance: hier_clkrst
  create_hier_cell_hier_clkrst [current_bd_instance .] hier_clkrst
   puts "asdasdasf"

  # Create instance: processing_system7_0, and set properties
  set processing_system7_0 [ create_bd_cell -type ip -vlnv xilinx.com:ip:processing_system7:5.5 processing_system7_0 ]
  apply_bd_automation -rule xilinx.com:bd_rule:processing_system7 -config {apply_board_preset "1"} $processing_system7_0
  set_property -dict [ list \
   CONFIG.PCW_CLK0_FREQ {100000000} \
   CONFIG.PCW_EN_CLK0_PORT {1} \
   CONFIG.PCW_FCLK_CLK0_BUF {TRUE} \
   CONFIG.PCW_FPGA0_PERIPHERAL_FREQMHZ {100} \
   CONFIG.PCW_FPGA_FCLK0_ENABLE {1} \
   CONFIG.PCW_M_AXI_GP0_ENABLE_STATIC_REMAP {1} \
   CONFIG.PCW_M_AXI_GP0_ID_WIDTH {12} \
   CONFIG.PCW_M_AXI_GP0_SUPPORT_NARROW_BURST {0} \
   CONFIG.PCW_M_AXI_GP0_THREAD_ID_WIDTH {6} \
   CONFIG.PCW_M_AXI_GP1_ENABLE_STATIC_REMAP {0} \
   CONFIG.PCW_M_AXI_GP1_ID_WIDTH {12} \
   CONFIG.PCW_M_AXI_GP1_SUPPORT_NARROW_BURST {0} \
   CONFIG.PCW_M_AXI_GP1_THREAD_ID_WIDTH {12} \
   CONFIG.PCW_P2F_ENET0_INTR {1} \
   CONFIG.PCW_P2F_SDIO0_INTR {1} \
   CONFIG.PCW_P2F_UART0_INTR {1} \
   CONFIG.PCW_P2F_USB0_INTR {1} \
   CONFIG.PCW_S_AXI_GP0_ID_WIDTH {6} \
   CONFIG.PCW_S_AXI_GP1_ID_WIDTH {6} \
   CONFIG.PCW_S_AXI_HP0_DATA_WIDTH {64} \
   CONFIG.PCW_S_AXI_HP0_ID_WIDTH {6} \
   CONFIG.PCW_S_AXI_HP1_DATA_WIDTH {64} \
   CONFIG.PCW_S_AXI_HP1_ID_WIDTH {6} \
   CONFIG.PCW_S_AXI_HP2_DATA_WIDTH {64} \
   CONFIG.PCW_S_AXI_HP2_ID_WIDTH {6} \
   CONFIG.PCW_S_AXI_HP3_DATA_WIDTH {64} \
   CONFIG.PCW_S_AXI_HP3_ID_WIDTH {6} \
   CONFIG.PCW_USE_FABRIC_INTERRUPT {1} \
   CONFIG.PCW_USE_M_AXI_GP0 {1} \
   CONFIG.PCW_USE_PS_SLCR_REGISTERS {1} \
   CONFIG.PCW_USE_S_AXI_GP0 {1} \
   CONFIG.PCW_USE_S_AXI_HP0 {1} \
 ] $processing_system7_0

  # Create instance: rv_system
  create_hier_cell_rv_system [current_bd_instance .] rv_system
   puts "i am  rrrrrrrrrrvvvvvvvvv"
  # Create instance: xlconcat_0, and set properties
  set xlconcat_0 [ create_bd_cell -type ip -vlnv xilinx.com:ip:xlconcat:2.1 xlconcat_0 ]
  set_property -dict [ list \
   CONFIG.NUM_PORTS {3} \
 ] $xlconcat_0

  # Create interface connections
  connect_bd_intf_net -intf_net WuKong_0_io_master [get_bd_intf_pins axi_interconnect_0/S00_AXI] [get_bd_intf_pins rv_system/io_master]
  connect_bd_intf_net -intf_net S00_AXI_1 [get_bd_intf_pins axi_interconnect_1/S00_AXI] [get_bd_intf_pins processing_system7_0/M_AXI_GP0]
  connect_bd_intf_net -intf_net S00_AXI_2 [get_bd_intf_pins axi_interconnect_2/S00_AXI] [get_bd_intf_pins rv_system/io_mmio]
  connect_bd_intf_net -intf_net axi_interconnect_0_M00_AXI [get_bd_intf_pins axi_interconnect_0/M00_AXI] [get_bd_intf_pins processing_system7_0/S_AXI_HP0]
  connect_bd_intf_net -intf_net axi_interconnect_1_M00_AXI [get_bd_intf_pins axi_interconnect_1/M00_AXI] [get_bd_intf_pins hier_clkrst/S_AXI]
  connect_bd_intf_net -intf_net axi_interconnect_2_M00_AXI [get_bd_intf_pins axi_interconnect_2/M00_AXI] [get_bd_intf_pins processing_system7_0/S_AXI_GP0]
  connect_bd_intf_net -intf_net hier_clkrst_GPIO2 [get_bd_intf_ports SW] [get_bd_intf_pins hier_clkrst/GPIO2]
  connect_bd_intf_net -intf_net processing_system7_0_DDR [get_bd_intf_ports DDR] [get_bd_intf_pins processing_system7_0/DDR]
  connect_bd_intf_net -intf_net processing_system7_0_FIXED_IO [get_bd_intf_ports FIXED_IO] [get_bd_intf_pins processing_system7_0/FIXED_IO]


  # Create port connections
  connect_bd_net -net axi_gpio_0_gpio_io_o [get_bd_pins axi_interconnect_0/S00_ARESETN] [get_bd_pins axi_interconnect_1/M01_ARESETN] [get_bd_pins axi_interconnect_2/S00_ARESETN] [get_bd_pins hier_clkrst/corerstn] [get_bd_pins rv_system/corerstn]
  connect_bd_net -net clk_wiz_0_coreclk [get_bd_pins axi_interconnect_0/S00_ACLK] [get_bd_pins axi_interconnect_1/M01_ACLK] [get_bd_pins axi_interconnect_2/S00_ACLK] [get_bd_pins hier_clkrst/coreclk] [get_bd_pins rv_system/coreclk]
  connect_bd_net -net clk_wiz_0_uncoreclk [get_bd_pins axi_interconnect_0/ACLK] [get_bd_pins axi_interconnect_0/M00_ACLK] [get_bd_pins axi_interconnect_1/ACLK] [get_bd_pins axi_interconnect_1/M00_ACLK] [get_bd_pins axi_interconnect_1/S00_ACLK] [get_bd_pins axi_interconnect_2/ACLK] [get_bd_pins axi_interconnect_2/M00_ACLK] [get_bd_pins hier_clkrst/uncoreclk] [get_bd_pins processing_system7_0/M_AXI_GP0_ACLK] [get_bd_pins processing_system7_0/S_AXI_GP0_ACLK] [get_bd_pins processing_system7_0/S_AXI_HP0_ACLK]
  connect_bd_net -net proc_sys_reset_1_interconnect_aresetn [get_bd_pins axi_interconnect_0/ARESETN] [get_bd_pins axi_interconnect_0/M00_ARESETN] [get_bd_pins axi_interconnect_1/ARESETN] [get_bd_pins axi_interconnect_1/M00_ARESETN] [get_bd_pins axi_interconnect_1/S00_ARESETN] [get_bd_pins axi_interconnect_2/ARESETN] [get_bd_pins axi_interconnect_2/M00_ARESETN] [get_bd_pins hier_clkrst/interconnect_aresetn]
  connect_bd_net -net processing_system7_0_FCLK_CLK0 [get_bd_pins hier_clkrst/clk_in1] [get_bd_pins processing_system7_0/FCLK_CLK0]
  connect_bd_net -net processing_system7_0_IRQ_P2F_ENET0 [get_bd_pins processing_system7_0/IRQ_P2F_ENET0] [get_bd_pins xlconcat_0/In2]
  connect_bd_net -net processing_system7_0_IRQ_P2F_SDIO0 [get_bd_pins processing_system7_0/IRQ_P2F_SDIO0] [get_bd_pins xlconcat_0/In0]
  connect_bd_net -net processing_system7_0_IRQ_P2F_UART0 [get_bd_pins processing_system7_0/IRQ_P2F_UART0] [get_bd_pins xlconcat_0/In1]
  connect_bd_net -net resetn_1 [get_bd_pins hier_clkrst/resetn] [get_bd_pins processing_system7_0/FCLK_RESET0_N]

  # Create address segments
  puts "iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii"


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


