set device xc7z045ffg900-2
set board xilinx.com:zc706:part0:1.4

set script_dir  [file dirname [info script]]
puts $script_dir

# Add files for system top
set src_files [list \
  "[file normalize "${script_dir}/../../../src/test/vsrc/monitor.v"]" \
]

# Add files for constraint
#set xdc_files [list \
#  "[file normalize "${script_dir}/constr/constr.xdc"]" \
#  "[file normalize "${script_dir}/constr/vga.xdc"]" \
#]

source ${script_dir}/../common.tcl
