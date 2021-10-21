# Define our FIR IP
proc createSSR {} {
  set fir [ create_bd_cell -type ip -vlnv xilinx.com:ip:fir_compiler:7.2 fir ]
  set_property -dict [list \
    CONFIG.Component_Name {fir} \
    CONFIG.CoefficientSource {COE_File} \
    CONFIG.Coefficient_File {../../../../../weights.coe} \
    CONFIG.Sample_Frequency {4000} \
    CONFIG.Clock_Frequency {500} \
    CONFIG.Coefficient_Width {16} \
    CONFIG.Data_Width {16} \
    CONFIG.S_DATA_Has_FIFO {false} \
    CONFIG.Has_ARESETn {true} \
    CONFIG.Coefficient_Sets {1} \
    CONFIG.Clock_Frequency {500} \
    CONFIG.Coefficient_Sign {Signed} \
    CONFIG.Quantization {Integer_Coefficients} \
    CONFIG.Coefficient_Fractional_Bits {0} \
    CONFIG.Coefficient_Structure {Inferred} \
    CONFIG.Data_Width {16} \
    CONFIG.Data_Fractional_Bits {0} \
    CONFIG.Output_Rounding_Mode {Full_Precision} \
    CONFIG.Filter_Architecture {Systolic_Multiply_Accumulate} \
    CONFIG.Filter_Type {Single_Rate} \

  ] $fir
  file copy ../weights.coe ./prj.srcs/weights.coe
}

# Create main project
create_project prj ./ -force -part xczu28dr-ffvg1517-2-e

# Set project properties
set obj [get_projects prj]
set_property -name "default_lib" -value "xil_defaultlib" -objects $obj
set_property -name "ip_cache_permissions" -value "read write" -objects $obj
set_property -name "target_language" -value "VHDL" -objects $obj
set_property strategy "Performance_Explore" [get_runs impl_1]
add_files -fileset constrs_1 synth.xdc

# Generate block design
source ./block_design.tcl
update_compile_order -fileset sources_1
make_wrapper -files [get_files ./prj.srcs/sources_1/bd/design_1/design_1.bd] -top
add_files -norecurse ./prj.srcs/sources_1/bd/design_1/hdl/design_1_wrapper.vhd
set_property top design_1_wrapper [current_fileset]
generate_target all [get_ips fir]
update_compile_order -fileset sources_1

# Build
launch_runs impl_1 -jobs 4
wait_on_run impl_1

# Report
open_run impl_1
report_timing_summary -delay_type min_max -report_unconstrained -check_timing_verbose -max_paths 10 -input_pins -routable_nets -name timing_1 -file ./post_route_timing.rpt
report_utilization -file ./post_route_util.rpt
write_checkpoint ./post_route.dcp -force

close_project
