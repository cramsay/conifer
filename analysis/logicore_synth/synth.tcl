# Create temporary project to build IP from
set_part "xczu28dr-ffvg1517-2-e"
create_project ip ./ip -part xczu28dr-ffvg1517-2-e -ip -force
set_property board_part xilinx.com:zcu111:part0:1.2 [current_project]
set_property target_language VHDL [current_project]
set_property target_simulator XSim [current_project]

# Create FIR IP
create_ip -name fir_compiler -vendor xilinx.com -library ip -version 7.2 -module_name fir -dir ip

set_property -dict [list \
  CONFIG.Component_Name {fir} \
  CONFIG.CoefficientSource {COE_File} \
  CONFIG.Coefficient_File {../../weights.coe} \
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
] [get_ips fir]

generate_target {instantiation_template} [get_files ./ip/fir/fir.xci]
generate_target all [get_files  ./ip/fir/fir.xci]

close_project

# Set up out-of-project sources

read_ip ./ip/fir/fir.xci
#add_files { ./ip/fir/synth/fir.vhd }
set_property top fir [current_fileset]
update_compile_order -fileset [current_fileset]

add_files -fileset constrs_1 synth.xdc
set_property target_constrs_file synth.xdc [current_fileset -constrset]

# Synthesise out of context
synth_design -mode out_of_context  -part xczu28dr-ffvg1517-2-e
report_utilization -file ./post_synth_util.rpt
opt_design -directive  Explore
power_opt_design

# Place & route
place_design -directive  Explore
phys_opt_design -directive  Explore
route_design -directive  Explore

# Report
report_timing_summary -delay_type min_max -report_unconstrained -check_timing_verbose -max_paths 10 -input_pins -routable_nets -name timing_1 -file ./post_route_timing.rpt
report_utilization -file ./post_route_util.rpt
write_checkpoint ./post_route.dcp -force
