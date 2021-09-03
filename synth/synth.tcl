# Add sources
add_files { vhdl/ }
update_compile_order -fileset sources_1

add_files -fileset constrs_1 synth.xdc
set_property target_constrs_file synth.xdc [current_fileset -constrset]

# Synthesise out of context
synth_design -mode out_of_context  -part xczu28dr-ffvg1517-2-e -directive AreaOptimized_medium
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

