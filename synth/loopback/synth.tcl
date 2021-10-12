# Create project
create_project prj ./ -force -part xczu28dr-ffvg1517-2-e

# Set project properties
set obj [get_projects prj]
set_property -name "default_lib" -value "xil_defaultlib" -objects $obj
set_property -name "ip_cache_permissions" -value "read write" -objects $obj
set_property -name "target_language" -value "VHDL" -objects $obj
set_property strategy "Performance_Explore" [get_runs impl_1]

# Add sources
add_files { ../vhdl/ }
add_files -fileset constrs_1 synth.xdc
update_compile_order -fileset sources_1

# Make an IP block from our filter sources
ipx::package_project -root_dir ./ip -vendor user.org -library user -taxonomy /UserIP -import_files -set_current false
#ipx::unload_core ./ip/component.xml
ipx::edit_ip_in_project -upgrade true -name tmp_edit_project -directory ./ip ./ip/component.xml
update_compile_order -fileset sources_1
set_property core_revision 1 [ipx::current_core]
ipx::create_xgui_files [ipx::current_core]
ipx::update_checksums [ipx::current_core]
ipx::save_core [ipx::current_core]
close_project -delete

# Include IP in main project
set_property  ip_repo_paths  ./ip [current_project]
update_ip_catalog
update_compile_order -fileset sources_1

# Generate block design
source ./block_design.tcl
update_compile_order -fileset sources_1
make_wrapper -files [get_files ./prj.srcs/sources_1/bd/design_1/design_1.bd] -top
add_files -norecurse ./prj.srcs/sources_1/bd/design_1/hdl/design_1_wrapper.vhd
set_property top design_1_wrapper [current_fileset]
update_compile_order -fileset sources_1

# Build
launch_runs impl_1 -jobs 4
wait_on_run impl_1

# Report
open_run impl_1
report_timing_summary -delay_type min_max -report_unconstrained -check_timing_verbose -max_paths 10 -input_pins -routable_nets -name timing_1 -file ./post_route_timing.rpt
report_utilization -file ./post_route_util.rpt
write_checkpoint ./post_route.dcp -force

close_project -delete
