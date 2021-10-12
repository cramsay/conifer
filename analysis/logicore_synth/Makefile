IMPL_OOC = out_of_context/post_route.dcp
IMPL_LOOP = loopback/post_route.dcp

all: $(IMPL_OOC)

$(IMPL_OOC):
	cd out_of_context; vivado -mode batch -source synth.tcl

$(IMPL_LOOP):
	cd loopback; vivado -mode batch -source synth.tcl

clean:
	rm -rf out_of_context/hbs out_of_context/hd_visual out_of_context/ip* out_of_context/*.jou out_of_context/*.log out_of_context/*.dcp out_of_context/*.rpt
	rm -rf loopback/hbs loopback/hd_visual loopback/ip* loopback/*.jou loopback/*.log loopback/*.dcp loopback/*.rpt loopback/prj*

shrink: # Remove intermediate files, keeping final results
	rm -rf out_of_context/hbs out_of_context/hd_visual out_of_context/ip* out_of_context/*.jou out_of_context/*.log
	rm -rf loopback/hbs loopback/hd_visual loopback/ip* loopback/*.jou loopback/*.log

impl_ooc: $(IMPL_OOC)

impl_loop: $(IMPL_LOOP)
