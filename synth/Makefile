CLASH = clash
CLASH_OPTS = -fconstraint-solver-iterations=40 -fclash-inline-limit=400 -fclash-spec-limit=1000 -fclash-clear
FIR_SRC = Filter.hs
VHDL = vhdl/Main.topEntity/fir_dut.vhdl
VERILOG = verilog/Main/filter/fir_dut.v
IMPL_OOC = out_of_context/post_route.dcp
IMPL_LOOP = loopback/post_route.dcp
SIM = ghdl/sim_impulse.ghw

all: $(IMPL_OOC)

$(VHDL): $(FIR_SRC)
	$(CLASH) $(CLASH_OPTS) --vhdl $(FIR_SRC);
	sed -i -e 's/fir_dut_types.clk_SystemNR/std_logic/' \
	       -e 's/fir_dut_types.rst_SystemNR/std_logic/' $(VHDL);

$(VERILOG): $(FIR_SRC)
	$(CLASH) $(CLASH_OPTS) --verilog $(FIR_SRC);

$(SIM): $(VHDL)
	cd ghdl; ./simulate.sh

$(IMPL_OOC): $(VHDL)
	cd out_of_context; vivado -mode batch -source synth.tcl

$(IMPL_LOOP): $(VHDL)
	cd loopback; vivado -mode batch -source synth.tcl

clean:
	rm -rf *.o *.hi *.dyn_* verilog vhdl
	rm -f ghdl/*.ghw ghdl/work-*
	rm -f out_of_context/*.rpt out_of_context/*.dcp out_of_context/*.jou out_of_context/*.log out_of_context/hd_visual
	rm -rf loopback/*.rpt loopback/*.dcp loopback/*.jou loopback/*.log loopback/ip loopback/prj.*

shrink: # Remove intermediate files, keeping final results
	rm -rf *.o *.hi *.dyn_* verilog vhdl
	rm -f ghdl/*.ghw ghdl/work-*
	rm -f out_of_context/*.jou out_of_context/*.log out_of_context/hd_visual
	rm -rf loopback/*.jou loopback/*.log

# And now targets with human names...

vhdl: $(VHDL)

verilog: $(VERILOG)

sim : $(SIM)

impl_ooc: $(IMPL_OOC)

impl_loop: $(IMPL_LOOP)
