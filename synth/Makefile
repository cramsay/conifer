CLASH = clash
CLASH_OPTS = -fconstraint-solver-iterations=40 -fclash-inline-limit=400 -fclash-spec-limit=1000
FIR_SRC = Filter.hs
VHDL = vhdl/Main.topEntity/fir_dut.vhdl
VERILOG = verilog/Main/filter/fir_dut.v
IMPL = post_route.dcp

all: $(IMPL)

$(VHDL): $(FIR_SRC)
	$(CLASH) $(CLASH_OPTS) --vhdl $(FIR_SRC);

$(VERILOG): $(FIR_SRC)
	$(CLASH) $(CLASH_OPTS) --verilog $(FIR_SRC);

$(IMPL): $(VHDL)
	vivado -mode batch -source synth.tcl

clean:
	rm -r *.rpt *.dcp *.o *.hi hd_visual verilog vhdl *.jou *.log

shrink: # Remove intermediate files, keeping final results
	rm -r *.o *.hi hd_visual vhdl


# And now targets with human names...

vhdl: $(VHDL)

verilog: $(VERILOG)

impl: $(IMPL)

yosys: $(VERILOG)
	yosys -s view_synth.yosys

