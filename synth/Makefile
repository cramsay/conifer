CLASH = clash
CLASH_OPTS = -fconstraint-solver-iterations=20 -fclash-inline-limit=200 -fclash-spec-limit=1000 
BUILD_DIR = build/clash/SamFilters.hs
VHDL = build/clash/vhdl/SamFilters/topCoarseDec/topCoarseDec.vhdl
IP = build/xil/ip/component.xml

all: $(IP)

$(IP): $(VHDL)
	cd build/xil; vivado -mode batch -source mk_ip.tcl

$(VHDL): $(BUILD_DIR)
	cd build/clash; $(CLASH) $(CLASH_OPTS) --vhdl SamFilters.hs;

build_dir: $(BUILD_DIR)

$(BUILD_DIR):
	mkdir build
	cp -r src/* build

vivado_raw: $(VHDL)
		cd build/xil; vivado -mode batch -source prj_raw.tcl

yosys: $(VHDL)
	cd  build/clash/verilog/Top/fir_rsg; yosys -s ../../../../yosys/view_synth.yosys

clean:
	rm -r build
