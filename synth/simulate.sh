#!/bin/bash

# Clean previous runs
ghdl clean

# Load sources (careful of order here... need any type defs first, reorder
# helpers, then DUT)
ghdl -a --std=08 --ieee=synopsys vhdl/Main.topEntity/fir_dut_types.vhdl;
for f in vhdl/Main.topEntity/reorder*.vhdl
do
  [ -f "$fname" ] || continue
  ghdl -a --std=08 --ieee=synopsys $f;
done;
ghdl -a --std=08 --ieee=synopsys vhdl/Main.topEntity/fir_dut.vhdl;

# Load testbench, run, and export wavefile
ghdl -a --std=08 --ieee=synopsys impulse_tb.vhdl
ghdl -e --std=08 --ieee=synopsys impulse_tb
ghdl -r --std=08 --ieee=synopsys impulse_tb --stop-time=1us --wave=sim_impulse.ghw
ghdl -a --std=08 --ieee=synopsys overflow_tb.vhdl
ghdl -e --std=08 --ieee=synopsys overflow_tb
ghdl -r --std=08 --ieee=synopsys overflow_tb --stop-time=1us --wave=sim_overflow.ghw
