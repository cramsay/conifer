create_clock -period 1.25 -name clk [get_ports clk]
#set_property BLOCK_SYNTH.ADDER_THRESHOLD 127 [get_cells]
#set_property BLOCK_SYNTH.COMPARATOR_THRESHOLD 127 [get_cells]
#set_property BLOCK_SYNTH.LUT_COMBINING 1 [get_cells]
