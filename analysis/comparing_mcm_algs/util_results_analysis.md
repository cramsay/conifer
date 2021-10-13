Slice architecture has changed quite a bit since virtex II, so let's revisit results here.

LUT structure?
CARRY structure?
FFs per slice vs LUTs per slice

## Test case

Using ws = [492, 1829, 444, 1008, 732, 899, 962, 1555, 1288, 923, 1577, 757, 632, 1843, 867, 634, 268, 1787, 1222, 1840, 1595, 775, 1607, 2020, 1849, 1411]

HcubS (with maxDepth = Just 3) -> 32 adders, 3 depth, 321 slice est
Hcub (with maxDepth = Nothing) -> 27 adders, 7 depth, 922 slice est
RAG-n                          -> 28 adders, 7 depth, 931 slice est
RSG                            -> 39 adders, 3 depth, 428 slice est


## Shift registers

Hcub and RAG-n both infer SRL16Es for the uninterupted, non-branching series of
delays when synthesized fundamentals are generated early, but must be propagated
to reach pipeline stage 7! This uses some SLICEMs in place of even more SLICELs

Neither HcubS or RSG infer any SR16LEs.

It looks like the shift register usage might also infer some LUT4 and FDRE cells too, unless there is a big coincidence here:

| SRL16E   |   79 |  Distributed Memory |
| LUT4     |   79 |                 LUT |
| FDRE     |   79 |        Flop & Latch |

The rest of the FFs are all FDCEs, so this is extra suspicious.
Are the LUT4s muxes for SR16Le output? 2^4 = 16...?
TODO Open rag-n schematic and check

## LUT sizes?

LUT2s are the bulk for all implementations

What causes LUT1s?

LUT4s for Hcub and RAG-n are potentially to support SRL16Es.

What about HcubS though?

HcubS 32 adders 
RSG 39 adders    => HcubS 82% of the luts(ish)? Nope! What about the adder chain?



## Questions

  * Why does Hcub/RAG-n infer 2-bit out LUTs, while RSG doesn't?
  * Same as above but for shift registers & are these more efficient?
  * How well packed are the slices using the defaults?
  * Carry vs O5&O6 LUTS? Look at vivado settings
