# Conifer

A playground for parallel and multiplierless FIR filters with Clash.


## Environment notes

Reproducable environments are great, so let's use Nix.
`cd` to this directory and just run `nix-shell`.

Make sure vivado is in your `$PATH` if you want to generate bitstreams.

## Hcub bugs found

  * There's that dumb thing about target sets that are all powers of two reducing to empty lists and crashing

  * We note that there are discrepancies between the mathematics and the source
    provided. If we could just express our HDL mathematically, maybe this would
    be avoided. See distance 3 test case 5. It says we are an adder distance of
    3 away from a target if A*(S,t) intersects S (page p24). However the
    definition of the A* function (page 10; Eq. 6) explicitly excludes any
    elements of the input sets. Therefore this test *should* only every report
    failure. This is not the case in the SPIRAL implementation In the
    implementation there *is* some concept of exclusion that comes from storing
    fundamentals in a set, but this isn't the same thing...


## References

### Template Haskell

https://markkarpov.com/tutorial/th.html is a great tutorial

https://blog.jle.im/entry/fixed-length-vector-types-in-haskell.html for
explaining dependent typing with Vecs.

  + ScopedTypeVariables and explicit 'forall' lets us use a KnownNat n in the
    body!

### Generating lookups for adder costs

Found some example code @ http://spiral.ece.cmu.edu/mcm/gen.html Download
synth-jan-14-2009.tar.gz Good reference, but it's just a lookup table.

See Dempster and Macleod's (Multiplication by an integer using minimum
adders)[https://ieeexplore.ieee.org/document/297467/] for details.

Original BH (Bull and Hollock) paper (Primitive operator digital
filters)[https://ieeexplore.ieee.org/stamp/stamp.jsp?tp=&arnumber=258032]
actually has algorithms!

Nice comparison @ Arithmetic Circuits for DSP Applications
edited by Pramod Kumar Meher, Thanos Stouraitis

Only full implementation I found is a perl script that generates verilog!
https://github.com/verilog-to-routing/vtr-verilog-to-routing/blob/master/vtr_flow/benchmarks/arithmetic/multless_consts/verilog/firgen/multBlockGen.pl
