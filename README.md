# Conifer

> A playground for parallel and multiplierless FIR filters with Clash.

This work, presented first at the [2021 Asilomar
conference](https://asilomarsscconf.org/), uses Clash to compose two interesting
filter techniques --- the Fast FIR Algorithm (FFA) for smaller parallel
structures, and a set of different Multiple Constant Multiplication (MCM) blocks
for the subfilter arithmetic. This is presented as a solution for front-end
digital filtering in RFSoC devices, but is actually quite widely applicable.

Both of these techniques are well studied in the literature but very rarely
implemented. We argue that this is only an undiscovered pearl because of our
(unfortunately uncommon) choice of language and tooling. Traditionally,
designers would use _ad-hoc circuit generators_ written in a software
language. These are used essentially as a black-box, prove difficult to test,
and are even more painful to compose. Clash gives us the tools we need to describe
and compose these algorithms (with quite a lot of compile-time complexity) in a
native hardware description language.

For more info, see the prepublication versions of our
[presentation](https://cramsay.co.uk/content/images/2021/10/asilomar_video.mp4),
[slides](https://cramsay.co.uk/content/images/2021/10/asilomar_slides.pdf), and
[poster](https://cramsay.co.uk/content/images/2021/10/asilomar_poster.pdf).

## Usage

We have packaged our circuit generator as cabal and nix packages in `clash/`.

If you want to have a quick play around, the top-level `shell.nix` file will
source everything you need. You could start by looking at one example filter
(and our scripts to implement it out-of-context with Vivado) in the `synth/`
folder. Make sure you have Vivado in your PATH and you have installed the [nix
package manager](https://nixos.org/manual/nix/stable/#chap-installation).

```console
me@computer:~/conifer $ nix-shell

[nix-shell:~/conifer]$ clashi synth/Filter.hs 
Clashi, version 1.4.3 (using clash-lib, version 1.4.3):
https://clash-lang.org/  :? for help
[1 of 1] Compiling Main             ( synth/Filter.hs, interpreted )
Ok, one module loaded.
*Main> ...
```

Have a look at the examples in `clash/src/ExampleFilters.hs` for guidance, or
just have a browse in `clash/` if you're curious.

## Reproducible Science

We present a _lot_ of utilisation/timing results in our publication. We want this
to be as reproducible as possible. If you want to recreate our results, or do
something similar with your own extensions, take a look at the `analysis`
folder. There's another `shell.nix` environment that should include everything
you need. We offer our result-generating scripts as two Jupyter Notebooks:

  1. `ImplementationResults.ipynb` --- Scripts to implement our designs (and
     the LogiCore FIR Compiler's as a reference) with Vivado's out-of-context
     flow, plot the results, and export them as CSVs. See `coeffs.py` and
     `filters.py` for most of the automation and mechanics.
  2. `CoefficientSymbolicTests.ipynb` --- Presents an experimental verification
     (using symbolic programming with `sympy`) of the equations we have derived
     for the number of multiplications our filter structures really need to
     implement when presented with various common styles of symmetry present in
     real-world coefficient sets.

View and interact with them with:
```console
me@computer:~/conifer/analysis $ nix-shell --option sandbox false --command "jupyter lab"
```
This should open a browser window with the Jupyter Lab interface in a new tab.

## License

Conifer is licensed under [GPLv2](./LICENSE). Please feel free to use it
accordingly for educational and academic uses, including extending it for your
own publications. For commercial uses, this license gets *much* more restrictive.
Contact us if you want to negotiate a new license for commercial use.
