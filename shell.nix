{ nixpkgs ? import ./nix/nixpkgs.nix {} }:

with nixpkgs;

mkShell {
  name = "clash-conifer-shell";
  buildInputs = [

    yosys
    graphviz

    (python36.buildEnv.override {
      extraLibs = with python36Packages; [
	      xdot
      ];
    })

    (haskellPackages.ghcWithPackages (p: with p; [
      clash-ghc
      ghc-typelits-extra
      ghc-typelits-knownnat
      ghc-typelits-natnormalise
      Chart-cairo
      zlib
      hspec
      conifer
    ])
    )
  ];
}
