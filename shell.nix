{ }:

let
  pkgs = import (builtins.fetchGit {
           #name = "clash-1-2-4";                                                 
           url = "https://github.com/nixos/nixpkgs-channels/";                       
           ref = "refs/heads/nixpkgs-unstable";                     
           rev = "2c162d49cd5b979eb66ff1653aecaeaa01690fcc";
       }) {};    
in

with pkgs;

mkShell {
  name = "clash-compiler-shell";
  shellHook = "source /tools/Xilinx/Vivado/2019.1/settings64.sh";
  buildInputs = [
    # My own packages...
    #yosys
    #graphviz
    cabal-install

    #(python36.buildEnv.override {
    #  extraLibs = with python36Packages; [
    #    # Add pythonPackages without the prefix
	#xdot
    #  ];
    #})

    # For quick clash experimentation
    (pkgs.haskellPackages.ghcWithPackages (p: with p; [
      clash-ghc
      ghc-typelits-extra
      ghc-typelits-knownnat
      ghc-typelits-natnormalise
      Chart-cairo
      zlib
      hspec
    ])
    )
  ];
}
