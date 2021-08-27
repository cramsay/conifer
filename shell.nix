{ }:

let
  pkgs = import (builtins.fetchGit {
           #name = "clash-1-2-4";                                                 
           url = "https://github.com/nixos/nixpkgs-channels/";                       
           ref = "refs/heads/nixpkgs-unstable";                     
           rev = "2c162d49cd5b979eb66ff1653aecaeaa01690fcc";
       }) {};    

  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      conifer = self.callPackage clash/default.nix {};
    };
  };
in

with pkgs;

mkShell {
  name = "clash-compiler-shell";
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
