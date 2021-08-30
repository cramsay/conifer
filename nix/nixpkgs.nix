{ sources ? import ./sources.nix }:

let

  pkgs = import sources.nixpkgs {};
  clash-compiler = pkgs.callPackage sources.clash-compiler {};

  overlay = _: nixpkgs: {

    # Nix tooling
    niv = (import sources.niv {}).niv;
    gitignore = import sources.gitignore { inherit (nixpkgs) lib; };

    # Haskell overrides
    haskellPackages = nixpkgs.haskellPackages.override {
      overrides = self: super: {
        # External overrides

        ghc-typelits-knownnat =
         self.callCabal2nix "ghc-typelits-knownnat" sources.ghc-typelits-knownnat {};

        ghc-typelits-extra =
         self.callCabal2nix "ghc-typelits-extra" sources.ghc-typelits-extra {};

        ghc-typelits-natnormalise =
         self.callCabal2nix "ghc-typelits-natnormalise" sources.ghc-typelits-natnormalise {};

        ghc-tcplugins-extra =
         self.callCabal2nix "ghc-tcplugins-extra" sources.ghc-tcplugins-extra {};

        clash-lib     = clash-compiler.clash-lib;
        clash-ghc     = clash-compiler.clash-ghc;
        clash-prelude = clash-compiler.clash-prelude;

        # Internal overrides
        conifer = self.callPackage ../clash/default.nix {};
      };
    };
  };

in import sources.nixpkgs { overlays = [ overlay ]; }
