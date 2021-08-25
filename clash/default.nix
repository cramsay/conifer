{ mkDerivation, base, binary, bytestring, clash-prelude, containers
, ghc-typelits-extra, ghc-typelits-knownnat
, ghc-typelits-natnormalise, hspec, lib, mtl, process, QuickCheck
, reflection, singletons, template-haskell, zlib
}:
mkDerivation {
  pname = "conifer";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base binary bytestring clash-prelude containers ghc-typelits-extra
    ghc-typelits-knownnat ghc-typelits-natnormalise mtl process
    QuickCheck reflection singletons template-haskell zlib
  ];
  testHaskellDepends = [
    base binary bytestring clash-prelude containers ghc-typelits-extra
    ghc-typelits-knownnat ghc-typelits-natnormalise hspec mtl process
    QuickCheck reflection singletons template-haskell zlib
  ];
  doCheck = false;
  description = "A playground for parallel and multiplierless FIR filters with Clash";
  license = lib.licenses.gpl2;
}
