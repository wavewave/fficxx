{ stdenv, haskellPackages }:

let
  stdcxx-src = import ./gen.nix { inherit stdenv haskellPackages; };
in

{ mkDerivation, base, fficxx, fficxx-runtime, stdenv, template-haskell }:
mkDerivation {
  pname = "stdcxx";
  version = "0.6";
  src = stdcxx-src;
  libraryHaskellDepends = [
    base fficxx fficxx-runtime template-haskell
  ];
  librarySystemDepends = [ ];
  license = stdenv.lib.licenses.bsd3;
}
