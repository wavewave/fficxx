{ stdenv, haskellPackages }:

let
  hsenv = haskellPackages.ghcWithPackages (p: with p; [ fficxx-runtime fficxx ]);
in

stdenv.mkDerivation {
  name = "stdcxx-src";
  buildInputs = [ hsenv ];
  src = ./.;
  buildPhase = ''
    ghc Gen.hs
    ./Gen
  '';
  installPhase = ''
    mkdir -p $out
    cp -a stdcxx/* $out
  '';
}
