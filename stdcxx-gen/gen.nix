{ lib, stdenv, haskellPackages }:

let
  hsenv = haskellPackages.ghcWithPackages (p: with p; [ fficxx-runtime fficxx ]);
in

stdenv.mkDerivation {
  name = "stdcxx-src";
  buildInputs = [ hsenv ];
  src = ./.;
  buildPhase = ''
    runhaskell Gen.hs
  '' + lib.optionalString (stdenv.isDarwin) ''
    # for gcc/clang difference, we need this ad hoc treatment.
    # TODO: find a better way than os(darwin)
    sed -i 's/stdc++/c++/g' stdcxx/stdcxx.cabal
  '';
  installPhase = ''
    mkdir -p $out
    cp -a stdcxx/* $out
  '';
}
