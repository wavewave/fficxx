{ stdenv, haskellPackages }:

let
  hsenv = haskellPackages.ghcWithPackages (p: with p; [ fficxx-runtime fficxx ]);
in

stdenv.mkDerivation {
  name = "template-toplevel-src";
  buildInputs = [ hsenv ];
  src = ./.;
  buildPhase = ''
    runhaskell Gen.hs ./template
  '';
  installPhase = ''
    mkdir -p $out
    cp -a tmpl-toplevel-test/* $out
  '';
}
