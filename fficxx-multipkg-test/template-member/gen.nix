{ stdenv, haskellPackages }:

let
  hsenv = haskellPackages.ghcWithPackages (p: with p; [ fficxx-runtime fficxx ]);
in

stdenv.mkDerivation {
  name = "template-member-src";
  buildInputs = [ hsenv ];
  src = ./.;
  buildPhase = ''
    runhaskell Gen.hs ./template
  '';
  installPhase = ''
    mkdir -p $out
    cp -a tmf-test/* $out
  '';
}
