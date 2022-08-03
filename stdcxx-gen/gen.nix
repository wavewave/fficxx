{ stdenv, haskellPackages }:

let
  hsenv = haskellPackages.ghcWithPackages (p: with p; [ fficxx-runtime fficxx ]);
in

stdenv.mkDerivation {
  name = "stdcxx-src";
  buildInputs = [ hsenv ];
  src = ./.;
  buildPhase = ''
    runhaskell Gen.hs
    # for gcc/clang difference, we need this ad hoc treatment.
    # TODO: find a better way than os(darwin)
    sed -i 's/  extra-libraries:/if os(darwin)\n  extra-libraries:    libc++\nelse\n  extra-libraries:    libstdc++/g' stdcxx/stdcxx.cabal
  '';
  installPhase = ''
    mkdir -p $out
    cp -a stdcxx/* $out
  '';
}
