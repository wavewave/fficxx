{ mkDerivation, base, stdenv, template-haskell }:
mkDerivation {
  pname = "fficxx-runtime";
  version = "0.2.999";
  src = ./.;
  libraryHaskellDepends = [ base template-haskell ];
  description = "Runtime for fficxx-generated library";
  license = stdenv.lib.licenses.bsd3;
}
