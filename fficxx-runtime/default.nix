{ mkDerivation, base, bytestring, stdenv, template-haskell }:
mkDerivation {
  pname = "fficxx-runtime";
  version = "0.3";
  src = ./.;
  libraryHaskellDepends = [ base bytestring template-haskell ];
  description = "Runtime for fficxx-generated library";
  license = stdenv.lib.licenses.bsd3;
}
