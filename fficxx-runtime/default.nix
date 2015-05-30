{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "fficxx-runtime";
  version = "0.2";
  src = ./.;
  buildDepends = [ base ];
  description = "Runtime for fficxx-generated library";
  license = stdenv.lib.licenses.bsd3;
}
