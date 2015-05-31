{ mkDerivation, base, bytestring, Cabal, containers, data-default
, directory
, either, errors, filepath, hashable, HStringTemplate, lens, mtl
, process, pureMD5, split, stdenv, template-haskell, transformers
, unordered-containers
}:
mkDerivation {
  pname = "fficxx";
  version = "0.2.0";
  src = ./.;
  buildDepends = [
    base bytestring Cabal containers data-default directory either errors filepath
    hashable HStringTemplate lens mtl process pureMD5 split
    template-haskell transformers unordered-containers
  ];
  description = "automatic C++ binding generation";
  license = stdenv.lib.licenses.bsd3;
}
