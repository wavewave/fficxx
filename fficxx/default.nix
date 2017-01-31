{ mkDerivation, base, bytestring, Cabal, containers, data-default
, directory, either, errors, filepath, hashable, haskell-src-exts
, lens, mtl, process, pureMD5, split, stdenv, template
, template-haskell, text, transformers, unordered-containers
}:
mkDerivation {
  pname = "fficxx";
  version = "0.3";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring Cabal containers data-default directory either
    errors filepath hashable haskell-src-exts lens mtl process pureMD5
    split template template-haskell text transformers
    unordered-containers
  ];
  description = "automatic C++ binding generation";
  license = stdenv.lib.licenses.bsd3;
}
