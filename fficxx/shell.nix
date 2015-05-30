with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, base, bytestring, Cabal, containers, directory
             , either, errors, filepath, hashable, HStringTemplate, lens, mtl
             , process, pureMD5, split, stdenv, template-haskell, transformers
             , unordered-containers
             }:
             mkDerivation {
               pname = "fficxx";
               version = "0.2.0";
               src = ./.;
               buildDepends = [
                 base bytestring Cabal containers directory either errors filepath
                 hashable HStringTemplate lens mtl process pureMD5 split
                 template-haskell transformers unordered-containers
               ];
	       buildTools = [ haskellngPackages.cabal-install pkgs.snappy ];
               description = "automatic C++ binding generation";
               license = stdenv.lib.licenses.bsd3;
             }) {};
in
  pkg.env
