{ pkgs ? import <nixpkgs> {} }:


with pkgs;

let hsenv = haskellPackages.ghcWithPackages
      (p: with p; [ base-orphans cereal data-default dlist old-locale parallel parsec either errors lens 
                    HStringTemplate crypto-api pureMD5 syb split
       ] );
in stdenv.mkDerivation {
     name = "fficxx-shell";
     buildInputs = [ hsenv snappy ];
     shellHook= ''
       eval $(grep export ${hsenv}/bin/ghc)
     '';
   }
   
#let newghcpkgs = pkgs.callPackage <nixpkgs/pkgs/development/haskell-modules> {
#      ghc = pkgs.haskell.compiler.ghc7103;
#      packageSetConfig = pkgs.callPackage <nixpkgs/pkgs/development/haskell-modules/configuration-ghc-7.10.x.nix> {};
#      overrides = import ./override.nix { inherit pkgs; };
#    };
#    hsenv = newghcpkgs.ghcWithPackages (pkgs: with pkgs; [fficxx fficxx-runtime cabal-install]); 
#in pkgs.stdenv.mkDerivation {
#     name = "fficxx-use-env";
#     buildInputs = [ hsenv pkgs.snappy ];
#     shellHook = ''
#       eval $(grep export ${hsenv}/bin/ghc)
#     '';
#   }
