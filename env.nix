{ pkgs ? import <nixpkgs> {} }:

let newghcpkgs = pkgs.callPackage <nixpkgs/pkgs/development/haskell-modules> {
      ghc = pkgs.haskell.compiler.ghc7101;
      packageSetConfig = pkgs.callPackage <nixpkgs/pkgs/development/haskell-modules/configuration-ghc-7.10.x.nix> {};
      overrides = import ./override.nix { inherit pkgs; };
    };
    hsenv = newghcpkgs.ghcWithPackages (pkgs: with pkgs; [fficxx fficxx-runtime]); 
in pkgs.stdenv.mkDerivation {
     name = "fficxx-use-env";
     buildInputs = [ hsenv pkgs.snappy ];
     shellHook = ''
       eval $(grep export ${hsenv}/bin/ghc)
     '';
   }
