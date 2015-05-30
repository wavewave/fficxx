{ pkgs ? import <nixpkgs> {} }:

let newghcpkgs = pkgs.callPacakge <nixpkgs/pkgs/development/haskell-modules> {
      ghc = pkgs.haskell.packages.ghc7101;
      packageSetConfig = pkgs.callPackage <nixpkgs/pkgs/development/haskell-modules/configuration-ghc-7.10.x.nix> {};
      overrides = self: super: rec {
        fficxx = self.callPackage ./. {};
      };
    };
in pkgs.stdenv.mkDerivation {
     name = "fficxx-use-env";
     buildInputs = [ newghcpkgs pkgs.snappy ];
     shellHook = ''
       eval $(grep export ${newghcpkgs}/bin/ghc)
     '';
   }
