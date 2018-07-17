{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let
  config1 = self: super: {
              "fficxx" = self.callCabal2nix "fficxx" (../fficxx) {};
              "fficxx-runtime" = self.callCabal2nix "fficxx-runtime" (../fficxx-runtime) {};
  
            };
  haskellPackages1 = haskellPackages.override { overrides = config1; };
  
  stdcxx-gen = import ../stdcxx-gen {
                 inherit (pkgs) stdenv;
                 haskellPackages = haskellPackages1;
               };

  config2 = self: super: {
              "stdcxx" = self.callPackage stdcxx-gen { };
            };

  newHaskellPackages = haskellPackages1.override {
                         overrides = self: super: config1 self super // config2 self super;
                       };
  hsenv = newHaskellPackages.ghcWithPackages (p: with p; [fficxx fficxx-runtime stdcxx ]);

in

stdenv.mkDerivation {
  name = "fficxx-test";
  buildInputs = [ hsenv ];
}