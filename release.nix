{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let

  newHaskellPackages0 = haskellPackages.override {
    overrides = self: super: {
      "fficxx-runtime" = self.callCabal2nix "fficxx-runtime" ./fficxx-runtime {};
      "fficxx"         = self.callCabal2nix "fficxx"         ./fficxx         {};
    };
  };

  stdcxxNix = import ./stdcxx-gen/default.nix {
    inherit stdenv;
    haskellPackages = newHaskellPackages0;
  };

  newHaskellPackages = haskellPackages.override {
    overrides = self: super:
      {
        "fficxx-runtime" = self.callCabal2nix "fficxx-runtime" ./fficxx-runtime {};
        "fficxx"         = self.callCabal2nix "fficxx"         ./fficxx         {};
        "stdcxx"         = self.callPackage stdcxxNix {};
        "fficxx-test"    = self.callCabal2nix "fficxx-test"    ./fficxx-test    {};
      };
  };

in

{

  "fficxx"         = newHaskellPackages.fficxx;
  "fficxx-runtime" = newHaskellPackages.fficxx-runtime;
  "stdcxx"         = newHaskellPackages.stdcxx;
  "fficxx-test"    = newHaskellPackages.fficxx-test;

}
