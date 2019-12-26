{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let

  newHaskellPackages0 = haskellPackages.override {
    overrides = self: super: {
      "fficxx-runtime" = self.callCabal2nix "fficxx-runtime" ./fficxx-runtime {};
      "fficxx"         = self.callCabal2nix "fficxx"         ./fficxx         {};
    };
  };

  stdcxxSrc = import ./stdcxx-gen/gen.nix {
    inherit stdenv;
    haskellPackages = newHaskellPackages0;
  };

  newHaskellPackages = haskellPackages.override {
    overrides = self: super:
      {
        "fficxx-runtime" = self.callCabal2nix "fficxx-runtime" ./fficxx-runtime {};
        "fficxx"         = self.callCabal2nix "fficxx"         ./fficxx         {};
        "stdcxx"         = self.callCabal2nix "stdcxx"         stdcxxSrc        {};
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
