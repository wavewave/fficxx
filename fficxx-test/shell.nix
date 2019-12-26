# NOTE: this doesn't include stdcxx intentionally.

{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let

  newHaskellPackages0 = haskellPackages.override {
    overrides = self: super: {
      "fficxx-runtime" = self.callCabal2nix "fficxx-runtime" ../fficxx-runtime {};
      "fficxx"         = self.callCabal2nix "fficxx"         ../fficxx         {};
    };
  };

  stdcxxNix = import ../stdcxx-gen/default.nix {
    inherit stdenv;
    haskellPackages = newHaskellPackages0;
  };

  newHaskellPackages = haskellPackages.override {
    overrides = self: super: {
      "fficxx"         = self.callCabal2nix "fficxx"         ../fficxx         {};
      "fficxx-runtime" = self.callCabal2nix "fficxx-runtime" ../fficxx-runtime {};
      "fficxx-test"    = self.callCabal2nix "fficxx-test"    ./.               {};
      "stdcxx"         = self.callPackage stdcxxNix {};
    };

  };

in

newHaskellPackages.fficxx-test.env
