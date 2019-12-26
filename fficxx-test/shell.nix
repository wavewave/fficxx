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

  stdcxxSrc = import ../stdcxx-gen/gen.nix {
    inherit stdenv;
    haskellPackages = newHaskellPackages0;
  };

  newHaskellPackages = haskellPackages.override {
    overrides = self: super: {
      "fficxx"         = self.callCabal2nix "fficxx"         ../fficxx         {};
      "fficxx-runtime" = self.callCabal2nix "fficxx-runtime" ../fficxx-runtime {};
      "fficxx-test"    = self.callCabal2nix "fficxx-test"    ./.               {};
      "stdcxx"         = self.callCabal2nix "stdcxx"         stdcxxSrc         {};
    };

  };

in

newHaskellPackages.fficxx-test.env
