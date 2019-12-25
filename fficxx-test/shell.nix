# NOTE: this doesn't include stdcxx intentionally.

{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let

  newHaskellPackages0 = haskellPackages.override {
    overrides = self: super: {
      "fficxx-runtime" = self.callCabal2nix "fficxx-runtime" ../fficxx-runtime {};
      "fficxx"         = self.callCabal2nix "fficxx"         ../fficxx         {};
#      "stdcxx"         = self.callPackage stdcxxNix {};
    };
  };

  stdcxxNix = import ../stdcxx-gen/default.nix {
    inherit stdenv;
    haskellPackages = newHaskellPackages0;
  };

  newHaskellPackages = haskellPackages.override {
    overrides = self: super: {
      "fficxx-runtime" = self.callCabal2nix "fficxx-runtime" ../fficxx-runtime {};
      "fficxx"         = self.callCabal2nix "fficxx"         ../fficxx         {};
      "stdcxx"         = self.callPackage stdcxxNix {};
    };

  };

  hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
    fficxx
    fficxx-runtime
    stdcxx
  ]);

in

stdenv.mkDerivation {
  name = "fficxx-test-env";

  buildInputs = [
    hsenv
  ];

}
