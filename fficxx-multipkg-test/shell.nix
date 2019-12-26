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

  tmfTestNix = import ./template-member/default.nix {
    inherit stdenv;
    haskellPackages = newHaskellPackages0;
  };

  newHaskellPackages = haskellPackages.override {
    overrides = self: super: {
      "fficxx"               = self.callCabal2nix "fficxx"               ../fficxx         {};
      "fficxx-runtime"       = self.callCabal2nix "fficxx-runtime"       ../fficxx-runtime {};
      "fficxx-multipkg-test" = self.callCabal2nix "fficxx-multipkg-test" ./.               {};
      "stdcxx"               = self.callPackage stdcxxNix {};
      "tmf-test"             = self.callPacakge tmfTestNix {};
    };

  };

in

newHaskellPackages.fficxx-multipkg-test.env
