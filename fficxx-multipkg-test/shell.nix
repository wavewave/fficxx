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

  tmfTestSrc = import ./template-member/gen.nix {
    inherit stdenv;
    haskellPackages = newHaskellPackages0;
  };

  newHaskellPackages = haskellPackages.override {
    overrides = self: super: {
      "fficxx"               = self.callCabal2nix "fficxx"               ../fficxx         {};
      "fficxx-runtime"       = self.callCabal2nix "fficxx-runtime"       ../fficxx-runtime {};
      "fficxx-multipkg-test" = self.callCabal2nix "fficxx-multipkg-test" ./.               {};
      "stdcxx"               = self.callCabal2nix "stdcxx"               stdcxxSrc         {};
      "tmf-test"             = self.callCabal2nix "tmf-test"             tmfTestSrc        {};
    };

  };

in

newHaskellPackages.fficxx-multipkg-test.env
