# NOTE: this doesn't include stdcxx intentionally.

{ pkgs ? import ../nix/pinnedNixpkgs.nix {} }:

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

  tmplDepTestSrc = import ./template-dep/gen.nix {
    inherit stdenv;
    haskellPackages = newHaskellPackages0;
  };

  tmplTopLevelTestSrc = import ./template-toplevel/gen.nix {
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
      "tmpl-dep-test"        = self.callCabal2nix "tmpl-dep-test"        tmplDepTestSrc    {};
      "tmpl-toplevel-test"   = self.callCabal2nix "tmpl-toplevel-test"   tmplTopLevelTestSrc    {};
    };

  };

in

newHaskellPackages.fficxx-multipkg-test.env
