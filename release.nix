{ pkgs ? import ./nix/pinnedNixpkgs.nix {} }:

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

  tmfTestSrc = import ./fficxx-multipkg-test/template-member/gen.nix {
    inherit stdenv;
    haskellPackages = newHaskellPackages0;
  };

  tmplDepTestSrc = import ./fficxx-multipkg-test/template-dep/gen.nix {
    inherit stdenv;
    haskellPackages = newHaskellPackages0;
  };

  newHaskellPackages = haskellPackages.override {
    overrides = self: super:
      {
        "fficxx-runtime"       = self.callCabal2nix "fficxx-runtime"       ./fficxx-runtime       {};
        "fficxx"               = self.callCabal2nix "fficxx"               ./fficxx               {};
        "stdcxx"               = self.callCabal2nix "stdcxx"               stdcxxSrc              {};
        "fficxx-test"          = self.callCabal2nix "fficxx-test"          ./fficxx-test          {};
        "fficxx-multipkg-test" = self.callCabal2nix "fficxx-multipkg-test" ./fficxx-multipkg-test {};
        "tmf-test"             = self.callCabal2nix "tmf-test"             tmfTestSrc             {};
        "tmpl-dep-test"        = self.callCabal2nix "tmpl-dep-test"        tmplDepTestSrc         {};
        "tmpl-dup-inst"        = self.callCabal2nix "tmpl-dup-inst"        ./fficxx-multipkg-test/tmpl-dup-inst {};
      };
  };

in

{

  "fficxx"         = newHaskellPackages.fficxx;
  "fficxx-runtime" = newHaskellPackages.fficxx-runtime;
  "stdcxx"         = newHaskellPackages.stdcxx;
  "fficxx-test"    = newHaskellPackages.fficxx-test;
  "fficxx-multipkg-test" = newHaskellPackages.fficxx-multipkg-test;
  "tmpl-dup-inst"  = newHaskellPackages.tmpl-dup-inst;

}
