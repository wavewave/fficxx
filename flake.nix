{
  description = "fficxx";
  inputs = {
    # nixpkgs/master on 2022-07-18
    nixpkgs.url =
      "github:NixOS/nixpkgs/31997025a4d59f09a9b4c55a3c6ff5ade48de2d6";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        haskellOverlay = final: hself: hsuper: rec {
          stdcxxSrc = import ./stdcxx-gen/gen.nix {
            inherit (final) lib stdenv;
            haskellPackages = hself;
          };
          tmfTestSrc = import ./fficxx-multipkg-test/template-member/gen.nix {
            inherit (final) stdenv;
            haskellPackages = hself;
          };
          tmplDepTestSrc = import ./fficxx-multipkg-test/template-dep/gen.nix {
            inherit (final) stdenv;
            haskellPackages = hself;
          };
          tmplTopLevelTestSrc =
            import ./fficxx-multipkg-test/template-toplevel/gen.nix {
              inherit (final) stdenv;
              haskellPackages = hself;
            };

          "fficxx-runtime" =
            hself.callCabal2nix "fficxx-runtime" ./fficxx-runtime { };
          "fficxx" = hself.callCabal2nix "fficxx" ./fficxx { };
          "stdcxx" = hself.callCabal2nix "stdcxx" stdcxxSrc { };
          "fficxx-multipkg-test" =
            hself.callCabal2nix "fficxx-multipkg-test" ./fficxx-multipkg-test
            { };
          "tmf-test" = hself.callCabal2nix "tmf-test" tmfTestSrc { };
          "tmpl-dep-test" =
            hself.callCabal2nix "tmpl-dep-test" tmplDepTestSrc { };
          "tmpl-dup-inst" = hself.callCabal2nix "tmpl-dup-inst"
            ./fficxx-multipkg-test/tmpl-dup-inst { };
          "tmpl-toplevel-test" =
            hself.callCabal2nix "tmpl-toplevel-test" tmplTopLevelTestSrc { };

        };

        pkgs = import nixpkgs { inherit system; };

      in {
        packages = {
          inherit (pkgs.haskellPackages)
            fficxx fficxx-runtime stdcxx fficxx-test fficxx-multipkg-test
            tmf-test tmpl-dep-test tmpl-dup-inst tmpl-toplevel-test;
        };

        inherit haskellOverlay;

        devShells.default = let
          hpkgsGhc902 = pkgs.haskell.packages.ghc902.override {
            overrides = haskellOverlay pkgs;
          };
        in hpkgsGhc902.shellFor {
          packages = ps: [ ps.fficxx ps.fficxx-runtime ];
          buildInputs = [ pkgs.cabal-install pkgs.ormolu pkgs.nixfmt ];
          withHoogle = false;
        };
      });
}
