{
  description = "fficxx";
  inputs = {
    # nixpkgs/master on 2022-07-18
    nixpkgs.url = "github:NixOS/nixpkgs/31997025a4d59f09a9b4c55a3c6ff5ade48de2d6";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };
        finalHaskellOverlay = self: super: rec {
          stdcxxSrc = import ./stdcxx-gen/gen.nix {
            inherit (pkgs) lib stdenv;
            haskellPackages = self;
          };
          tmfTestSrc = import ./fficxx-multipkg-test/template-member/gen.nix {
            inherit (pkgs) stdenv;
            haskellPackages = self;
          };
          tmplDepTestSrc = import ./fficxx-multipkg-test/template-dep/gen.nix {
            inherit (pkgs) stdenv;
            haskellPackages = self;
          };
          tmplTopLevelTestSrc =
            import ./fficxx-multipkg-test/template-toplevel/gen.nix {
              inherit (pkgs) stdenv;
              haskellPackages = self;
            };


          "fficxx-runtime" =
            self.callCabal2nix "fficxx-runtime" ./fficxx-runtime { };
          "fficxx" = self.callCabal2nix "fficxx" ./fficxx { };
          "stdcxx" = self.callCabal2nix "stdcxx" stdcxxSrc { };
          "fficxx-multipkg-test" =
            self.callCabal2nix "fficxx-multipkg-test" ./fficxx-multipkg-test
            { };
          "tmf-test" = self.callCabal2nix "tmf-test" tmfTestSrc { };
          "tmpl-dep-test" =
            self.callCabal2nix "tmpl-dep-test" tmplDepTestSrc { };
          "tmpl-dup-inst" = self.callCabal2nix "tmpl-dup-inst"
            ./fficxx-multipkg-test/tmpl-dup-inst { };
          "tmpl-toplevel-test" =
            self.callCabal2nix "tmpl-toplevel-test" tmplTopLevelTestSrc { };

        };

        newHaskellPackages = pkgs.haskellPackages.override {
          overrides = finalHaskellOverlay;
        };

      in {
        packages = {
          inherit (newHaskellPackages)
            fficxx fficxx-runtime stdcxx fficxx-test fficxx-multipkg-test tmf-test
            tmpl-dep-test tmpl-dup-inst tmpl-toplevel-test;
        };

        # see these issues and discussions:
        # - https://github.com/NixOS/nixpkgs/issues/16394
        # - https://github.com/NixOS/nixpkgs/issues/25887
        # - https://github.com/NixOS/nixpkgs/issues/26561
        # - https://discourse.nixos.org/t/nix-haskell-development-2020/6170
        overlay = final: prev: {
          haskellPackages = prev.haskellPackages.override (old: {
            overrides = final.lib.composeExtensions (old.overrides or (_: _: { }))
              finalHaskellOverlay;
          });
        };

        devShell = pkgs.haskellPackages.shellFor {
          packages = ps: [ ps.fficxx ps.fficxx-runtime ];
          buildInputs = [ pkgs.cabal-install pkgs.ormolu ];
          withHoogle = false;
        };
      }
  );
}
