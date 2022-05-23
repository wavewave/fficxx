{
  description = "fficxx";
  inputs = { nixpkgs.url = "github:NixOS/nixpkgs/nixos-20.03"; };
  outputs = { self, nixpkgs }:
    let
      pkgs = nixpkgs.legacyPackages.x86_64-linux;

      haskellPackages = pkgs.haskell.packages.ghc865;
      newHaskellPackages0 = haskellPackages.override {
        overrides = self: super: {
          "fficxx-runtime" =
            self.callCabal2nix "fficxx-runtime" ./fficxx-runtime { };
          "fficxx" = self.callCabal2nix "fficxx" ./fficxx { };
        };
      };

      stdcxxSrc = import ./stdcxx-gen/gen.nix {
        inherit (pkgs) stdenv;
        haskellPackages = newHaskellPackages0;
      };

      finalHaskellOverlay = self: super: {
        "fficxx-runtime" =
          self.callCabal2nix "fficxx-runtime" ./fficxx-runtime { };
        "fficxx" = self.callCabal2nix "fficxx" ./fficxx { };
        "stdcxx" = self.callCabal2nix "stdcxx" stdcxxSrc { };
      };

      newHaskellPackages = haskellPackages.override {
        overrides = let
          tmfTestSrc = import ./fficxx-multipkg-test/template-member/gen.nix {
            inherit (pkgs) stdenv;
            haskellPackages = newHaskellPackages0;
          };
          tmplDepTestSrc = import ./fficxx-multipkg-test/template-dep/gen.nix {
            inherit (pkgs) stdenv;
            haskellPackages = newHaskellPackages0;
          };
          tmplTopLevelTestSrc =
            import ./fficxx-multipkg-test/template-toplevel/gen.nix {
              inherit (pkgs) stdenv;
              haskellPackages = newHaskellPackages0;
            };

        in self: super:
        finalHaskellOverlay self super // {
          "fficxx-test" = self.callCabal2nix "fficxx-test" ./fficxx-test { };
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
      };

    in {
      packages.x86_64-linux = {
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

      devShell.x86_64-linux = with pkgs;
        let
          hsenv = haskell.packages.ghc865.ghcWithPackages
            (p: [ p.cabal-install p.hspec ]);
        in mkShell {
          buildInputs = [ hsenv ];
          shellHook = "";
        };
    };
}

