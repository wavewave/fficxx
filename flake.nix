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

      newHaskellPackages =
        haskellPackages.override { overrides = finalHaskellOverlay; };

    in {
      packages.x86_64-linux = {
        inherit (newHaskellPackages) fficxx fficxx-runtime stdcxx;
      };

      overlay = final: prev: {
        haskellPackages = prev.haskell.packages.ghc865.override {
          overrides = finalHaskellOverlay;
        };
      };

      devShell.x86_64-linux = with pkgs;
        let
          hsenv = haskell.packages.ghc865.ghcWithPackages
            (p: with p; [ cabal-install ]);
        in mkShell {
          buildInputs = [ hsenv ];
          shellHook = "";
        };
    };
}

