{
  description = "fficxx";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/master";
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

          "template" = final.haskell.lib.doJailbreak (hsuper.template);

          "fficxx-runtime" =
            hself.callCabal2nix "fficxx-runtime" ./fficxx-runtime { };
          "fficxx" = hself.callCabal2nix "fficxx" ./fficxx { };
          "stdcxx" = hself.callCabal2nix "stdcxx" stdcxxSrc { };
          "fficxx-test" = hself.callCabal2nix "fficxx-test" ./fficxx-test { };
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

        hpkgsFor = compiler:
          pkgs.haskell.packages.${compiler}.extend (haskellOverlay pkgs);

        mkPackages = compiler: {
          inherit (hpkgsFor compiler)
            fficxx fficxx-runtime stdcxx fficxx-test fficxx-multipkg-test
            tmf-test tmpl-dep-test tmpl-dup-inst tmpl-toplevel-test;
        };

        mkShellFor = compiler:
          let
            pyenv = pkgs.python3.withPackages
              (p: [ p.sphinx p.sphinx_rtd_theme p.myst-parser ]);
          in (hpkgsFor compiler).shellFor {
            packages = ps: [ ps.fficxx ps.fficxx-runtime ];
            extraDependencies = ps: {
              libraryHaskellDepends = [ ps.hspec-discover ];
            };

            buildInputs = [ pkgs.cabal-install pkgs.ormolu pkgs.nixfmt pyenv ];
            withHoogle = false;
            shellHook = ''
              export PS1="\n[fficxx:\w]$ \0"  
            '';
          };

        supportedCompilers = [ "ghc927" "ghc945" "ghc962" ];
      in {
        packages =
          pkgs.lib.genAttrs supportedCompilers (compiler: hpkgsFor compiler);

        inherit haskellOverlay;

        devShells =
          pkgs.lib.genAttrs supportedCompilers (compiler: mkShellFor compiler);
      });
}
