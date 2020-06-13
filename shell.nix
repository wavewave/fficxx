{ pkgs ? import ./nix/pinnedNixpkgs.nix {} }:

with pkgs;

let

  fficxx = haskellPackages.callCabal2nix "fficxx" ./fficxx {};

  hsenv = haskellPackages.ghcWithPackages
            (p: with p;
               let deps = builtins.concatMap
                            (p: p.buildInputs ++ p.nativeBuildInputs ++ p.propagatedBuildInputs)
                            [ fficxx ];
               in deps ++ [ hspec silently ]
            );
in

mkShell {
  buildInputs = [ hsenv ];
}