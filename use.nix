{ pkgs ? import ./nix/pinnedNixpkgs.nix {} }:

with pkgs;

let

  newHaskellPackages = haskellPackages.override {
    overrides = self: super: {
      "fficxx-runtime" = self.callCabal2nix "fficxx-runtime" ./fficxx-runtime {};
      "fficxx"         = self.callCabal2nix "fficxx"         ./fficxx         {};
    };
  };

  hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
    fficxx
    fficxx-runtime
  ]);

in

stdenv.mkDerivation {
  name = "fficxx-use-env";

  buildInputs = [
    hsenv
  ];

}
