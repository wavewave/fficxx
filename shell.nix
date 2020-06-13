{ pkgs ? import ./nix/pinnedNixpkgs.nix {} }:

with pkgs;

let

  fficxx = pkgs.haskellPackages.callCabal2nix "fficxx" ./fficxx {};

in

fficxx.env
