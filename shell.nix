{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let

  fficxx = pkgs.haskellPackages.callCabal2nix "fficxx" ./fficxx {};

in

fficxx.env
