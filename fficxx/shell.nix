{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let

  fficxx = pkgs.haskellPackages.callCabal2nix "fficxx" ./. {};

in

  fficxx.env
