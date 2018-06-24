{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let

  fficxx = haskellPackages.callCabal2nix "fficxx" ./. {};

in

  fficxx.env
