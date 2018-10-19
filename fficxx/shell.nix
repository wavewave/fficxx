{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let

  fficxx = pkgs.haskell.packages.ghc822.callCabal2nix "fficxx" ./. {};

in

  fficxx.env
