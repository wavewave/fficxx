{ pkgs }:

self: super: rec {
  fficxx = self.callPackage ./fficxx {};
  fficxx-runtime = self.callPackage ./fficxx-runtime {};
}
