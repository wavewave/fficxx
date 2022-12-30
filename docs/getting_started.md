Getting Started
===============

fficxx is mainly packaged in nix.
`shell.nix` is for development,
```
nix-shell shell.nix
```
and `use.nix` is for using the generated binding package.
```
nix-shell use.nix
```

For all build,
```
nix-build release.nix
```
