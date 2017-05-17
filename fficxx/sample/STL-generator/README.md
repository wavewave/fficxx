How to test this.

For build

```
$ nix-shell ../../shell.nix
$ cabal sandbox init
$ cabal sandbox add-source ../..
$ cabal sandbox add-source ../../../fficxx-runtime
$ cabal install fficxx
$ cabal install fficxx-runtime
$ cabal exec -- runhaskell STLGen.hs
$ cabal sandbox add-source STL
$ cabal install STL
$ cabal exec -- g++ -c stub.cc --std=c++14 -I../../../fficxx-runtime/csrc -ISTL/csrc
$ cabal exec -- ghc test.hs
$ cabal exec -- ghc test.hs stub.o
```

then you will see
```
$ ./test
0
1
101
100
5
Foo: 9
1
Foo: 10
3
101
103
1003
```
