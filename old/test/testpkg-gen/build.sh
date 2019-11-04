GHCDIR=$(dirname $(which ghc))
BASEDIR=${GHCDIR}/../lib/ghc-8.0.2

rm Gen.o
ghc Gen.hs && ./Gen && cd testpkg && cabal clean && cd ..

cabal sandbox delete && cabal sandbox init && cabal sandbox add-source testpkg && cabal install testpkg \

g++ -c stub.cc -I${BASEDIR}/stdcxx-0.5/include -I${BASEDIR}/fficxx-runtime-0.5/include -Itestpkg/csrc
cabal exec -- ghc -c test.hs
cabal exec -- ghc test.hs stub.o
