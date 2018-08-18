GHCDIR=$(dirname $(which ghc))
BASEDIR=${GHCDIR}/../lib/ghc-8.0.2

ghc Gen.hs
./Gen
cd testpkg && cabal clean && cd ..

cabal sandbox delete
cabal sandbox init
cabal sandbox add-source testpkg
cabal install testpkg

g++ -c stub.cc -I${BASEDIR}/stdcxx-0.0/include -I${BASEDIR}/fficxx-runtime-0.3/include
cabal exec -- ghc -c test.hs
cabal exec -- ghc test.hs stub.o
