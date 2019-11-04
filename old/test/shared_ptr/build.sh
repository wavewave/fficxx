GHCDIR=$(dirname $(which ghc))
BASEDIR=${GHCDIR}/../lib/ghc-8.0.2

g++ -c stub.cc -I${BASEDIR}/stdcxx-0.0/include -I${BASEDIR}/fficxx-runtime-0.3/include
ghc -c -threaded test.hs
ghc -threaded test.hs stub.o
