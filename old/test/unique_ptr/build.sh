GHCDIR=$(dirname $(which ghc))
BASEDIR=${GHCDIR}/../lib/ghc-8.0.2

g++ -c stub.cc -I${BASEDIR}/stdcxx-0.5/include -I${BASEDIR}/fficxx-runtime-0.5/include
ghc -c test.hs
ghc test.hs stub.o
