GHCDIR=$(dirname $(which ghc))
BASEDIR=${GHCDIR}/../lib/ghc-8.0.2

rm function_test.o
rm hsMain.o

g++ -c function_test.cc -I${BASEDIR}/fficxx-runtime-0.3/include
ghc -c Binding.hs
ghc -c hsMain.hs
ghc -o hsMain hsMain.hs Binding.hs function_test.o -lstdc++



