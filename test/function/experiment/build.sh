GHCDIR=$(dirname $(which ghc))
BASEDIR=${GHCDIR}/../lib/ghc-8.0.2

rm function_test.o
rm hsMain.o

g++ -c function_test.cpp -I${BASEDIR}/fficxx-runtime-0.3/include
#ghc -c Template.hs
#ghc -c TH.hs
#ghc -c Binding.hs
ghc -ddump-splices hsMain.hs
ghc -o hsMain hsMain.hs function_test.o -lstdc++



