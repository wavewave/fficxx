GHCDIR=$(dirname $(which ghc))
BASEDIR=${GHCDIR}/../lib/ghc-8.0.2

rm test.o
rm hsMain.o

g++ -E test.cpp -I${BASEDIR}/fficxx-runtime-0.5/include
#ghc -c Template.hs
#ghc -c TH.hs
#ghc -c Binding.hs
ghc -c -ddump-splices hsMain.hs
ghc -o hsMain hsMain.hs test.o -lstdc++



