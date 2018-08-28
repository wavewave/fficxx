GHCDIR=$(dirname $(which ghc))
BASEDIR=${GHCDIR}/../lib/ghc-8.0.2

rm function_test.o
rm main.o

g++ -c function_test.cc -I${BASEDIR}/fficxx-runtime-0.3/include
#gcc -c main.c
#g++ -o main main.o function_test.o
ghc -c Binding.hs
ghc -c hsMain.hs
ghc -o hsMain hsMain.hs Binding.hs function_test.o -lstdc++



