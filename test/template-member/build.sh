g++ -c test.cpp
g++ -c main.cpp
g++ -o main main.o test.o

rm Gen.o
rm -rf working
ghc Gen.hs && ./Gen && cd testpkg && cabal clean && cd ..

cabal sandbox delete && cabal sandbox init && cabal sandbox add-source testpkg && cabal install testpkg

cabal exec -- ghc -c app.hs
cabal exec -- ghc app.hs

