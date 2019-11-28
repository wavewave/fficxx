rm -rf dist-newstyle
rm -rf stdcxx
rm -rf working
cabal new-build fficxx-runtime
cabal new-build fficxx
sleep 1s
cabal new-exec runhaskell ../stdcxx-gen/Gen.hs
cabal new-build stdcxx

# vector
# for checking temporarily generated cpp file, use -keep-tmp-files
rm vector/test.o vector/test
cabal new-exec -- ghc vector/test.hs
./vector/test

# unique_ptr
rm unique_ptr/test.o unique_ptr/test
cabal new-exec -- ghc unique_ptr/test.hs
./unique_ptr/test

# shared_ptr
rm shared_ptr/test.o shared_ptr/test
cabal new-exec -- ghc -threaded shared_ptr/test.hs
./shared_ptr/test

# function
rm test.o function/hsMain.o function/hsMain
g++ -c function/test.cpp
cabal new-exec -- ghc -keep-tmp-files function/hsMain.hs test.o
./function/hsMain
