rm -rf dist-newstyle
cabal new-build fficxx
cabal new-build fficxx-runtime
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
