rm -rf dist-newstyle
cabal new-build fficxx
cabal new-build fficxx-runtime
cabal new-exec runhaskell ../stdcxx-gen/Gen.hs
cabal new-build stdcxx

# vector
# for checking temporarily generated cpp file, use -keep-tmp-files
rm vector/test.o
cabal new-exec -- ghc  vector/test.hs
#cabal new-exec -- g++ -c vector/stub.cc -o vector/stub.o -I stdcxx/csrc  -I../fficxx-runtime/csrc/
#cabal new-exec -- ghc -c vector/test.hs
#cabal new-exec -- ghc vector/test.hs vector/stub.o

# unique_ptr
#cabal new-exec -- g++ -c unique_ptr/stub.cc -o unique_ptr/stub.o -I stdcxx/csrc  -I../fficxx-runtime/csrc/
#cabal new-exec -- ghc -c unique_ptr/test.hs
#cabal new-exec -- ghc unique_ptr/test.hs unique_ptr/stub.o

# shared_ptr
#cabal new-exec -- g++ -c shared_ptr/stub.cc -o shared_ptr/stub.o -I stdcxx/csrc  -I../fficxx-runtime/csrc/
#cabal new-exec -- ghc -c shared_ptr/test.hs
#cabal new-exec -- ghc -threaded shared_ptr/test.hs shared_ptr/stub.o
