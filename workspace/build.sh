rm -rf dist-newstyle
cd ../stdcxx-gen; ghc Gen.hs ; cd ../workspace
../stdcxx-gen/Gen
cabal new-build stdcxx

# vector
cabal new-exec -- ghc  -keep-tmp-files vector/test.hs vector/TestTH.hs
#cabal new-exec -- g++ -c vector/stub.cc -o vector/stub.o -I stdcxx/csrc  -I../fficxx-runtime/csrc/
#cabal new-exec -- ghc -c vector/test.hs
#cabal new-exec -- ghc vector/test.hs vector/stub.o

# unique_ptr
cabal new-exec -- g++ -c unique_ptr/stub.cc -o unique_ptr/stub.o -I stdcxx/csrc  -I../fficxx-runtime/csrc/
cabal new-exec -- ghc -c unique_ptr/test.hs
cabal new-exec -- ghc unique_ptr/test.hs unique_ptr/stub.o

# shared_ptr
cabal new-exec -- g++ -c shared_ptr/stub.cc -o shared_ptr/stub.o -I stdcxx/csrc  -I../fficxx-runtime/csrc/
cabal new-exec -- ghc -c shared_ptr/test.hs
cabal new-exec -- ghc -threaded shared_ptr/test.hs shared_ptr/stub.o
