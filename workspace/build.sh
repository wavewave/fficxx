rm -rf dist-newstyle
cd ../stdcxx-gen; ghc Gen.hs ; cd ../workspace
../stdcxx-gen/Gen
cabal new-build stdcxx
cabal new-exec -- g++ -c vector/stub.cc -I stdcxx/csrc  -I../fficxx-runtime/csrc/
cabal new-exec -- ghc -c vector/test.hs
cabal new-exec -- ghc vector/test.hs stub.o
