rm -rf dist-newstyle
rm -rf stdcxx
rm -rf tmf-test
rm -rf proxy-test
rm -rf working
cabal new-build fficxx && cabal new-exec runhaskell ../stdcxx-gen/Gen.hs && cabal new-build stdcxx

# map
rm map/test.o map/test
cabal new-exec -- ghc map/test.hs
./map/test
