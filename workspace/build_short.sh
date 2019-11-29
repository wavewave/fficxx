rm -rf dist-newstyle
rm -rf stdcxx
rm -rf tmf-test
rm -rf inherit-test
rm -rf working
cabal new-build fficxx
sleep 1s
cabal new-exec runhaskell ../stdcxx-gen/Gen.hs
cabal new-build stdcxx

# inherit
runhaskell ./inherit/Gen.hs ./inherit/template
cabal new-build inherit-test
cabal new-exec -- runhaskell -- -fobject-code -O0 inherit/app.hs
