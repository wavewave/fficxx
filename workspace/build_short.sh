rm -rf dist-newstyle
rm -rf stdcxx
rm -rf tmf-test
rm -rf proxy-test
rm -rf working
cabal new-build fficxx
sleep 1s
cabal new-exec runhaskell ../stdcxx-gen/Gen.hs
cabal new-build stdcxx

# proxy
runhaskell ./proxy/Gen.hs ./proxy/template
cabal new-build proxy-test
cabal new-exec -- runhaskell -- -iproxy -fobject-code -O0 proxy/app.hs
