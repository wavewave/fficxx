#rm -rf dist-newstyle
#rm -rf stdcxx
#rm -rf tmf-test
#rm -rf working
#cabal build fficxx-runtime
cabal build fficxx && cabal exec -- runhaskell ../fficxx-multipkg-test/template-member/Gen.hs ../fficxx-multipkg-test/template-member/template && cabal build tmf-test

#sleep 1s
#cabal exec runhaskell ../stdcxx-gen/Gen.hs
#cabal build stdcxx

# template-member
#cabal exec -- runhaskell -- -fobject-code -O0 ../fficxx-multipkg-test/template-member/app.hs
