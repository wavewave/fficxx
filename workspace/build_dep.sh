rm -rf dist-newstyle
rm -rf stdcxx
rm -rf tmpl-dep-test
rm -rf working
cabal build fficxx-runtime
cabal build fficxx
sleep 1s
cabal exec runhaskell -- ../stdcxx-gen/Gen.hs
cabal build stdcxx


# template-member
cabal exec runhaskell -- ../fficxx-multipkg-test/template-dep/Gen.hs ../fficxx-multipkg-test/template-dep/template
cabal build tmpl-dep-test
#cabal new-exec -- runhaskell -- -fobject-code -O0 template-member/app.hs

