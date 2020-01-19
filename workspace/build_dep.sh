rm -rf dist-newstyle
rm -rf stdcxx
rm -rf tmpl-dep-test
rm -rf working
cabal new-build fficxx-runtime
cabal new-build fficxx
sleep 1s
cabal new-exec runhaskell ../stdcxx-gen/Gen.hs
cabal new-build stdcxx


# template-member
runhaskell ../fficxx-multipkg-test/template-dep/Gen.hs ../fficxx-multipkg-test/template-dep/template
cabal new-build tmpl-dep-test
#cabal new-exec -- runhaskell -- -fobject-code -O0 template-member/app.hs

