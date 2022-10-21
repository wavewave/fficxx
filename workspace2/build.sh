rm -rf dist-newstyle
rm -rf stdcxx
rm -rf tmf-test
rm -rf tmpl-dep-test
rm -rf tmpl-toplevel-test
rm -rf working
cabal new-build fficxx
sleep 1s
cabal exec runhaskell ../stdcxx-gen/Gen.hs
cabal exec runhaskell ../fficxx-multipkg-test/template-dep/Gen.hs ../fficxx-multipkg-test/template-dep/template
cabal exec runhaskell ../fficxx-multipkg-test/template-member/Gen.hs ../fficxx-multipkg-test/template-member/template
cabal exec runhaskell ../fficxx-multipkg-test/template-toplevel/Gen.hs ../fficxx-multipkg-test/template-toplevel/template

cabal build stdcxx

cabal build tmf-test
cabal build tmpl-dep-test
cabal build tmpl-toplevel-test

cabal test fficxx-multipkg-test
