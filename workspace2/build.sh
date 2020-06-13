rm -rf dist-newstyle
rm -rf stdcxx
rm -rf tmf-test
rm -rf tmpl-dep-test
rm -rf tmpl-toplevel-test
rm -rf working
cabal new-build fficxx
sleep 1s
cabal new-exec runhaskell ../stdcxx-gen/Gen.hs
cabal new-exec runhaskell ../fficxx-multipkg-test/template-dep/Gen.hs ../fficxx-multipkg-test/template-dep/template
cabal new-exec runhaskell ../fficxx-multipkg-test/template-member/Gen.hs ../fficxx-multipkg-test/template-member/template
cabal new-exec runhaskell ../fficxx-multipkg-test/template-toplevel/Gen.hs ../fficxx-multipkg-test/template-toplevel/template

cabal new-build stdcxx

cabal new-build tmf-test
cabal new-build tmpl-dep-test
cabal new-build tmpl-toplevel-test

cabal new-test fficxx-multipkg-test
