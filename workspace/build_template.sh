cabal build fficxx && \
    cabal exec -- runhaskell ../fficxx-multipkg-test/template-member/Gen.hs ../fficxx-multipkg-test/template-member/template && \
    cabal exec -- runhaskell ../fficxx-multipkg-test/template-dep/Gen.hs ../fficxx-multipkg-test/template-dep/template && \
    cabal exec -- runhaskell ../fficxx-multipkg-test/template-toplevel/Gen.hs ../fficxx-multipkg-test/template-toplevel/template && \
    cabal build tmf-test && \
    cabal build tmpl-dep-test && \
    cabal build tmpl-toplevel-test && \
    cabal test fficxx-multipkg-test
