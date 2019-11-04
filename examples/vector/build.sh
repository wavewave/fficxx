g++ -c stub.cc -I../../stdcxx/csrc -I../../fficxx-runtime/csrc
ghc -c test.hs
ghc test.hs stub.o
