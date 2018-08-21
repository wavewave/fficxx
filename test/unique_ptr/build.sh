g++ -c stub.cc -I../../stdcxx/csrc
ghc -c test.hs
ghc test.hs stub.o
