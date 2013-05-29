What is fficxx?
===============

fficxx is an automatic haskell Foreign Function Interface (FFI) generator to C++. While haskell has a well-specified standard for C FFI, making haskell-C++ FFI support  is an arbitrary and painful process. Since Object-Oriented Programming (OOP) paradigm and Functional Programming (FP) paradigm are different, automatic translation of C++ libraries to haskell libraries is not a straightforward task. The goal of fficxx is to minimize this disparity and efforts and maximize user's convenience by providing familiar interface to the original C++ library as a result. 

One of the projects that uses fficxx is HROOT (haskell binding to the ROOT framework).  [HROOT-generate](wavewave/HROOT-generate) is a haskell script using fficxx that generates HROOT packages (HROOT-core, HROOT-hist, HROOT-graf, HROOT-io, HROOT-math and umbrella package HROOT). Once generated, each package can be directly installable as a cabal package. Currently, C++ interface is defined as a haskell data structure as one can see, for
example, in HROOT-generate/lib/HROOT/Data/Core/Class. At this moment, automatic generation from C++ code is not supported yet, but it is planned to be supported. 

fficxx has two components: 

* fficxx : FFI binding generator library
* fficxx-runtime : runtime modules needed for various common routines 

Haskell packages that are generated from fficxx will be dependent on fficxx-runtime. 

