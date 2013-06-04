What is fficxx?
===============

fficxx is an automatic haskell Foreign Function Interface (FFI) generator to C++. While haskell has a well-specified standard for C FFI, making haskell-C++ FFI is an arbitrary and painful process. Since Object-Oriented Programming (OOP) paradigm and Functional Programming (FP) paradigm are different, automatic translation of C++ libraries to haskell libraries is not a straightforward task. The goal of fficxx is to minimize this disparity and maximize user's convenience by providing familiar interface to the original C++ library as a result. 

One of the projects that successfully uses fficxx is [HROOT](http://ianwookim.org/HROOT) which is a haskell binding to the [ROOT](http://root.cern.ch) library. A haskell script called [HROOT-generate](http://github.com/wavewave/HROOT-generate) using fficxx generates HROOT packages. Once generated, each package can be directly installable as a cabal package. Currently, C++ interface is defined as a haskell data structure as one can see, for example, in the module [HROOT.Data.Core.Class](https://github.com/wavewave/HROOT-generate/blob/master/lib/HROOT/Data/Core/Class.hs). At this moment, automatic generation from C++ code is not supported yet, but it is planned to be supported. 

fficxx is separated into generator part and runtime part: 

* fficxx : FFI types and binding generator library
* fficxx-runtime : runtime modules needed for various common routines 

Haskell packages that are generated from fficxx will be dependent on fficxx-runtime. 


Getting Started
===============

We provide a very simple sample case in the `fficxx/sample` directory. `fficxx/sample/cxxlib` is a sample C++ library. 
Build the C++ library by 
```
> sh ./build.sh
```
in the directory. 

fficxx/sample/mysample-generator has a haskell code for generating haskell cabal package for binding to the C++ library. You can start code generation by compling MySampleGen.hs
```
> ghc MySampleGen.hs
```
then run it 
```
> ./MySampleGen
```
and then it generates a `MySample` package in the `MySample` directory which is installable with 
`cabal install`. Note that the generated `MySample.cabal` file has the absolute path for the `cxxlib/include` and `cxxlib/lib`. Later, one can change this to an appropriate path. 

To test, we provide the code `use_mysample.hs`. Note that one need to set `LD_LIBRARY_PATH` (or `DYLD_LIBRARY_PATH` on Mac OS X) in your environment. For example, if you run in `fficxx/mysample-generator`, 
```
> LD_LIBRARY_PATH=../cxxlib/lib ./use_mysample 
``` 
You should see that the C++ library is successfully called. 


Object-Oriented Programming Model in fficxx
===========================================

To be explained 
