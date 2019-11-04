Getting Started
===============

We provide a very simple sample case in the `fficxx/sample` directory. `fficxx/sample/cxxlib` is a sample C++ library.
Build the C++ library by running
```
> make
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

To test, we provide the code `use_mysample.hs` in `fficxx/sample/mysample-generator`. Note that one need to set `LD_LIBRARY_PATH` (or `DYLD_LIBRARY_PATH` on Mac OS X) in your environment. For example, if you run in `fficxx/sample/mysample-generator`, run the following command:
```
> LD_LIBRARY_PATH=../cxxlib/lib ./use_mysample

A:foo
B:foo
B:foo
bar
```
You should see that the C++ library is successfully called as above.
