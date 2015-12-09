What is fficxx?
===============

fficxx ("eff fix") is an automatic haskell Foreign Function Interface (FFI) generator to C++.

To use fficxx, you write a Haskell model of the C++ public interfaces and fficxx generates both a C wrapper and associated haskell functions and type classes which reflect specified model of the C++ interfaces. It is currently the user's responsibility to specify a correct model of the C++ interfaces, because fficxx does not presently check for model correctness.

While haskell has a well-specified standard for C FFI, making haskell-C++ FFI is an arbitrary and painful process. Since Object-Oriented Programming (OOP) paradigm and Functional Programming (FP) paradigm are different, automatic translation of C++ libraries to haskell libraries is not a straightforward task. The goal of fficxx is to minimize this disparity and maximize user's convenience by providing familiar interface to the original C++ library as a result.

One of the projects that successfully uses fficxx is [HROOT](http://ianwookim.org/HROOT) which is a haskell binding to the [ROOT](http://root.cern.ch) library. A haskell script called [HROOT-generate](http://github.com/wavewave/HROOT-generate) using fficxx generates HROOT packages. Once generated, each package can be directly installable as a cabal package. Currently, C++ interface is defined as a haskell data structure as one can see, for example, in the module [HROOT.Data.Core.Class](https://github.com/wavewave/HROOT-generate/blob/master/lib/HROOT/Data/Core/Class.hs). At this moment, automatic generation from C++ code is not supported yet, but it is planned to be supported.

fficxx is separated into generator part and runtime part:

* fficxx : FFI types and binding generator library
* fficxx-runtime : runtime modules needed for various common routines

Haskell packages that are generated from fficxx will be dependent on fficxx-runtime.


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


OOP Model in fficxx
===================

fficxx generates haskell codes at raw level (C wrapper and haskell foreign pointer  that are directly matched with C/C++ types) and at high level (newtype wrapper for raw level pointer and haskell typeclasses reflecting OOP class hierarchy). As a pure functional programming language, haskell does not have a concept of subtyping, but typeclasses can be regarded as interface description language for describing OOP classes. This means that we do not provide any easy way to create a new subclass and overload member functions from existing classes on haskell side, but at least, for a given class, a typeclass interface is given to describe a contract for a to be equal to or a subclass of the class. For this purpose, we separate haskell type corresponding to a class from haskell typeclass corresponding to a set of classes that can be equal to or a subclass of the class.

Assuming that there is a C++ class `A`. fficxx generates a haskell type `A`. This haskell type `A` is nothing but a newtype wrapping `ForeignPtr` tagged by `RawA`. `RawA` exists only for the purpose of a phantom type to be used as tags for `Ptr` in FFI imports (`foreign import` statements.) When programming with fficxx-generated code at high level, programmers should seldom encounter `Raw` types. An instance object of C++ class `A` is a value of haskell type `A`. To create an instance, ffixx provides a smart constructor (`newA`) if specified with a corresponding constructor.

Therefore, one can create an instance from a concrete haskell type, and pass it to any functions which needs them. On Haskell side, member functions of a C++ class are nothing but functions whose first argument is the same as the corresponding haskell type to the class. For example, if the class `A` has a member function `foo` of signature `void A::foo( int param )`, then fficxx generates a high level function
```
foo :: A -> CInt -> IO () 
```
which is a wrapper for a raw level FFI call defined by
```
foriegn import ccall "A_foo" c_foo :: Ptr RawA -> CInt -> IO ()
```
where `A_foo` is a generated C shim function for `A::foo`. So one can translate the following C++ code
```
A* a = new A();
a->foo(3);
```
to the haskell code (in do-block of IO monad)
```
do a <- newA
   foo a 3
```
Haskell type `A` can be used in the arbitrary argument position. Assume there is another member function `bar` of `A` which takes an object of `A` as an argument like `void A::bar( A* a )`. Then, we have
```
bar :: A -> A -> IO ()
```
for which `x->bar(y)` (`x`,`y` are of class `A`) corresponds to `bar x y`.

In this example, the C++ class `A` may have the following declaration:
```
class A
{
  public:
    A();
    virtual void foo( int );
    virtual void bar( A* ); 
};
```
To reflect subtype relations, fficxx creates an interface typeclass `IA` for `A`, which is defined as
```
class IA a where
  foo :: a -> CInt -> IO ()
  bar :: (IA b) => a -> b -> IO ()
```
which declares all C++ virtual functions as members. Then, haskell type `A` is a typeclass instance of `IA`:
```
instance IA A where
  -- foo :: A -> CInt -> IO ()
  foo = ...
  -- bar :: (IA b) => A -> b -> IO ()
  bar = ...
```
so that `foo` and `bar` functions we considered in the above example were actually defined in the `IA` instance definition of A.
Note that the type signature of `bar` allows generic typeclass instances of `IA` as the argument (paraterized by `b`).

Now consider another C++ class `B` which is a subclass of `A`:
```
class B : public A
{
  public:
    B();
    virtual int baz() ;
}
```
Again, we will have a concrete haskell type `B` and an object of `B` will be created as a value from the `newB` constructor function. 
A typeclass `IB` is also generated as well, and it reflects the inheritance relationship for C++ class as constraints:
```
class (IA b) => IB b where
  baz :: b -> IO CInt
```
Thanks to the constraints `(IA b) =>` in the declaration, every instance of `IB` must have implementation of `IA`. This is true for `B`, too.
So fficxx generates
```
instance IA B where
  foo = ...
  bar = ...

instance IB B where
  baz = ...
```
This instance generation (*implemenation of C++ class*) is automaticaly done by fficxx, but it's not guaranteed for future subclassing. Any type which implements instances of `IA` and `IB` can be regarded as a subclass of `B`, but it's not automatically done as we have in OOP. The scope of fficxx is to generate such implementations only for existing C++ classes.   


Real World Usage
================

For the time being, to see how to use generated haskell library, check examples for HROOT : [http://ianwookim.org/HROOT/gallery.html](http://ianwookim.org/HROOT/gallery.html) (if you click examples, then you can see the source code for them. )



