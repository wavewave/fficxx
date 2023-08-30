OOP Model in fficxx
===================

fficxx generates haskell codes at raw level (C wrapper and haskell foreign pointer  that are directly matched with C/C++ types) and at high level (newtype wrapper for raw level pointer and haskell typeclasses reflecting OOP class hierarchy). Haskell does not have a concept of subtyping, i.e. we do not provide any easy way to create a new subclass and overload member functions from existing classes on haskell side. However, fortunately, one can describe the OOP subclass relationship using a typeclass interface as a contract for a class `b` to be equal to or a subclass of `a`. In a sense, typeclasses are Interface Definition Language (IDL) for describing OOP classes. Thus, a C++ class is represented by both haskell concrete type (for a C++ class itself) and typeclass (a set of classes that can be equal to or a subclass of the C++ class).

Assuming that there is a C++ class `A`. fficxx generates a haskell type `A`. This haskell type `A` is nothing but a newtype wrapping `ForeignPtr` tagged by `RawA`.
```
data RawA

newtype A = A (ForeignPtr RawA)
```
`RawA` exists only for the purpose of a phantom type to be used as tags for `Ptr` in FFI imports (`foreign import` statements.) When programming with fficxx-generated code at high level, programmers should seldom encounter `Raw` types. An instance object of C++ class `A` is a value of haskell type `A`. To create an instance, fficxx provides a smart constructor (`newA`) if specified with a corresponding constructor.

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
