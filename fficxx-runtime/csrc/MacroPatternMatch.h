#ifndef __MACROPATTERNMATCH__
#define __MACROPATTERNMATCH__ 


#define CAT(a,...) PRIMITIVE_CAT(a, __VA_ARGS__ )
#define PRIMITIVE_CAT(a,...) a ## __VA_ARGS__ 

#define IIF(c) PRIMITIVE_CAT(IIF_, c)
#define IIF_0(t, ...) __VA_ARGS__
#define IIF_1(t, ...) t 

#define CHECK_N(x, n, ... ) n 
#define CHECK(...) CHECK_N (__VA_ARGS__, 0, ) 
#define PROBE(x) x, 1,

#define IS_PAREN(x) CHECK(IS_PAREN_PROBE x)
#define IS_PAREN_PROBE(...) PROBE(~)



template<class ToType, class FromType>
const ToType* to_const(const FromType* x) {
  return reinterpret_cast<const ToType*>(x);
}

template<class ToType, class FromType>
ToType* to_nonconst(FromType* x) {
  return reinterpret_cast<ToType*>(x);
}

template<class ToType, class FromType>
const ToType& to_constref(const FromType& x) {
  return reinterpret_cast<const ToType&>(x);
}

template<class ToType, class FromType>
ToType& to_nonconstref(FromType& x) {
  return reinterpret_cast<ToType&>(x);
}


#endif // __MACROPATTERNMATCH__ 

