#ifndef __MACROPATTERNMATCH__
#define __MACROPATTERNMATCH__


#define FXCAT(a,...) FXPRIMITIVE_CAT(a, __VA_ARGS__ )
#define FXPRIMITIVE_CAT(a,...) a ## __VA_ARGS__

#define FXIIF(c) FXPRIMITIVE_CAT(FXIIF_, c)
#define FXIIF_0(t, ...) __VA_ARGS__
#define FXIIF_1(t, ...) t

#define FXCHECK_N(x, n, ... ) n
#define FXCHECK(...) FXCHECK_N (__VA_ARGS__, 0, )
#define FXPROBE(x) x, 1,

#define FXIS_PAREN(x) FXCHECK(FXIS_PAREN_PROBE x)
#define FXIS_PAREN_PROBE(...) FXPROBE(~)



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
