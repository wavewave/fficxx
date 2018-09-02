#include <MacroPatternMatch.h>
//#include "TestPkgA.h"
//#include "TestPkgB.h"
#include "testpkgType.h"
#include "test.h"

#define A_method(Type)                                                  \
    extern "C" {                                                        \
        void A_method_##Type ( A_p p, Type##_p x );                     \
    }                                                                   \
    inline void A_method_##Type ( A_p p, Type##_p x )                    \
    {                                                                   \
        (to_nonconst<A,A_t>(p))->method<Type>(to_nonconst<Type,Type##_t>(x));  \
    }                                                                   \
    auto a_A_method_##Type = A_method_##Type  ;




A_method(T1)

A_method(T2)
