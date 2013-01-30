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

#endif // __MACROPATTERNMATCH__ 

