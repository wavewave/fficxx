#include <functional>
#include <iostream>
#include "function_test.h"
// #include "MacroPatternMatch.h"

void* newFunction1( void (*fp)() ) {
    std::function<void()>* p = new std::function<void()>(fp);
    return static_cast<void*>(p);
}

void callFunction1( void* stdfn ) {
    std::function<void()>* p = static_cast<std::function<void()>*>(stdfn);
    (*p)();
}

void* newFunction2( void (*fp)(int) ) {
    std::function<void(int)>* p = new std::function<void(int)>(fp);
    return static_cast<void*>(p);
}

void callFunction2( void* stdfn, int x ) {
    std::function<void(int)>* p = static_cast<std::function<void(int)>*>(stdfn);
    (*p)(x);
}


void* newFunction3( int (*fp)(int) ) {
    std::function<int(int)>* p = new std::function<int(int)>(fp);
    return static_cast<void*>(p);
}

int callFunction3( void* stdfn, int x ) {
    std::function<int(int)>* p = static_cast<std::function<int(int)>*>(stdfn);
    return (*p)(x);
}

void* newFunction4( int (*fp)(int,char) ) {
    std::function<int(int,char)>* p = new std::function<int(int,char)>(fp);
    return static_cast<void*>(p);
}

int callFunction4( void* stdfn, int x, char y ) {
    std::function<int(int,char)>* p = static_cast<std::function<int(int,char)>*>(stdfn);
    return (*p)(x,y);
}

#define Function_new_inst(NAME,R,...)\
  inline void* Function_new_ ## NAME ( R (*fp)( __VA_ARGS__ ) )\
  {\
    std::function< R ( __VA_ARGS__ )>* p = new std::function< R ( __VA_ARGS__ ) >(fp);\
    return static_cast<void*>(p);\
  }\
  auto a_Function_new_ ## NAME = Function_new_ ## NAME;


#define VARGS(...) __VA_ARGS__

// works only for gcc and clang
#define VARGS1(x,...) x, ## __VA_ARGS__

//#define COMMA ,

//#define CALLARGS(p,ps)                                \
//    IIF(CHECK(UNPAREN(ps))) (p , p COMMA UNPAREN(ps))


// #define HEAD(x,...) x

#define TAIL(x,...) __VA_ARGS__

#define UNPAREN(x) VARGS x

//UNPAREN(ATYPS) )

#define Function_call_inst(NAME,R,ATYPS,AVARS)        \
  inline R Function_call_ ## NAME ( UNPAREN(ATYPS) ) \
  {\
      std::function< R (TAIL ATYPS) >* p = static_cast< std::function< R (TAIL ATYPS) >* >(op); \
    return (*p) AVARS;                                 \
  }\
  auto a_Function_call_ ## NAME = Function_call_ ## NAME;



Function_new_inst(f1,void)

Function_call_inst(f1,void,(void* op),())

Function_new_inst(f2,void)

Function_call_inst(f2,void,(void* op,int x),(x))


//#define TEST(x,...) x, ## __VA_ARGS__

//TEST(void* stdfn)
//TEST(void* stdfn,)

//Function_new_inst(f5,int)

//Function_call_inst(f5,int,(),)
