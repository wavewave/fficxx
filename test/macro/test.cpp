//#include "Cloak/cloak.h"
#include "uSHET/lib/cpp_magic.h"

#define GETTYPE(X) FIRST X
#define GETVAR(X) SECOND X

MAP(GETTYPE, COMMA, (int,x), (char*,y), (A,z) )
MAP(GETVAR, COMMA, (int,x), (char*,y), (A,z) )


#define VARGS(...) __VA_ARGS__

#define TAIL(x,...) __VA_ARGS__

#define UNPAREN(x) VARGS x


#define Function_new_decl(NAME,R,ATYPS)\
    void* Function_new_ ## NAME ( R (*fp) ( UNPAREN(ATYPS) ));

#define Function_call_decl(NAME,R,ATYPS)                                \
    R Function_call_ ## NAME ( REMOVE_TRAILING_COMMAS( void*, UNPAREN(ATYPS) ) );

#define Function_delete_decl(NAME,R,ATYPS)           \
    void Function_delete_ ## NAME ( void*  );

#define Function_new_inst(NAME,R,ATYPS,AVARS)                                \
    inline void* Function_new_ ## NAME ( R (*fp)( UNPAREN(ATYPS) ) )    \
    {                                                                   \
        std::function< R ( UNPAREN(ATYPS) )>* p = new std::function< R ( UNPAREN(ATYPS) ) >(fp); \
        return static_cast<void*>(p);                                   \
    }                                                                   \
    auto a_Function_new_ ## NAME = Function_new_ ## NAME;


#define Function_call_inst(NAME,R,ATYPS,AVARS)        \
    inline R Function_call_ ## NAME ( REMOVE_TRAILING_COMMAS( void* op,  UNPAREN(ATYPS) ) ) \
    {                                                                   \
        std::function< R (UNPAREN(ATYPS)) >* p = static_cast< std::function< R (UNPAREN(ATYPS)) >* >(op); \
        return (*p) AVARS;                                              \
    }                                                                   \
    auto a_Function_call_ ## NAME = Function_call_ ## NAME;

#define Function_delete_inst(NAME,R,ATYPS,AVARS)             \
    inline void Function_delete_ ## NAME ( void* op )   \
    {                                                                   \
        std::function< R (UNPAREN(ATYPS)) >* p = static_cast< std::function< R (UNPAREN(ATYPS)) >* >(op); \
        delete p;                                                       \
    }                                                                   \
    auto a_Function_delete_ ## NAME = Function_delete_ ## NAME;


Function_new_decl(f1,void,())

Function_call_decl(f1,void,())

Function_delete_decl(f1,void,())


Function_new_decl(f4,int,(int,char))

Function_call_decl(f4,int,(int,char))

Function_delete_decl(f4,int,(int,char))

Function_new_inst(f4,int,(int,char),(x,y))

Function_call_inst(f4,int,(int,char),(x,y))

Function_delete_inst(f4,int,(int,char),(x,y))


/*
Function_new_decl(f5,int,())

Function_call_decl(f5,int,(void* op))

Function_delete_decl(f5)

Function_new_inst(f5,int,())

Function_call_inst(f5,int,(void* op),())

Function_delete_inst(f5,int,(void* op))
*/

VARGS()
// MAP( GETTYPE,COMMA )
