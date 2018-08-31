#ifdef __cplusplus
extern "C" {
#endif

#include "cpp_magic.h"

#define VARGS(...) __VA_ARGS__

#define TAIL(x,...) __VA_ARGS__

#define UNPAREN(x) VARGS x

#define FIRSTSECOND(X,Y) X Y

#define GETTYPE(X) FIRST X
#define GETVAR(X) SECOND X
#define GETTYPVAR(X) FIRSTSECOND X



#define GETTYPES(x) MAP(GETTYPE,COMMA,UNPAREN(x))
#define GETVARS(x) MAP(GETVAR,COMMA,UNPAREN(x))
#define GETTYPVARS(x) MAP(GETTYPVAR,COMMA,UNPAREN(x))

#define Function_new_decl(NAME,R,ATYPS)                         \
    void* Function_new_ ## NAME ( R (*fp) ( GETTYPES(ATYPS) ));

#define Function_call_decl(NAME,R,ATYPS)                                \
    R Function_call_ ## NAME ( REMOVE_TRAILING_COMMAS( void*, GETTYPES(ATYPS) ) );

#define Function_delete_decl(NAME,R,ATYPS)           \
    void Function_delete_ ## NAME ( void*  );

#define Function_new_inst(NAME,R,ATYPS)                                \
    inline void* Function_new_ ## NAME ( R (*fp)( GETTYPES(ATYPS) ) )   \
    {                                                                   \
        std::function< R ( GETTYPES(ATYPS) )>* p = new std::function< R ( GETTYPES(ATYPS) ) >(fp); \
        return static_cast<void*>(p);                                   \
    }                                                                   \
    auto a_Function_new_ ## NAME = Function_new_ ## NAME;


#define Function_call_inst(NAME,R,ATYPS)        \
    inline R Function_call_ ## NAME ( REMOVE_TRAILING_COMMAS( void* op,  GETTYPVARS(ATYPS) ) ) \
    {                                                                   \
        std::function< R ( GETTYPES(ATYPS) )>* p = static_cast< std::function< R ( GETTYPES(ATYPS) )>* >(op); \
        return (*p) ( GETVARS(ATYPS) );                              \
    }                                                                   \
    auto a_Function_call_ ## NAME = Function_call_ ## NAME;

#define Function_delete_inst(NAME,R,ATYPS)             \
    inline void Function_delete_ ## NAME ( void* op )   \
    {                                                                   \
        std::function< R ( GETTYPES(ATYPS) ) >* p = static_cast< std::function< R ( GETTYPES(ATYPS) ) >* >(op); \
        delete p;                                                       \
    }                                                                   \
    auto a_Function_delete_ ## NAME = Function_delete_ ## NAME;


#define Function(NAME,R,ATYPS) \
    extern "C" { \
        Function_new_decl(NAME,R,ATYPS)         \
        Function_call_decl(NAME,R,ATYPS)        \
        Function_delete_decl(NAME,R,ATYPS)      \
    }                                           \
    Function_new_inst(NAME,R,ATYPS)             \
    Function_call_inst(NAME,R,ATYPS)            \
    Function_delete_inst(NAME,R,ATYPS)

#ifdef __cplusplus
}
#endif
