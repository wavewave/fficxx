#ifdef __cplusplus
extern "C" { 
#endif

#ifndef $typemacro$
#define $typemacro$

#undef ROOT_TYPE_DECLARATION 
#define ROOT_TYPE_DECLARATION(Type) \\
typedef struct Type ##_tag Type ## _t; \\
typedef Type ## _t * Type ## _p; \\
typedef Type ## _t const* const_ ## Type ## _p 

$typeDeclBody$

#endif // $typemacro$

#ifdef __cplusplus
}
#endif