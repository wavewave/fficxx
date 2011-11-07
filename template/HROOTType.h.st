#ifdef __cplusplus
extern "C" { 
#endif

#ifndef __HROOTTYPE__
#define __HROOTTYPE__

#undef ROOT_TYPE_DECLARATION 
#define ROOT_TYPE_DECLARATION(Type) \\
typedef struct Type ##_tag Type ## _t; \\
typedef Type ## _t * Type ## _p; \\
typedef Type ## _t const* const_ ## Type ## _p 

$typeDeclBody$

#endif __HROOTTYPE__

#ifdef __cplusplus
}
#endif