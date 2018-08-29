#include "Function.h"

#ifdef __cplusplus
extern "C" {
#endif // __cplusplus

Function_new_decl(f1,void,())

Function_call_decl(f1,void,(void* op))

Function_delete_decl(f1)

Function_new_decl(f2,void,(int x))

Function_call_decl(f2,void,(void* op,int x))

Function_delete_decl(f2)

Function_new_decl(f3,int,(int x))

Function_call_decl(f3,int,(void* op,int x))

Function_delete_decl(f3)

Function_new_decl(f4,int,(int x,char y))

Function_call_decl(f4,int,(void* op,int x,char y))

Function_delete_decl(f4)

Function_new_decl(f5,int,())

Function_call_decl(f5,int,(void* op))

Function_delete_decl(f5)


#ifdef __cplusplus
}
#endif // __cplusplus
