#include <functional>
#include <iostream>
#include "test.h"


Function_new_inst(f1,void,())

Function_call_inst(f1,void,(void* op),())

Function_delete_inst(f1,void,(void* op))

Function_new_inst(f2,void,(int))

Function_call_inst(f2,void,(void* op,int x),(x))

Function_delete_inst(f2,void,(void* ,int ))

Function_new_inst(f3,int,(int))

Function_call_inst(f3,int,(void* op,int x),(x))

Function_delete_inst(f3,int,(void* op,int x))

Function_new_inst(f4,int,(int,char))

Function_call_inst(f4,int,(void* op,int x,char y),(x,y))

Function_delete_inst(f4,int,(void* op,int x,char y))

Function_new_inst(f5,int,())

Function_call_inst(f5,int,(void* op),())

Function_delete_inst(f5,int,(void* op))
