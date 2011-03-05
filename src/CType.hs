module CType where

data CTypes = CTString | CTInt | CTDouble | CTBool 

data CPPTypes = CPTClass String 

data IsConst = Const | NoConst

data Types = Void 
           | SelfType
           | CT  CTypes IsConst 
           | CPT CPPTypes IsConst

cstring_ = CT CTString Const 
cint_    = CT CTInt    Const
int_     = CT CTInt    NoConst
cdouble_ = CT CTDouble Const
double_  = CT CTDouble NoConst
bool_    = CT CTBool   NoConst 
void_ = Void 


cstring var = (cstring_ , var)
cint    var = (cint_    , var) 
int     var = (int_     , var)
cdouble var = (cdouble_ , var)
double  var = (double_  , var)
bool    var = (bool_    , var)

cppclass name =  CPT (CPTClass name) NoConst

hsCTypeName :: CTypes -> String 
hsCTypeName CTString = "CString" 
hsCTypeName CTInt    = "CInt"
hsCTypeName CTDouble = "CDouble"
hsCTypeName CTBool   = "CInt"

hsCppTypeName (CPTClass name) =  "(Ptr Raw"++name++")"  

