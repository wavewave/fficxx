module CType where

data CTypes = CTString | CTInt | CTDouble 

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

cstring var = (cstring_ , var)
cint    var = (cint_    , var) 
int     var = (int_     , var)
cdouble var = (cdouble_ , var)
double  var = (double_  , var)
