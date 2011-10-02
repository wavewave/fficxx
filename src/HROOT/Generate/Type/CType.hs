module HROOT.Generate.Type.CType where

data CTypes = CTString | CTInt | CTDouble | CTBool | CTDoubleStar | CTVoidStar | CTIntStar | CTCharStarStar | CTUInt
            deriving Show 

data CPPTypes = CPTClass String 
              deriving Show

data IsConst = Const | NoConst
             deriving Show

data Types = Void 
           | SelfType
           | CT  CTypes IsConst 
           | CPT CPPTypes IsConst
           deriving Show

self_ :: Types 
self_ = SelfType

cstring_ :: Types
cstring_ = CT CTString Const 

cint_ :: Types
cint_    = CT CTInt    Const

int_ :: Types 
int_     = CT CTInt    NoConst

uint_ :: Types
uint_ = CT CTUInt NoConst

short_ :: Types
short_ = int_

cdouble_ :: Types
cdouble_ = CT CTDouble Const

double_ :: Types
double_  = CT CTDouble NoConst

doublep_ :: Types
doublep_ = CT CTDoubleStar NoConst

float_ :: Types
float_ = double_

bool_ :: Types 
bool_    = CT CTBool   NoConst 

void_ :: Types
void_ = Void 

voidp_ :: Types 
voidp_ = CT CTVoidStar NoConst

intp_ :: Types 
intp_ = CT CTIntStar NoConst

charpp_ :: Types
charpp_ = CT CTCharStarStar NoConst

voidp :: String -> (Types,String) 
voidp var = (voidp_ , var)

cstring :: String -> (Types,String)
cstring var = (cstring_ , var)

cint :: String -> (Types,String)
cint    var = (cint_    , var) 

int :: String -> (Types,String)
int     var = (int_     , var)

uint :: String -> (Types,String)
uint var = (uint_ , var)

short :: String -> (Types,String)
short = int

cdouble :: String -> (Types,String)
cdouble var = (cdouble_ , var)

double :: String -> (Types,String)
double  var = (double_  , var)

doublep :: String -> (Types,String)
doublep var = (doublep_ , var)

float :: String -> (Types,String)
float = double

bool :: String -> (Types,String)
bool    var = (bool_    , var)

intp :: String -> (Types, String) 
intp var = (intp_ , var)

charpp :: String -> (Types, String)
charpp var = (charpp_, var)

cppclass_ :: String -> Types
cppclass_ cname =  CPT (CPTClass cname) NoConst

cppclass :: String -> String -> (Types, String)
cppclass cname vname = ( cppclass_ cname, vname)

cppclassconst :: String -> String -> (Types, String) 
cppclassconst cname vname = ( CPT (CPTClass cname) Const, vname)

hsCTypeName :: CTypes -> String 
hsCTypeName CTString = "CString" 
hsCTypeName CTInt    = "CInt"
hsCTypeName CTUInt   = "CUInt" 
hsCTypeName CTDouble = "CDouble"
hsCTypeName CTDoubleStar = "(Ptr CDouble)"
hsCTypeName CTBool   = "CInt"
hsCTypeName CTVoidStar = "(Ptr ())"
hsCTypeName CTIntStar = "(Ptr CInt)"
hsCTypeName CTCharStarStar = "(Ptr (CString))"



hsCppTypeName :: CPPTypes -> String
hsCppTypeName (CPTClass name) =  "(Ptr Raw"++name++")"  

