-----------------------------------------------------------------------------
-- |
-- Module      : FFICXX.Generate.Type.Abbreviation
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module FFICXX.Generate.Type.Abbreviation where

{- Naming: Functions that end with a underscore '_' are used for function declarations. -}
-- self_ :: CPPType
-- self_ = SelfType

-- | const char* type
cstring_ :: CPPType
cstring_ = QConst (Ptr CPTChar)

-- | const int type
cint_ :: CPPType
cint_ = QConst CPTInt

int_ :: CPPType
int_ = CPTInt

uint_ :: CPPType
uint_ = CPTUInt

ulong_ :: CPPType
ulong_ = CPTULong

long_ :: CPPType
long_ = CPTLong

culong_ :: CPPType
culong_ = QConst CPTULong

clong_ :: CPPType
clong_ = QConst CPTLong

cchar_ :: CPPType
cchar_ = QConst CPTChar

char_ :: CPPType
char_ = PrimType CPTChar

-- unimplemented
-- short_ :: CPPType
-- short_ = int_

cdouble_ :: CPPType
cdouble_ = QConst CPTDouble

double_ :: CPPType
double_  = CPTDouble

-- doublep_ :: CPPType
-- doublep_ = CT CTDoubleStar NoConst

-- not implemented yet
-- float_ :: CPPType
-- float_ = double_

bool_ :: CPPType
bool_ = CPTBool

void_ :: CPPType
void_ = CPTVoid

-- voidp_ :: CPPType
-- voidp_ = CT CTVoidStar NoConst

-- intp_ :: CPPType
-- intp_ = CT CTIntStar NoConst

-- charpp_ :: CPPType
-- charpp_ = CT CTCharStarStar NoConst


-- star_ :: CTypes -> CPPType
-- star_ t = CT (CPointer t) NoConst

-- cstar_ :: CTypes -> CPPType
-- cstar_ t = CT (CPointer t) Cons

-- self :: String -> (CPPType, String)
-- self var = (self_, var)

makeTypeVar :: CPPType -> String -> (CPPType,String)
makeTypeVar = (,)

-- voidp :: String -> (CPPType,String)
-- voidp = makeTypVar void

cstring :: String -> (CPPType,String)
cstring = makeTypeVar cstring_

cint :: String -> (CPPType,String)
cint = makeTypeVar cint_

int :: String -> (CPPType,String)
int = makeTypeVar int_

uint :: String -> (CPPType,String)
uint = makeTypeVar uint_

{-
long :: String -> (CPPType,String)
long var = (long_, var)

ulong :: String -> (CPPType,String)
ulong var = (ulong_ , var)

clong :: String -> (CPPType,String)
clong var = (clong_, var)

culong :: String -> (CPPType,String)
culong var = (culong_ , var)

cchar :: String -> (CPPType,String)
cchar var = (cchar_ , var)
aIio
char :: String -> (CPPType,String)
char var = (char_ , var)

short :: String -> (CPPType,String)
short = int

cdouble :: String -> (CPPType,String)
cdouble var = (cdouble_ , var)

double :: String -> (CPPType,String)
double  var = (double_  , var)

doublep :: String -> (CPPType,String)
doublep var = (doublep_ , var)

float :: String -> (CPPType,String)
float = double

bool :: String -> (CPPType,String)
bool    var = (bool_    , var)

intp :: String -> (CPPType, String)
intp var = (intp_ , var)

charpp :: String -> (CPPType, String)
charpp var = (charpp_, var)

star :: CTypes -> String -> (CPPType, String)
star t var = (star_ t, var)

cstar :: CTypes -> String -> (CPPType, String)
cstar t var = (cstar_ t, var)
-}

{-
cppclass_ :: Class -> CPPType
cppclass_ c =  CPT (CPTClass c) NoConst

cppclass :: Class -> String -> (CPPType, String)
cppclass c vname = ( cppclass_ c, vname)

cppclassconst :: Class -> String -> (CPPType, String)
cppclassconst c vname = ( CPT (CPTClass c) Const, vname)

cppclassref_ :: Class -> CPPType
cppclassref_ c = CPT (CPTClassRef c) NoConst

cppclassref :: Class -> String -> (CPPType, String)
cppclassref c vname = (cppclassref_ c, vname)
-}
