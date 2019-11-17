{-# LANGUAGE RecordWildCards #-}
module FFICXX.Generate.Code.Primitive where

import           Control.Monad.Trans.State    ( runState, put, get )
import           Data.Monoid                  ( (<>) )
import           Language.Haskell.Exts.Syntax ( Asst(..), Context, Type(..) )
--
import qualified FFICXX.Runtime.CodeGen.C as R
--
import           FFICXX.Generate.Name
import           FFICXX.Generate.Type.Class
import           FFICXX.Generate.Util
import           FFICXX.Generate.Util.HaskellSrcExts


data CFunSig = CFunSig { cArgTypes :: [Arg]
                       , cRetType :: Types
                       }

data HsFunSig = HsFunSig { hsSigTypes :: [Type ()]
                         , hsSigConstraints :: [Asst ()]
                         }

cvarToStr :: CTypes -> IsConst -> String -> String
cvarToStr ctyp isconst varname = ctypToStr ctyp isconst <> " " <> varname

ctypToStr :: CTypes -> IsConst -> String
ctypToStr ctyp isconst =
  let typword = case ctyp of
        CTBool      -> "bool"
        CTChar      -> "char"
        CTClock     -> "clock_t"
        CTDouble    -> "double"
        CTFile      -> "FILE"
        CTFloat     -> "float"
        CTFpos      -> "fpos_t"
        CTInt       -> "int"
        CTIntMax    -> "intmax_t"
        CTIntPtr    -> "intptr_t"
        CTJmpBuf    -> "jmp_buf"
        CTLLong     -> "long long"
        CTLong      -> "long"
        CTPtrdiff   -> "ptrdiff_t"
        CTSChar     -> "sized char"
        CTSUSeconds -> "suseconds_t"
        CTShort     -> "short"
        CTSigAtomic -> "sig_atomic_t"
        CTSize      -> "size_t"
        CTTime      -> "time_t"
        CTUChar     -> "unsigned char"
        CTUInt      -> "unsigned int"
        CTUIntMax   -> "uintmax_t"
        CTUIntPtr   -> "uintptr_t"
        CTULLong    -> "unsigned long long"
        CTULong     -> "unsigned long"
        CTUSeconds  -> "useconds_t"
        CTUShort    -> "unsigned short"
        CTWchar     -> "wchar_t"
        CTInt8      -> "int8_t"
        CTInt16     -> "int16_t"
        CTInt32     -> "int32_t"
        CTInt64     -> "int64_t"
        CTUInt8     -> "uint8_t"
        CTUInt16    -> "uint16_t"
        CTUInt32    -> "uint32_t"
        CTUInt64    -> "uint64_t"
        CTString    -> "char*"
        CTVoidStar  -> "void*"
        CEnum _ type_str -> type_str
        CPointer s  -> ctypToStr s NoConst <> "*"
        CRef s      -> ctypToStr s NoConst <> "*"
  in case isconst of
       Const   -> "const" <> " " <> typword
       NoConst -> typword

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

ulong_ :: Types
ulong_ = CT CTULong NoConst

long_ :: Types
long_ = CT CTLong NoConst

culong_ :: Types
culong_ = CT CTULong Const

clong_ :: Types
clong_ = CT CTLong Const

cchar_ :: Types
cchar_ = CT CTChar Const

char_ :: Types
char_ = CT CTChar NoConst

cshort_ :: Types
cshort_ = CT CTShort Const

short_ :: Types
short_ = CT CTShort NoConst

cdouble_ :: Types
cdouble_ = CT CTDouble Const

double_ :: Types
double_  = CT CTDouble NoConst

doublep_ :: Types
doublep_ = CT (CPointer CTDouble) NoConst

cfloat_ :: Types
cfloat_ = CT CTFloat Const

float_ :: Types
float_ = CT CTFloat NoConst

bool_ :: Types
bool_    = CT CTBool   NoConst

void_ :: Types
void_ = Void

voidp_ :: Types
voidp_ = CT CTVoidStar NoConst

intp_ :: Types
intp_ = CT (CPointer CTInt) NoConst

intref_ :: Types
intref_ = CT (CRef CTInt) NoConst


charpp_ :: Types
charpp_ = CT (CPointer CTString) NoConst

ref_ :: CTypes -> Types
ref_ t = CT (CRef t) NoConst

star_ :: CTypes -> Types
star_ t = CT (CPointer t) NoConst

cstar_ :: CTypes -> Types
cstar_ t = CT (CPointer t) Const

self :: String -> Arg
self var = Arg self_ var

voidp :: String -> Arg
voidp var = Arg voidp_ var

cstring :: String -> Arg
cstring var = Arg cstring_ var

cint :: String -> Arg
cint var = Arg cint_ var

int :: String -> Arg
int var = Arg int_ var

uint :: String -> Arg
uint var = Arg uint_ var

long :: String -> Arg
long var = Arg long_ var

ulong :: String -> Arg
ulong var = Arg ulong_ var

clong :: String -> Arg
clong var = Arg clong_ var

culong :: String -> Arg
culong var = Arg culong_ var

cchar :: String -> Arg
cchar var = Arg cchar_ var

char :: String -> Arg
char var = Arg char_ var

cshort :: String -> Arg
cshort var = Arg cshort_ var

short :: String -> Arg
short var = Arg short_ var

cdouble :: String -> Arg
cdouble var = Arg cdouble_ var

double :: String -> Arg
double  var = Arg double_ var

doublep :: String -> Arg
doublep var = Arg doublep_ var

cfloat :: String -> Arg
cfloat var = Arg float_ var

float :: String -> Arg
float var = Arg float_ var

bool :: String -> Arg
bool var = Arg bool_ var

intp :: String -> Arg
intp var = Arg intp_ var

intref :: String -> Arg
intref var = Arg intref_ var

charpp :: String -> Arg
charpp var = Arg charpp_ var

ref :: CTypes -> String -> Arg
ref t var = Arg (ref_ t) var

star :: CTypes -> String -> Arg
star t var = Arg (star_ t) var

cstar :: CTypes -> String -> Arg
cstar t var = Arg (cstar_ t) var


cppclass_ :: Class -> Types
cppclass_ c =  CPT (CPTClass c) NoConst

cppclass :: Class -> String -> Arg
cppclass c vname = Arg (cppclass_ c) vname



cppclassconst :: Class -> String -> Arg
cppclassconst c vname = Arg (CPT (CPTClass c) Const) vname

cppclassref_ :: Class -> Types
cppclassref_ c = CPT (CPTClassRef c) NoConst

cppclassref :: Class -> String -> Arg
cppclassref c vname = Arg (cppclassref_ c) vname

cppclasscopy_ :: Class -> Types
cppclasscopy_ c = CPT (CPTClassCopy c) NoConst

cppclasscopy :: Class -> String -> Arg
cppclasscopy c vname = Arg (cppclasscopy_ c) vname

cppclassmove_ :: Class -> Types
cppclassmove_ c = CPT (CPTClassMove c) NoConst

cppclassmove :: Class -> String -> Arg
cppclassmove c vname = Arg (cppclassmove_ c) vname


argToCTypVar :: Arg -> (R.CType,R.CName)
argToCTypVar (Arg (CT ctyp isconst) varname) =
  (R.CType (ctypToStr ctyp isconst), R.sname varname)
argToCTypVar (Arg SelfType varname) =
  (R.CType "Type ## _p", R.sname varname)
argToCTypVar (Arg (CPT (CPTClass c) isconst) varname) =
  case isconst of
    Const   -> (R.CType ("const_" <> cname <> "_p"), R.sname varname)
    NoConst -> (R.CType (cname <> "_p"), R.sname varname)
  where cname = ffiClassName c
argToCTypVar (Arg (CPT (CPTClassRef c) isconst) varname) =
  case isconst of
    Const   -> (R.CType ("const_" <> cname <> "_p"), R.sname varname)
    NoConst -> (R.CType (cname <> "_p"), R.sname varname)
  where cname = ffiClassName c
argToCTypVar (Arg (CPT (CPTClassCopy c) isconst) varname) =
  case isconst of
    Const   -> (R.CType ("const_" <> cname <> "_p"), R.sname varname)
    NoConst -> (R.CType (cname <> "_p"), R.sname varname)
  where cname = ffiClassName c
argToCTypVar (Arg (CPT (CPTClassMove c) isconst) varname) =
  case isconst of
    Const   -> (R.CType ("const_" <> cname <> "_p"), R.sname varname)
    NoConst -> (R.CType (cname <> "_p"), R.sname varname)
  where cname = ffiClassName c
argToCTypVar (Arg (TemplateApp     _) varname) = (R.CType "void*", R.sname varname)
argToCTypVar (Arg (TemplateAppRef  _) varname) = (R.CType "void*", R.sname varname)
argToCTypVar (Arg (TemplateAppMove _) varname) = (R.CType "void*", R.sname varname)
argToCTypVar t = error ("argToCTypVar: " <> show t)

argsToCTypVar :: [Arg] -> [(R.CType,R.CName)]
argsToCTypVar args =
  let args' = (Arg SelfType "p") : args
  in map argToCTypVar args'

argsToCTypVarNoSelf :: [Arg] -> [(R.CType,R.CName)]
argsToCTypVarNoSelf = map argToCTypVar



-- TODO: remove this function
argToCallString :: Arg -> String
argToCallString (Arg t e) = castC2Cpp t e


argsToCallString :: [Arg] -> String
argsToCallString = intercalateWith conncomma argToCallString

-- TODO: rename this function by castExpressionFrom/To or something like that.
rettypeToString :: Types -> String
rettypeToString (CT ctyp isconst)        = ctypToStr ctyp isconst
rettypeToString Void                     = "void"
rettypeToString SelfType                 = "Type ## _p"
rettypeToString (CPT (CPTClass c) _)     = ffiClassName c <> "_p"
rettypeToString (CPT (CPTClassRef c) _)  = ffiClassName c <> "_p"
rettypeToString (CPT (CPTClassCopy c) _) = ffiClassName c <> "_p"
rettypeToString (CPT (CPTClassMove c) _) = ffiClassName c <> "_p"
rettypeToString (TemplateApp     _)      = "void*"
rettypeToString (TemplateAppRef  _)      = "void*"
rettypeToString (TemplateAppMove _)      = "void*"
rettypeToString (TemplateType _)         = "void*"
rettypeToString (TemplateParam _)        = "Type ## _p"
rettypeToString (TemplateParamPointer _) = "Type ## _p"



-- TODO: Rewrite this with static_cast
castC2Cpp :: Types -> String -> String
castC2Cpp t e =
  case t of
    CT  (CRef _)         _ -> "(*"<> e <> ")"
    CPT (CPTClass     c) _ -> "to_nonconst<" <> f <> "," <> f <> "_t>(" <> e <> ")"
                              where f = ffiClassName c
    CPT (CPTClassRef  c) _ -> "to_nonconstref<" <> f <> "," <> f <> "_t>(*" <> e <> ")"
                              where f = ffiClassName c
    CPT (CPTClassCopy c) _ -> "*(to_nonconst<" <> f <> "," <> f <> "_t>(" <> e <> "))"
                              where f = ffiClassName c
    CPT (CPTClassMove c) _ -> "std::move(to_nonconstref<" <> f <> "," <> f<> "_t>(*" <> e <> "))"
                              where f = ffiClassName c
    TemplateApp    p  -> "to_nonconst<" <> tapp_CppTypeForParam p <> ",void>(" <> e <> ")"
    TemplateAppRef p  -> "*( (" <> tapp_CppTypeForParam p <> "*) " <> e <> ")"
    TemplateAppMove p -> "std::move(*( (" <> tapp_CppTypeForParam p <> "*) " <> e <> "))"
    _                 -> e


-- TODO: Rewrite this with static_cast
--       Merge this with returnCpp after Void and simple type adjustment
castCpp2C :: Types -> String -> String
castCpp2C t e =
  case t of
    Void                   -> ""
    SelfType               -> "to_nonconst<Type ## _t, Type>((Type *)" <> e <> ")"
    CT (CRef _) _          -> "&(" <> e <> ")"
    CT _ _                 -> e
    CPT (CPTClass c) _     -> "to_nonconst<" <> f <> "_t," <> f <> ">((" <> f <> "*)" <> e <> ")"
                               where f = ffiClassName c
    CPT (CPTClassRef c) _  -> "to_nonconst<" <> f <> "_t," <> f <> ">(&(" <> e <> "))"
                               where f = ffiClassName c
    CPT (CPTClassCopy c) _ -> "to_nonconst<" <> f <> "_t," <> f <> ">(new " <> f <> "(" <> e <> "))"
                               where f = ffiClassName c
    CPT (CPTClassMove c) _ -> "std::move(to_nonconst<" <> f <> "_t," <> f <>">(&(" <> e <> ")))"
                               where f = ffiClassName c
    TemplateApp _          -> error "castCpp2C: TemplateApp"
                              -- g <> "* r = new " <> g <> "(" <> e <> "); "
                              --  <> "return (static_cast<void*>(r));"
    TemplateAppRef _       -> error "castCpp2C: TemplateAppRef"
                              -- g <> "* r = new " <> g <> "(" <> e <> "); "
                              -- <> "return (static_cast<void*>(r));"
    TemplateAppMove _      -> error "castCpp2C: TemplateAppMove"
    TemplateType _         -> error "castCpp2C: TemplateType"
    TemplateParam _        -> error "castCpp2C: TemplateParam"
                              -- if b then e
                              --      else "to_nonconst<Type ## _t, Type>((Type *)&(" <> e <> "))"
    TemplateParamPointer _ -> error "castCpp2C: TemplateParamPointer"
                              -- if b then "(" <> callstr <> ");"
                              --      else "to_nonconst<Type ## _t, Type>(" <> e <> ") ;"

tmplArgToCTypVar :: Bool -> TemplateClass -> Arg -> (R.CType,R.CName)
tmplArgToCTypVar _ _  (Arg (CT ctyp isconst) varname) =
  (R.CType (ctypToStr ctyp isconst), R.sname varname)
tmplArgToCTypVar _ t (Arg SelfType varname) =
  (R.CType (tclass_oname t <> "*"), R.sname varname)
tmplArgToCTypVar _ _ (Arg (CPT (CPTClass c) isconst) varname) =
  case isconst of
    Const   -> (R.CType ("const_" <> ffiClassName c <> "_p"), R.sname varname)
    NoConst -> (R.CType (ffiClassName c <> "_p"), R.sname varname)
tmplArgToCTypVar _ _ (Arg (CPT (CPTClassRef c) isconst) varname) =
  case isconst of
    Const   -> (R.CType ("const_" <> ffiClassName c <> "_p"), R.sname varname)
    NoConst -> (R.CType (ffiClassName c <> "_p"), R.sname varname)
tmplArgToCTypVar _ _ (Arg (CPT (CPTClassMove c) isconst) varname) =
  case isconst of
    Const   -> (R.CType ("const_" <> ffiClassName c <> "_p"), R.sname varname)
    NoConst -> (R.CType (ffiClassName c <> "_p"), R.sname varname)
tmplArgToCTypVar _ _ (Arg (TemplateApp     _) v) = (R.CType "void*", R.sname v)
tmplArgToCTypVar _ _ (Arg (TemplateAppRef  _) v) = (R.CType "void*", R.sname v)
tmplArgToCTypVar _ _ (Arg (TemplateAppMove _) v) = (R.CType "void*", R.sname v)
tmplArgToCTypVar _ _ (Arg (TemplateType    _) v) = (R.CType "void*", R.sname v)
tmplArgToCTypVar True  _ (Arg (TemplateParam _) v) = (R.CType "Type", R.sname v)
tmplArgToCTypVar False _ (Arg (TemplateParam _) v) = (R.CType "Type ## _p", R.sname v)
tmplArgToCTypVar True  _ (Arg (TemplateParamPointer _) v) = (R.CType "Type", R.sname v)
tmplArgToCTypVar False _ (Arg (TemplateParamPointer _) v) = (R.CType "Type ## _p", R.sname v)
tmplArgToCTypVar _ _ _ = error "tmplArgToCTypVar: undefined"

tmplAllArgsToCTypVar ::
     Bool
  -> Selfness
  -> TemplateClass
  -> [Arg]
  -> [(R.CType,R.CName)]
tmplAllArgsToCTypVar b s t args =
  let args' = case s of
                Self   -> (Arg (TemplateType t) "p") : args
                NoSelf -> args
  in map (tmplArgToCTypVar b t) args'



tmplArgToCallString
  :: Bool  -- ^ is primitive type?
  -> Arg
  -> String
tmplArgToCallString _ (Arg (CPT (CPTClass c) _) varname) =
  -- TODO: Rewrite this with static_cast.
  "to_nonconst<"<>str<>","<>str<>"_t>("<>varname<>")" where str = ffiClassName c
tmplArgToCallString _ (Arg (CPT (CPTClassRef c) _) varname) =
  -- TODO: Rewrite this with static_cast.
  "to_nonconstref<"<>str<>","<>str<>"_t>(*"<>varname<>")" where str = ffiClassName c
tmplArgToCallString _ (Arg (CPT (CPTClassMove c) _) varname) =
  -- TODO: Rewrite this with static_cast.
  "std::move(to_nonconstref<"<>str<>","<>str<>"_t>(*"<>varname<>"))" where str = ffiClassName c
tmplArgToCallString _ (Arg (CT (CRef _) _) varname) = "(*"<> varname<> ")"
tmplArgToCallString _ (Arg (TemplateApp x) varname) =
  case tapp_tparam x of
    TArg_TypeParam p -> "static_cast<" <> tclass_oname (tapp_tclass x) <> "<Type>*>(" <> varname <> ")"
    _ -> -- TODO: Implement this.
         error "tmplArgToCallString: TemplateApp"
tmplArgToCallString _ (Arg (TemplateAppRef x) varname) =
  case tapp_tparam x of
    TArg_TypeParam p -> "*" <> "(static_cast<" <> tclass_oname (tapp_tclass x) <> "<Type>*>(" <> varname <> "))"
    _ -> -- TODO: Implement this.
         error "tmplArgToCallString: TemplateAppRef"
tmplArgToCallString _ (Arg (TemplateAppMove x) varname) =
  case tapp_tparam x of
    TArg_TypeParam p -> "std::move(*" <> "(static_cast<" <> tclass_oname (tapp_tclass x) <> "<Type>*>(" <> varname <> ")))"
    _ -> -- TODO: Implement this.
         error "tmplArgToCallString: TemplateAppMove"
tmplArgToCallString b (Arg (TemplateParam _) varname) =
  case b of
    True  -> varname
    False -> "*(to_nonconst<Type,Type ## _t>(" <> varname <> "))"
tmplArgToCallString b (Arg (TemplateParamPointer _) varname) =
  case b of
    True  -> varname
    False -> "to_nonconst<Type,Type ## _t>(" <> varname <> ")"
tmplArgToCallString _ (Arg _ varname) = varname

tmplAllArgsToCallString
  :: Bool  -- ^ is primitive type?
  -> [Arg]
  -> String
tmplAllArgsToCallString b = intercalateWith conncomma (tmplArgToCallString b)



tmplRetTypeToString :: Bool   -- ^ is primitive type?
                    -> Types
                    -> String
tmplRetTypeToString _ (CT ctyp isconst)        = ctypToStr ctyp isconst
tmplRetTypeToString _ Void                     = "void"
tmplRetTypeToString _ SelfType                 = "void*"
tmplRetTypeToString _ (CPT (CPTClass c) _)     = ffiClassName c <> "_p"
tmplRetTypeToString _ (CPT (CPTClassRef c) _)  = ffiClassName c <> "_p"
tmplRetTypeToString _ (CPT (CPTClassCopy c) _) = ffiClassName c <> "_p"
tmplRetTypeToString _ (CPT (CPTClassMove c) _) = ffiClassName c <> "_p"
tmplRetTypeToString _ (TemplateApp     _)      = "void*"
tmplRetTypeToString _ (TemplateAppRef  _)      = "void*"
tmplRetTypeToString _ (TemplateAppMove _)      = "void*"
tmplRetTypeToString _ (TemplateType _)         = "void*"
tmplRetTypeToString b (TemplateParam _)        = if b then "Type" else "Type ## _p"
tmplRetTypeToString b (TemplateParamPointer _) = if b then "Type" else "Type ## _p"





-- ---------------------------
-- Template Member Function --
-- ---------------------------

tmplMemFuncArgToCTypVar :: Class -> Arg -> (R.CType, R.CName)
tmplMemFuncArgToCTypVar _ (Arg (CT ctyp isconst) varname) =
  (R.CType (ctypToStr ctyp isconst), R.sname varname)
tmplMemFuncArgToCTypVar c (Arg SelfType varname) =
  (R.CType (ffiClassName c <> "_p"), R.sname varname)
tmplMemFuncArgToCTypVar _ (Arg (CPT (CPTClass c) isconst) varname) =
  case isconst of
    Const   -> (R.CType ("const_" <> ffiClassName c <> "_p"), R.sname varname)
    NoConst -> (R.CType (ffiClassName c <> "_p"), R.sname varname)
tmplMemFuncArgToCTypVar _ (Arg (CPT (CPTClassRef c) isconst) varname) =
  case isconst of
    Const   -> (R.CType ("const_" <> ffiClassName c <> "_p"), R.sname varname)
    NoConst -> (R.CType (ffiClassName c <> "_p"), R.sname varname)
tmplMemFuncArgToCTypVar _ (Arg (CPT (CPTClassMove c) isconst) varname) =
  case isconst of
    Const   -> (R.CType ("const_" <> ffiClassName c <> "_p"), R.sname varname)
    NoConst -> (R.CType (ffiClassName c <> "_p"), R.sname varname)
tmplMemFuncArgToCTypVar _ (Arg (TemplateApp     _) v) = (R.CType "void*", R.sname v)
tmplMemFuncArgToCTypVar _ (Arg (TemplateAppRef  _) v) = (R.CType "void*", R.sname v)
tmplMemFuncArgToCTypVar _ (Arg (TemplateAppMove _) v) = (R.CType "void*", R.sname v)
tmplMemFuncArgToCTypVar _ (Arg (TemplateType   _)  v) = (R.CType "void*", R.sname v)
tmplMemFuncArgToCTypVar _ (Arg (TemplateParam _) v) = (R.CType "Type##_p", R.sname v)
tmplMemFuncArgToCTypVar _ (Arg (TemplateParamPointer _) v) = (R.CType "Type##_p", R.sname v)
tmplMemFuncArgToCTypVar _ _ = error "tmplMemFuncArgToString: undefined"



tmplMemFuncRetTypeToString :: Class -> Types -> String
tmplMemFuncRetTypeToString _ (CT ctyp isconst)        = ctypToStr ctyp isconst
tmplMemFuncRetTypeToString _ Void                     = "void"
tmplMemFuncRetTypeToString c SelfType                 = ffiClassName c <> "_p"
tmplMemFuncRetTypeToString _ (CPT (CPTClass c) _)     = ffiClassName c <> "_p"
tmplMemFuncRetTypeToString _ (CPT (CPTClassRef c) _)  = ffiClassName c <> "_p"
tmplMemFuncRetTypeToString _ (CPT (CPTClassCopy c) _) = ffiClassName c <> "_p"
tmplMemFuncRetTypeToString _ (CPT (CPTClassMove c) _) = ffiClassName c <> "_p"
tmplMemFuncRetTypeToString _ (TemplateApp     _)      = "void*"
tmplMemFuncRetTypeToString _ (TemplateAppRef  _)      = "void*"
tmplMemFuncRetTypeToString _ (TemplateAppMove _)      = "void*"
tmplMemFuncRetTypeToString _ (TemplateType _)         = "void*"
tmplMemFuncRetTypeToString _ (TemplateParam _)        = "Type##_p"
tmplMemFuncRetTypeToString _ (TemplateParamPointer _) = "Type##_p"



-- |
convertC2HS :: CTypes -> Type ()
convertC2HS CTBool      = tycon "CBool"
convertC2HS CTChar      = tycon "CChar"
convertC2HS CTClock     = tycon "CClock"
convertC2HS CTDouble    = tycon "CDouble"
convertC2HS CTFile      = tycon "CFile"
convertC2HS CTFloat     = tycon "CFloat"
convertC2HS CTFpos      = tycon "CFpos"
convertC2HS CTInt       = tycon "CInt"
convertC2HS CTIntMax    = tycon "CIntMax"
convertC2HS CTIntPtr    = tycon "CIntPtr"
convertC2HS CTJmpBuf    = tycon "CJmpBuf"
convertC2HS CTLLong     = tycon "CLLong"
convertC2HS CTLong      = tycon "CLong"
convertC2HS CTPtrdiff   = tycon "CPtrdiff"
convertC2HS CTSChar     = tycon "CSChar"
convertC2HS CTSUSeconds = tycon "CSUSeconds"
convertC2HS CTShort     = tycon "CShort"
convertC2HS CTSigAtomic = tycon "CSigAtomic"
convertC2HS CTSize      = tycon "CSize"
convertC2HS CTTime      = tycon "CTime"
convertC2HS CTUChar     = tycon "CUChar"
convertC2HS CTUInt      = tycon "CUInt"
convertC2HS CTUIntMax   = tycon "CUIntMax"
convertC2HS CTUIntPtr   = tycon "CUIntPtr"
convertC2HS CTULLong    = tycon "CULLong"
convertC2HS CTULong     = tycon "CULong"
convertC2HS CTUSeconds  = tycon "CUSeconds"
convertC2HS CTUShort    = tycon "CUShort"
convertC2HS CTWchar     = tycon "CWchar"
convertC2HS CTInt8      = tycon "Int8"
convertC2HS CTInt16      = tycon "Int16"
convertC2HS CTInt32      = tycon "Int32"
convertC2HS CTInt64      = tycon "Int64"
convertC2HS CTUInt8      = tycon "Word8"
convertC2HS CTUInt16      = tycon "Word16"
convertC2HS CTUInt32      = tycon "Word32"
convertC2HS CTUInt64      = tycon "Word64"
convertC2HS CTString    = tycon "CString"
convertC2HS CTVoidStar   = tyapp (tycon "Ptr") unit_tycon
convertC2HS (CEnum t _)  = convertC2HS t
convertC2HS (CPointer t) = tyapp (tycon "Ptr") (convertC2HS t)
convertC2HS (CRef t)     = tyapp (tycon "Ptr") (convertC2HS t)

-- |
convertCpp2HS :: Maybe Class -> Types -> Type ()
convertCpp2HS _c Void                  = unit_tycon
convertCpp2HS (Just c) SelfType        = tycon ((fst.hsClassName) c)
convertCpp2HS Nothing SelfType         = error "convertCpp2HS : SelfType but no class "
convertCpp2HS _c (CT t _)              = convertC2HS t
convertCpp2HS _c (CPT (CPTClass c') _)     = (tycon . fst . hsClassName) c'
convertCpp2HS _c (CPT (CPTClassRef c') _)  = (tycon . fst . hsClassName) c'
convertCpp2HS _c (CPT (CPTClassCopy c') _) = (tycon . fst . hsClassName) c'
convertCpp2HS _c (CPT (CPTClassMove c') _) = (tycon . fst . hsClassName) c'
convertCpp2HS _c (TemplateApp x)     = tyapp
                                         (tycon (tclass_name (tapp_tclass x)))
                                         (tycon (hsClassNameForTArg (tapp_tparam x)))
convertCpp2HS _c (TemplateAppRef x)  = tyapp
                                         (tycon (tclass_name (tapp_tclass x)))
                                         (tycon (hsClassNameForTArg (tapp_tparam x)))
convertCpp2HS _c (TemplateAppMove x) = tyapp
                                         (tycon (tclass_name (tapp_tclass x)))
                                         (tycon (hsClassNameForTArg (tapp_tparam x)))
convertCpp2HS _c (TemplateType t)    = tyapp
                                         (tycon (tclass_name t))
                                         (mkTVar (tclass_param t))
convertCpp2HS _c (TemplateParam p)         = mkTVar p
convertCpp2HS _c (TemplateParamPointer p)  = mkTVar p

-- |
convertCpp2HS4Tmpl
  :: Type ()    -- ^ self
  -> Maybe Class
  -> Type ()    -- ^ type paramemter splice
  -> Types
  -> Type ()
convertCpp2HS4Tmpl _ _c _ Void                  = unit_tycon
convertCpp2HS4Tmpl _ (Just c) _ SelfType        = tycon ((fst.hsClassName) c)
convertCpp2HS4Tmpl _ Nothing _ SelfType         = error "convertCpp2HS4Tmpl : SelfType but no class "
convertCpp2HS4Tmpl _ _c _ (CT t _)              = convertC2HS t
convertCpp2HS4Tmpl _ _c _ (CPT (CPTClass c') _)     = (tycon . fst . hsClassName) c'
convertCpp2HS4Tmpl _ _c _ (CPT (CPTClassRef c') _)  = (tycon . fst . hsClassName) c'
convertCpp2HS4Tmpl _ _c _ (CPT (CPTClassCopy c') _) = (tycon . fst . hsClassName) c'
convertCpp2HS4Tmpl _ _c _ (CPT (CPTClassMove c') _) = (tycon . fst . hsClassName) c'
convertCpp2HS4Tmpl e c s x@(TemplateApp p) =
  case tapp_tparam p of
    TArg_TypeParam _ -> let t = tapp_tclass p
                            (hname,_) = hsTemplateClassName t
                        in tyapp (tycon hname) s
    _ -> convertCpp2HS c x
convertCpp2HS4Tmpl e c s x@(TemplateAppRef p) =
  case tapp_tparam p of
    TArg_TypeParam _ -> let t = tapp_tclass p
                            (hname,_) = hsTemplateClassName t
                        in tyapp (tycon hname) s
    _ -> convertCpp2HS c x
convertCpp2HS4Tmpl e c s x@(TemplateAppMove p) =
  case tapp_tparam p of
    TArg_TypeParam _ -> let t = tapp_tclass p
                            (hname,_) = hsTemplateClassName t
                        in tyapp (tycon hname) s
    _ -> convertCpp2HS c x
convertCpp2HS4Tmpl e _c _ (TemplateType _)          = e
convertCpp2HS4Tmpl _ _c s (TemplateParam _)         = s
convertCpp2HS4Tmpl _ _c s (TemplateParamPointer _)  = s


hsFuncXformer :: Function -> String
hsFuncXformer func@(Constructor _ _) = let len = length (genericFuncArgs func)
                                       in if len > 0
                                          then "xform" <> show (len - 1)
                                          else "xformnull"
hsFuncXformer func@(Static _ _ _ _) =
  let len = length (genericFuncArgs func)
  in if len > 0
     then "xform" <> show (len - 1)
     else "xformnull"
hsFuncXformer func = let len = length (genericFuncArgs func)
                     in "xform" <> show len



classConstraints :: Class -> Context ()
classConstraints = cxTuple . map ((\n->classA (unqual n) [mkTVar "a"]) . typeclassName) . class_parents

extractArgRetTypes
  :: Maybe Class  -- ^ class (Nothing for top-level function)
  -> Bool         -- ^ is virtual function?
  -> CFunSig      -- ^ C type signature information for a given function      -- (Args,Types)           -- ^ (argument types, return type) of a given function
  -> HsFunSig     -- ^ Haskell type signature information for the function    --   ([Type ()],[Asst ()])  -- ^ (types, class constraints)
extractArgRetTypes mc isvirtual (CFunSig args ret) =
  let  (typs,s) = flip runState ([],(0 :: Int)) $ do
                    as <- mapM (mktyp . arg_type) args
                    r <- case ret of
                           SelfType -> case mc of
                                         Nothing -> error "extractArgRetTypes: SelfType return but no class"
                                         Just c -> if isvirtual then return (mkTVar "a") else return $ tycon ((fst.hsClassName) c)
                           x -> (return . convertCpp2HS Nothing) x
                    return (as ++ [tyapp (tycon "IO") r])
  in   HsFunSig { hsSigTypes = typs
                , hsSigConstraints = fst s
                }
 where addclass c = do
         (ctxts,n) <- get
         let cname = (fst.hsClassName) c
             iname = typeclassNameFromStr cname
             tvar = mkTVar ('c' : show n)
             ctxt1 = classA (unqual iname) [tvar]
             ctxt2 = classA (unqual "FPtr") [tvar]
         put (ctxt1:ctxt2:ctxts,n+1)
         return tvar
       addstring = do
         (ctxts,n) <- get
         let tvar = mkTVar ('c' : show n)
             ctxt = classA (unqual "Castable") [tvar,tycon "CString"]
         put (ctxt:ctxts,n+1)
         return tvar

       mktyp typ =
         case typ of
           SelfType -> return (mkTVar "a")
           CT CTString Const -> addstring
           CT _ _   -> return $ convertCpp2HS Nothing typ
           CPT (CPTClass c') _    -> addclass c'
           CPT (CPTClassRef c') _ -> addclass c'
           CPT (CPTClassCopy c') _ -> addclass c'
           CPT (CPTClassMove c') _ -> addclass c'
           -- it is not clear whether the following is okay or not.
           (TemplateApp x)    -> pure $
                                   tyapp
                                     (tycon (tclass_name (tapp_tclass x)))
                                     (tycon (hsClassNameForTArg (tapp_tparam x)))
           (TemplateAppRef x) -> pure $
                                   tyapp
                                     (tycon (tclass_name (tapp_tclass x)))
                                     (tycon (hsClassNameForTArg (tapp_tparam x)))
           (TemplateAppMove x)-> pure $
                                   tyapp
                                     (tycon (tclass_name (tapp_tclass x)))
                                     (tycon (hsClassNameForTArg (tapp_tparam x)))
           (TemplateType t)   -> pure $
                                   tyapp
                                     (tycon (tclass_name t))
                                     (mkTVar (tclass_param t))
           (TemplateParam p)      -> return (mkTVar p)
           Void -> return unit_tycon
           _ -> error ("No such c type : " <> show typ)

functionSignature :: Class -> Function -> Type ()
functionSignature c f =
  let HsFunSig typs assts = extractArgRetTypes
                              (Just c)
                              (isVirtualFunc f)
                              (CFunSig (genericFuncArgs f) (genericFuncRet f))
      ctxt = cxTuple assts
      arg0
        | isVirtualFunc f    = (mkTVar "a" :)
        | isNonVirtualFunc f = (mkTVar (fst (hsClassName c)) :)
        | otherwise          = id
  in TyForall () Nothing (Just ctxt) (foldr1 tyfun (arg0 typs))

functionSignatureT :: TemplateClass -> TemplateFunction -> Type ()
functionSignatureT t TFun {..} =
  let (hname,_) = hsTemplateClassName t
      tp = tclass_param t
      ctyp = convertCpp2HS Nothing tfun_ret
      arg0 =  (tyapp (tycon hname) (mkTVar tp) :)
      lst = arg0 (map (convertCpp2HS Nothing . arg_type) tfun_args)
  in foldr1 tyfun (lst <> [tyapp (tycon "IO") ctyp])
functionSignatureT t TFunNew {..} =
  let ctyp = convertCpp2HS Nothing (TemplateType t)
      lst = map (convertCpp2HS Nothing . arg_type) tfun_new_args
  in foldr1 tyfun (lst <> [tyapp (tycon "IO") ctyp])
functionSignatureT t TFunDelete =
  let ctyp = convertCpp2HS Nothing (TemplateType t)
  in ctyp `tyfun` (tyapp (tycon "IO") unit_tycon)



-- TODO: rename this and combine this with functionSignatureTMF
functionSignatureTT :: TemplateClass -> TemplateFunction -> Type ()
functionSignatureTT t f = foldr1 tyfun (lst <> [tyapp (tycon "IO") ctyp])
 where
  (hname,_) = hsTemplateClassName t
  ctyp = case f of
           TFun {..}    -> convertCpp2HS4Tmpl e Nothing spl tfun_ret
           TFunNew {..} -> convertCpp2HS4Tmpl e Nothing spl (TemplateType t)
           TFunDelete   -> unit_tycon
  e = tyapp (tycon hname) spl
  spl = tySplice (parenSplice (mkVar (tclass_param t)))
  lst =
    case f of
      TFun {..}    -> e : map (convertCpp2HS4Tmpl e Nothing spl . arg_type) tfun_args
      TFunNew {..} -> map (convertCpp2HS4Tmpl e Nothing spl . arg_type) tfun_new_args
      TFunDelete -> [e]

-- TODO: rename this and combine this with functionSignatureTT
functionSignatureTMF :: Class -> TemplateMemberFunction -> Type ()
functionSignatureTMF c f = foldr1 tyfun (lst <> [tyapp (tycon "IO") ctyp])
  where
    ctyp = convertCpp2HS4Tmpl e Nothing spl (tmf_ret f)
    e = tycon (fst (hsClassName c))
    spl = tySplice (parenSplice (mkVar (tmf_param f)))
    lst = e : map (convertCpp2HS4Tmpl e Nothing spl . arg_type) (tmf_args f)


accessorCFunSig :: Types -> Accessor -> CFunSig
accessorCFunSig typ Getter = CFunSig [] typ
accessorCFunSig typ Setter = CFunSig [Arg typ "x"] Void


accessorSignature :: Class -> Variable -> Accessor -> Type ()
accessorSignature c v accessor =
  let csig = accessorCFunSig (arg_type (unVariable v)) accessor
      HsFunSig typs assts = extractArgRetTypes (Just c) False csig
      ctxt = cxTuple assts
      arg0 = (mkTVar (fst (hsClassName c)) :)
  in TyForall () Nothing (Just ctxt) (foldr1 tyfun (arg0 typs))


-- | this is for FFI type.
hsFFIFuncTyp :: Maybe (Selfness, Class) -> CFunSig -> Type ()
hsFFIFuncTyp msc (CFunSig args ret) =
  foldr1 tyfun $ case msc of
                   Nothing         -> argtyps <> [tyapp (tycon "IO") rettyp]
                   Just (Self,_)   -> selftyp: argtyps <> [tyapp (tycon "IO") rettyp]
                   Just (NoSelf,_) -> argtyps <> [tyapp (tycon "IO") rettyp]
  where argtyps :: [Type ()]
        argtyps = map (hsargtype . arg_type) args
        rettyp :: Type ()
        rettyp  = hsrettype ret
        selftyp = case msc of
                    Just (_,c) -> tyapp tyPtr (tycon (snd (hsClassName c)))
                    Nothing    -> error "hsFFIFuncTyp: no self for top level function"
        hsargtype :: Types -> Type ()
        hsargtype (CT ctype _)               = convertC2HS ctype
        hsargtype (CPT (CPTClass d) _)       = tyapp tyPtr (tycon rawname)
          where rawname = snd (hsClassName d)
        hsargtype (CPT (CPTClassRef d) _)    = tyapp tyPtr (tycon rawname)
          where rawname = snd (hsClassName d)
        hsargtype (CPT (CPTClassMove d) _)    = tyapp tyPtr (tycon rawname)
          where rawname = snd (hsClassName d)
        hsargtype (CPT (CPTClassCopy d) _)    = tyapp tyPtr (tycon rawname)
          where rawname = snd (hsClassName d)
        hsargtype (TemplateApp x)   = tyapp
                                        tyPtr
                                        (tyapp
                                           (tycon rawname)
                                           (tycon (hsClassNameForTArg (tapp_tparam x))))
          where rawname = snd (hsTemplateClassName (tapp_tclass x))
        hsargtype (TemplateAppRef x) = tyapp
                                         tyPtr
                                         (tyapp
                                            (tycon rawname)
                                            (tycon (hsClassNameForTArg (tapp_tparam x))))
          where rawname = snd (hsTemplateClassName (tapp_tclass x))
        hsargtype (TemplateAppMove x)= tyapp
                                         tyPtr
                                         (tyapp
                                            (tycon rawname)
                                            (tycon (hsClassNameForTArg (tapp_tparam x))))
          where rawname = snd (hsTemplateClassName (tapp_tclass x))
        hsargtype (TemplateType t)           = tyapp tyPtr (tyapp (tycon rawname) (mkTVar (tclass_param t)))
          where rawname = snd (hsTemplateClassName t)
        hsargtype (TemplateParam p)          = mkTVar p
        hsargtype SelfType                   = selftyp
        hsargtype _ = error "hsFuncTyp: undefined hsargtype"
        ---------------------------------------------------------
        hsrettype Void                       = unit_tycon
        hsrettype SelfType                   = selftyp
        hsrettype (CT ctype _)               = convertC2HS ctype
        hsrettype (CPT (CPTClass d) _)       = tyapp tyPtr (tycon rawname)
          where rawname = snd (hsClassName d)
        hsrettype (CPT (CPTClassRef d) _)    = tyapp tyPtr (tycon rawname)
          where rawname = snd (hsClassName d)
        hsrettype (CPT (CPTClassCopy d) _)   = tyapp tyPtr (tycon rawname)
          where rawname = snd (hsClassName d)
        hsrettype (CPT (CPTClassMove d) _)   = tyapp tyPtr (tycon rawname)
          where rawname = snd (hsClassName d)
        hsrettype (TemplateApp x)    = tyapp
                                         tyPtr
                                         (tyapp
                                            (tycon rawname)
                                            (tycon (hsClassNameForTArg (tapp_tparam x))))
          where rawname = snd (hsTemplateClassName (tapp_tclass x))
        hsrettype (TemplateAppRef x) = tyapp
                                         tyPtr
                                         (tyapp
                                            (tycon rawname)
                                            (tycon (hsClassNameForTArg (tapp_tparam x))))
          where rawname = snd (hsTemplateClassName (tapp_tclass x))
        hsrettype (TemplateAppMove x)= tyapp
                                         tyPtr
                                         (tyapp
                                            (tycon rawname)
                                            (tycon (hsClassNameForTArg (tapp_tparam x))))
          where rawname = snd (hsTemplateClassName (tapp_tclass x))
        hsrettype (TemplateType t)           = tyapp tyPtr (tyapp (tycon rawname) (mkTVar (tclass_param t)))
          where rawname = snd (hsTemplateClassName t)
        hsrettype (TemplateParam p)          = mkTVar p
        hsrettype (TemplateParamPointer p)   = mkTVar p



genericFuncRet :: Function -> Types
genericFuncRet f =
  case f of
    Constructor _ _ -> self_
    Virtual t _ _ _ -> t
    NonVirtual t _ _ _-> t
    Static t _ _ _ -> t
    Destructor _ -> void_

genericFuncArgs :: Function -> [Arg]
genericFuncArgs (Destructor _) = []
genericFuncArgs f = func_args f
