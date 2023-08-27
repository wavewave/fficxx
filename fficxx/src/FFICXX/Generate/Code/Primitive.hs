{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module FFICXX.Generate.Code.Primitive where

import Control.Monad.Trans.State (get, put, runState)
import Data.Functor.Identity (Identity)
import FFICXX.Generate.Name
  ( ffiClassName,
    hsClassName,
    hsClassNameForTArg,
    hsTemplateClassName,
    tmplAccessorName,
    typeclassName,
    typeclassNameFromStr,
  )
import FFICXX.Generate.Type.Class
  ( Accessor (Getter, Setter),
    Arg (..),
    CPPTypes (..),
    CTypes (..),
    Class (..),
    EnumType (enum_name),
    Form (..),
    Function (..),
    IsConst (Const, NoConst),
    Safety (..),
    Selfness (NoSelf, Self),
    TemplateAppInfo (..),
    TemplateArgType (TArg_TypeParam),
    TemplateClass (..),
    TemplateFunction (..),
    TemplateMemberFunction (..),
    Types (..),
    Variable (..),
    argsFromOpExp,
    isNonVirtualFunc,
    isVirtualFunc,
  )
import qualified FFICXX.Generate.Util.GHCExactPrint as Ex
import qualified FFICXX.Runtime.CodeGen.Cxx as R
import FFICXX.Runtime.TH (IsCPrimitive (CPrim, NonCPrim))
import GHC.Hs (GhcPs)
import qualified GHC.Types.ForeignCall as GHC (Safety (..))
import Language.Haskell.Syntax
  ( HsContext,
    HsType,
  )

toGHCSafety :: Safety -> GHC.Safety
toGHCSafety Unsafe = GHC.PlayRisky
toGHCSafety Safe = GHC.PlaySafe
toGHCSafety Interruptible = GHC.PlayInterruptible

data CFunSig = CFunSig
  { cArgTypes :: [Arg],
    cRetType :: Types
  }

data HsFunSig = HsFunSig
  { hsSigTypes :: [HsType GhcPs],
    hsSigConstraints :: [HsType GhcPs]
  }

ctypToCType :: CTypes -> IsConst -> R.CType Identity
ctypToCType ctyp isconst =
  let typ = case ctyp of
        CTBool -> R.CTSimple $ R.sname "bool"
        CTChar -> R.CTSimple $ R.sname "char"
        CTClock -> R.CTSimple $ R.sname "clock_t"
        CTDouble -> R.CTSimple $ R.sname "double"
        CTFile -> R.CTSimple $ R.sname "FILE"
        CTFloat -> R.CTSimple $ R.sname "float"
        CTFpos -> R.CTSimple $ R.sname "fpos_t"
        CTInt -> R.CTSimple $ R.sname "int"
        CTIntMax -> R.CTSimple $ R.sname "intmax_t"
        CTIntPtr -> R.CTSimple $ R.sname "intptr_t"
        CTJmpBuf -> R.CTSimple $ R.sname "jmp_buf"
        CTLLong -> R.CTSimple $ R.sname "long long"
        CTLong -> R.CTSimple $ R.sname "long"
        CTPtrdiff -> R.CTSimple $ R.sname "ptrdiff_t"
        CTSChar -> R.CTSimple $ R.sname "sized char"
        CTSUSeconds -> R.CTSimple $ R.sname "suseconds_t"
        CTShort -> R.CTSimple $ R.sname "short"
        CTSigAtomic -> R.CTSimple $ R.sname "sig_atomic_t"
        CTSize -> R.CTSimple $ R.sname "size_t"
        CTTime -> R.CTSimple $ R.sname "time_t"
        CTUChar -> R.CTSimple $ R.sname "unsigned char"
        CTUInt -> R.CTSimple $ R.sname "unsigned int"
        CTUIntMax -> R.CTSimple $ R.sname "uintmax_t"
        CTUIntPtr -> R.CTSimple $ R.sname "uintptr_t"
        CTULLong -> R.CTSimple $ R.sname "unsigned long long"
        CTULong -> R.CTSimple $ R.sname "unsigned long"
        CTUSeconds -> R.CTSimple $ R.sname "useconds_t"
        CTUShort -> R.CTSimple $ R.sname "unsigned short"
        CTWchar -> R.CTSimple $ R.sname "wchar_t"
        CTInt8 -> R.CTSimple $ R.sname "int8_t"
        CTInt16 -> R.CTSimple $ R.sname "int16_t"
        CTInt32 -> R.CTSimple $ R.sname "int32_t"
        CTInt64 -> R.CTSimple $ R.sname "int64_t"
        CTUInt8 -> R.CTSimple $ R.sname "uint8_t"
        CTUInt16 -> R.CTSimple $ R.sname "uint16_t"
        CTUInt32 -> R.CTSimple $ R.sname "uint32_t"
        CTUInt64 -> R.CTSimple $ R.sname "uint64_t"
        CTString -> R.CTStar $ R.CTSimple $ R.sname "char"
        CTVoidStar -> R.CTStar R.CTVoid
        CPointer s -> R.CTStar (ctypToCType s NoConst)
        CRef s -> R.CTStar (ctypToCType s NoConst)
   in case isconst of
        Const -> R.CTConst typ
        NoConst -> typ

self_ :: Types
self_ = SelfType

cstring_ :: Types
cstring_ = CT CTString Const

cint_ :: Types
cint_ = CT CTInt Const

int_ :: Types
int_ = CT CTInt NoConst

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
double_ = CT CTDouble NoConst

doublep_ :: Types
doublep_ = CT (CPointer CTDouble) NoConst

cfloat_ :: Types
cfloat_ = CT CTFloat Const

float_ :: Types
float_ = CT CTFloat NoConst

bool_ :: Types
bool_ = CT CTBool NoConst

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
double var = Arg double_ var

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
cppclass_ c = CPT (CPTClass c) NoConst

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

argToCTypVar :: Arg -> (R.CType Identity, R.CName Identity)
argToCTypVar (Arg (CT ctyp isconst) varname) =
  (ctypToCType ctyp isconst, R.sname varname)
argToCTypVar (Arg SelfType varname) =
  (R.CTSimple (R.CName [R.NamePart "Type", R.NamePart "_p"]), R.sname varname)
argToCTypVar (Arg (CPT (CPTEnum _e) _isconst) varname) =
  (ctypToCType CTInt NoConst, R.sname varname)
argToCTypVar (Arg (CPT (CPTClass c) isconst) varname) =
  case isconst of
    Const -> (R.CTSimple (R.sname ("const_" <> cname <> "_p")), R.sname varname)
    NoConst -> (R.CTSimple (R.sname (cname <> "_p")), R.sname varname)
  where
    cname = ffiClassName c
argToCTypVar (Arg (CPT (CPTClassRef c) isconst) varname) =
  case isconst of
    Const -> (R.CTSimple (R.sname ("const_" <> cname <> "_p")), R.sname varname)
    NoConst -> (R.CTSimple (R.sname (cname <> "_p")), R.sname varname)
  where
    cname = ffiClassName c
argToCTypVar (Arg (CPT (CPTClassCopy c) isconst) varname) =
  case isconst of
    Const -> (R.CTSimple (R.sname ("const_" <> cname <> "_p")), R.sname varname)
    NoConst -> (R.CTSimple (R.sname (cname <> "_p")), R.sname varname)
  where
    cname = ffiClassName c
argToCTypVar (Arg (CPT (CPTClassMove c) isconst) varname) =
  case isconst of
    Const -> (R.CTSimple (R.sname ("const_" <> cname <> "_p")), R.sname varname)
    NoConst -> (R.CTSimple (R.sname (cname <> "_p")), R.sname varname)
  where
    cname = ffiClassName c
argToCTypVar (Arg (TemplateApp _) varname) = (R.CTStar R.CTVoid, R.sname varname)
argToCTypVar (Arg (TemplateAppRef _) varname) = (R.CTStar R.CTVoid, R.sname varname)
argToCTypVar (Arg (TemplateAppMove _) varname) = (R.CTStar R.CTVoid, R.sname varname)
argToCTypVar t = error ("argToCTypVar: " <> show t)

argsToCTypVar :: [Arg] -> [(R.CType Identity, R.CName Identity)]
argsToCTypVar args =
  let args' = (Arg SelfType "p") : args
   in map argToCTypVar args'

argsToCTypVarNoSelf :: [Arg] -> [(R.CType Identity, R.CName Identity)]
argsToCTypVarNoSelf = map argToCTypVar

argToCallCExp :: Arg -> R.CExp Identity
argToCallCExp (Arg t e) = c2Cxx t (R.CVar (R.sname e))

-- TODO: rename this function by castExpressionFrom/To or something like that.
returnCType :: Types -> R.CType Identity
returnCType (CT ctyp isconst) = ctypToCType ctyp isconst
returnCType Void = R.CTVoid
returnCType SelfType = R.CTSimple (R.CName [R.NamePart "Type", R.NamePart "_p"])
-- TODO: for now, weakly typed enum.
returnCType (CPT (CPTEnum _) _) = ctypToCType CTInt Const
returnCType (CPT (CPTClass c) _) = R.CTSimple (R.sname (ffiClassName c <> "_p"))
returnCType (CPT (CPTClassRef c) _) = R.CTSimple (R.sname (ffiClassName c <> "_p"))
returnCType (CPT (CPTClassCopy c) _) = R.CTSimple (R.sname (ffiClassName c <> "_p"))
returnCType (CPT (CPTClassMove c) _) = R.CTSimple (R.sname (ffiClassName c <> "_p"))
returnCType (TemplateApp _) = R.CTStar R.CTVoid
returnCType (TemplateAppRef _) = R.CTStar R.CTVoid
returnCType (TemplateAppMove _) = R.CTStar R.CTVoid
returnCType (TemplateType _) = R.CTStar R.CTVoid
returnCType (TemplateParam t) = R.CTSimple (R.CName [R.NamePart t, R.NamePart "_p"])
returnCType (TemplateParamPointer t) = R.CTSimple (R.CName [R.NamePart t, R.NamePart "_p"])

-- TODO: Rewrite this with static_cast
c2Cxx :: Types -> R.CExp Identity -> R.CExp Identity
c2Cxx t e =
  case t of
    CT (CRef _) _ -> R.CStar e
    CPT (CPTEnum en) _ ->
      R.CCast (R.CTVerbatim (enum_name en)) e
    CPT (CPTClass c) _ ->
      R.CTApp
        (R.sname "from_nonconst_to_nonconst")
        [R.CTSimple (R.sname f), R.CTSimple (R.sname (f <> "_t"))]
        [e]
      where
        f = ffiClassName c
    CPT (CPTClassRef c) _ ->
      R.CTApp
        (R.sname "from_nonconstref_to_nonconstref")
        [R.CTSimple (R.sname f), R.CTSimple (R.sname (f <> "_t"))]
        [R.CStar e]
      where
        f = ffiClassName c
    CPT (CPTClassCopy c) _ ->
      R.CStar $
        R.CTApp
          (R.sname "from_nonconst_to_nonconst")
          [R.CTSimple (R.sname f), R.CTSimple (R.sname (f <> "_t"))]
          [e]
      where
        f = ffiClassName c
    CPT (CPTClassMove c) _ ->
      R.CApp
        (R.CVar (R.sname "std::move"))
        [ R.CTApp
            (R.sname "from_nonconstref_to_nonconstref")
            [R.CTSimple (R.sname f), R.CTSimple (R.sname (f <> "_t"))]
            [R.CStar e]
        ]
      where
        f = ffiClassName c
    TemplateApp p ->
      R.CTApp
        (R.sname "from_nonconstref_to_nonconst")
        [R.CTVerbatim (tapp_CppTypeForParam p), R.CTVoid]
        [e]
    TemplateAppRef p ->
      R.CStar $
        R.CCast (R.CTStar (R.CTVerbatim (tapp_CppTypeForParam p))) e
    TemplateAppMove p ->
      R.CApp
        (R.CVar (R.sname "std::move"))
        [ R.CStar $
            R.CCast (R.CTStar (R.CTVerbatim (tapp_CppTypeForParam p))) e
        ]
    _ -> e

-- TODO: Rewrite this with static_cast
--       Merge this with returnCpp after Void and simple type adjustment
-- TODO: Resolve all the error cases
cxx2C :: Types -> R.CExp Identity -> R.CExp Identity
cxx2C t e =
  case t of
    Void -> R.CNull
    SelfType ->
      R.CTApp
        (R.sname "from_nonconst_to_nonconst")
        [R.CTSimple (R.CName [R.NamePart "Type", R.NamePart "_t"]), R.CTSimple (R.sname "Type")]
        [R.CCast (R.CTStar (R.CTSimple (R.sname "Type"))) e]
    -- "to_nonconst<Type ## _t, Type>((Type *)" <> e <> ")"
    CT (CRef _) _ -> R.CAddr e
    -- "&(" <> e <> ")"
    CT _ _ -> e
    -- e
    CPT (CPTEnum _) _ -> e
    -- e
    CPT (CPTClass c) _ ->
      R.CTApp
        (R.sname "from_nonconst_to_nonconst")
        [R.CTSimple (R.sname (f <> "_t")), R.CTSimple (R.sname f)]
        [R.CCast (R.CTStar (R.CTSimple (R.sname f))) e]
      where
        f = ffiClassName c
    -- "to_nonconst<" <> f <> "_t," <> f <> ">((" <> f <> "*)" <> e <> ")"
    CPT (CPTClassRef c) _ ->
      R.CTApp
        (R.sname "from_nonconst_to_nonconst")
        [R.CTSimple (R.sname (f <> "_t")), R.CTSimple (R.sname f)]
        [R.CAddr e]
      where
        f = ffiClassName c
    -- "to_nonconst<" <> f <> "_t," <> f <> ">(&(" <> e <> "))"
    CPT (CPTClassCopy c) _ ->
      R.CTApp
        (R.sname "from_nonconst_to_nonconst")
        [R.CTSimple (R.sname (f <> "_t")), R.CTSimple (R.sname f)]
        [R.CNew (R.sname f) [e]]
      where
        f = ffiClassName c
    -- "to_nonconst<" <> f <> "_t," <> f <> ">(new " <> f <> "(" <> e <> "))"
    CPT (CPTClassMove c) _ ->
      R.CApp
        (R.CVar (R.sname "std::move"))
        [ R.CTApp
            (R.sname "from_nonconst_to_nonconst")
            [R.CTSimple (R.sname (f <> "_t")), R.CTSimple (R.sname f)]
            [R.CAddr e]
        ]
      where
        f = ffiClassName c
    -- "std::move(to_nonconst<" <> f <> "_t," <> f <>">(&(" <> e <> ")))"
    TemplateApp _ ->
      error "cxx2C: TemplateApp"
    -- g <> "* r = new " <> g <> "(" <> e <> "); "
    --  <> "return (static_cast<void*>(r));"
    TemplateAppRef _ ->
      error "cxx2C: TemplateAppRef"
    -- g <> "* r = new " <> g <> "(" <> e <> "); "
    -- <> "return (static_cast<void*>(r));"
    TemplateAppMove _ ->
      error "cxx2C: TemplateAppMove"
    TemplateType _ ->
      error "cxx2C: TemplateType"
    TemplateParam _ ->
      error "cxx2C: TemplateParam"
    -- if b then e
    --      else "to_nonconst<Type ## _t, Type>((Type *)&(" <> e <> "))"
    TemplateParamPointer _ ->
      error "cxx2C: TemplateParamPointer"

-- if b then "(" <> callstr <> ");"
--      else "to_nonconst<Type ## _t, Type>(" <> e <> ") ;"

tmplAppTypeFromForm :: Form -> [R.CType Identity] -> R.CType Identity
tmplAppTypeFromForm (FormSimple tclass) targs = R.CTTApp (R.sname tclass) targs
tmplAppTypeFromForm (FormNested tclass inner) targs = R.CTScoped (R.CTTApp (R.sname tclass) targs) (R.CTVerbatim inner)

tmplArgToCTypVar ::
  IsCPrimitive ->
  Arg ->
  (R.CType Identity, R.CName Identity)
tmplArgToCTypVar _ (Arg (CT ctyp isconst) varname) =
  (ctypToCType ctyp isconst, R.sname varname)
tmplArgToCTypVar _ (Arg SelfType varname) =
  (R.CTStar R.CTVoid, R.sname varname)
tmplArgToCTypVar _ (Arg (CPT (CPTClass c) isconst) varname) =
  case isconst of
    Const -> (R.CTSimple (R.sname ("const_" <> ffiClassName c <> "_p")), R.sname varname)
    NoConst -> (R.CTSimple (R.sname (ffiClassName c <> "_p")), R.sname varname)
tmplArgToCTypVar _ (Arg (CPT (CPTClassRef c) isconst) varname) =
  case isconst of
    Const -> (R.CTSimple (R.sname ("const_" <> ffiClassName c <> "_p")), R.sname varname)
    NoConst -> (R.CTSimple (R.sname (ffiClassName c <> "_p")), R.sname varname)
tmplArgToCTypVar _ (Arg (CPT (CPTClassMove c) isconst) varname) =
  case isconst of
    Const -> (R.CTSimple (R.sname ("const_" <> ffiClassName c <> "_p")), R.sname varname)
    NoConst -> (R.CTSimple (R.sname (ffiClassName c <> "_p")), R.sname varname)
tmplArgToCTypVar _ (Arg (TemplateApp _) v) = (R.CTStar R.CTVoid, R.sname v)
tmplArgToCTypVar _ (Arg (TemplateAppRef _) v) = (R.CTStar R.CTVoid, R.sname v)
tmplArgToCTypVar _ (Arg (TemplateAppMove _) v) = (R.CTStar R.CTVoid, R.sname v)
tmplArgToCTypVar _ (Arg (TemplateType _) v) = (R.CTStar R.CTVoid, R.sname v)
tmplArgToCTypVar CPrim (Arg (TemplateParam t) v) = (R.CTSimple (R.sname t), R.sname v)
tmplArgToCTypVar NonCPrim (Arg (TemplateParam t) v) = (R.CTSimple (R.CName [R.NamePart t, R.NamePart "_p"]), R.sname v)
tmplArgToCTypVar CPrim (Arg (TemplateParamPointer t) v) = (R.CTStar (R.CTSimple (R.sname t)), R.sname v)
tmplArgToCTypVar NonCPrim (Arg (TemplateParamPointer t) v) = (R.CTSimple (R.CName [R.NamePart t, R.NamePart "_p"]), R.sname v)
tmplArgToCTypVar _ _ = error "tmplArgToCTypVar: undefined"

tmplAllArgsToCTypVar ::
  IsCPrimitive ->
  Selfness ->
  TemplateClass ->
  [Arg] ->
  [(R.CType Identity, R.CName Identity)]
tmplAllArgsToCTypVar b s t args =
  let args' = case s of
        Self -> (Arg (TemplateType t) "p") : args
        NoSelf -> args
   in map (tmplArgToCTypVar b) args'

-- TODO: Rewrite this with static_cast.
--       Implement missing cases.
tmplArgToCallCExp ::
  IsCPrimitive ->
  Arg ->
  R.CExp Identity
tmplArgToCallCExp _ (Arg (CPT (CPTClass c) _) varname) =
  R.CTApp
    (R.sname "from_nonconst_to_nonconst")
    [R.CTSimple (R.sname str), R.CTSimple (R.sname (str <> "_t"))]
    [R.CVar (R.sname varname)]
  where
    str = ffiClassName c
tmplArgToCallCExp _ (Arg (CPT (CPTClassRef c) _) varname) =
  R.CTApp
    (R.sname "from_nonconstref_to_nonconstref")
    [R.CTSimple (R.sname str), R.CTSimple (R.sname (str <> "_t"))]
    [R.CStar $ R.CVar $ R.sname varname]
  where
    str = ffiClassName c
tmplArgToCallCExp _ (Arg (CPT (CPTClassMove c) _) varname) =
  R.CApp
    (R.CVar (R.sname "std::move"))
    [ R.CTApp
        (R.sname "from_nonconstref_to_nonconstref")
        [R.CTSimple (R.sname str), R.CTSimple (R.sname (str <> "_t"))]
        [R.CStar $ R.CVar $ R.sname varname]
    ]
  where
    str = ffiClassName c
tmplArgToCallCExp _ (Arg (CT (CRef _) _) varname) =
  R.CStar $ R.CVar $ R.sname varname
tmplArgToCallCExp _ (Arg (TemplateApp x) varname) =
  let targs = map (R.CTSimple . R.sname . hsClassNameForTArg) (tapp_tparams x)
   in R.CTApp
        (R.sname "static_cast")
        [R.CTStar $ tmplAppTypeFromForm (tclass_cxxform (tapp_tclass x)) targs]
        [R.CVar $ R.sname varname]
tmplArgToCallCExp _ (Arg (TemplateAppRef x) varname) =
  let targs = map (R.CTSimple . R.sname . hsClassNameForTArg) (tapp_tparams x)
   in R.CStar $
        R.CTApp
          (R.sname "static_cast")
          [R.CTStar $ tmplAppTypeFromForm (tclass_cxxform (tapp_tclass x)) targs]
          [R.CVar $ R.sname varname]
tmplArgToCallCExp _ (Arg (TemplateAppMove x) varname) =
  let targs = map (R.CTSimple . R.sname . hsClassNameForTArg) (tapp_tparams x)
   in R.CApp
        (R.CVar (R.sname "std::move"))
        [ R.CStar $
            R.CTApp
              (R.sname "static_cast")
              [R.CTStar $ tmplAppTypeFromForm (tclass_cxxform (tapp_tclass x)) targs]
              [R.CVar $ R.sname varname]
        ]
tmplArgToCallCExp b (Arg (TemplateParam typ) varname) =
  case b of
    CPrim -> R.CVar $ R.sname varname
    NonCPrim ->
      R.CStar $
        R.CTApp
          (R.sname "from_nonconst_to_nonconst")
          [R.CTSimple (R.sname typ), R.CTSimple (R.CName [R.NamePart typ, R.NamePart "_t"])]
          [R.CVar $ R.sname varname]
tmplArgToCallCExp b (Arg (TemplateParamPointer typ) varname) =
  case b of
    CPrim -> R.CVar $ R.sname varname
    NonCPrim ->
      R.CTApp
        (R.sname "from_nonconst_to_nonconst")
        [R.CTSimple (R.sname typ), R.CTSimple (R.CName [R.NamePart typ, R.NamePart "_t"])]
        [R.CVar $ R.sname varname]
tmplArgToCallCExp _ (Arg _ varname) = R.CVar $ R.sname varname

tmplReturnCType ::
  IsCPrimitive ->
  Types ->
  R.CType Identity
tmplReturnCType _ (CT ctyp isconst) = ctypToCType ctyp isconst
tmplReturnCType _ Void = R.CTVoid
tmplReturnCType _ SelfType = R.CTStar R.CTVoid
tmplReturnCType _ (CPT (CPTEnum _) _) = ctypToCType CTInt Const
tmplReturnCType _ (CPT (CPTClass c) _) = R.CTSimple (R.sname (ffiClassName c <> "_p"))
tmplReturnCType _ (CPT (CPTClassRef c) _) = R.CTSimple (R.sname (ffiClassName c <> "_p"))
tmplReturnCType _ (CPT (CPTClassCopy c) _) = R.CTSimple (R.sname (ffiClassName c <> "_p"))
tmplReturnCType _ (CPT (CPTClassMove c) _) = R.CTSimple (R.sname (ffiClassName c <> "_p"))
tmplReturnCType _ (TemplateApp _) = R.CTStar R.CTVoid
tmplReturnCType _ (TemplateAppRef _) = R.CTStar R.CTVoid
tmplReturnCType _ (TemplateAppMove _) = R.CTStar R.CTVoid
tmplReturnCType _ (TemplateType _) = R.CTStar R.CTVoid
tmplReturnCType b (TemplateParam t) = case b of
  CPrim -> R.CTSimple $ R.sname t
  NonCPrim -> R.CTSimple $ R.CName [R.NamePart t, R.NamePart "_p"]
tmplReturnCType b (TemplateParamPointer t) = case b of
  CPrim -> R.CTSimple $ R.sname t
  NonCPrim -> R.CTSimple $ R.CName [R.NamePart t, R.NamePart "_p"]

-- ---------------------------
-- Template Member Function --
-- ---------------------------

tmplMemFuncArgToCTypVar :: Class -> Arg -> (R.CType Identity, R.CName Identity)
tmplMemFuncArgToCTypVar _ (Arg (CT ctyp isconst) varname) =
  (ctypToCType ctyp isconst, R.sname varname)
tmplMemFuncArgToCTypVar c (Arg SelfType varname) =
  (R.CTSimple (R.sname (ffiClassName c <> "_p")), R.sname varname)
tmplMemFuncArgToCTypVar _ (Arg (CPT (CPTEnum _) _) varname) =
  (ctypToCType CTInt Const, R.sname varname)
tmplMemFuncArgToCTypVar _ (Arg (CPT (CPTClass c) isconst) varname) =
  case isconst of
    Const -> (R.CTSimple (R.sname ("const_" <> ffiClassName c <> "_p")), R.sname varname)
    NoConst -> (R.CTSimple (R.sname (ffiClassName c <> "_p")), R.sname varname)
tmplMemFuncArgToCTypVar _ (Arg (CPT (CPTClassRef c) isconst) varname) =
  case isconst of
    Const -> (R.CTSimple (R.sname ("const_" <> ffiClassName c <> "_p")), R.sname varname)
    NoConst -> (R.CTSimple (R.sname (ffiClassName c <> "_p")), R.sname varname)
tmplMemFuncArgToCTypVar _ (Arg (CPT (CPTClassMove c) isconst) varname) =
  case isconst of
    Const -> (R.CTSimple (R.sname ("const_" <> ffiClassName c <> "_p")), R.sname varname)
    NoConst -> (R.CTSimple (R.sname (ffiClassName c <> "_p")), R.sname varname)
tmplMemFuncArgToCTypVar _ (Arg (TemplateApp _) v) = (R.CTStar R.CTVoid, R.sname v)
tmplMemFuncArgToCTypVar _ (Arg (TemplateAppRef _) v) = (R.CTStar R.CTVoid, R.sname v)
tmplMemFuncArgToCTypVar _ (Arg (TemplateAppMove _) v) = (R.CTStar R.CTVoid, R.sname v)
tmplMemFuncArgToCTypVar _ (Arg (TemplateType _) v) = (R.CTStar R.CTVoid, R.sname v)
tmplMemFuncArgToCTypVar _ (Arg (TemplateParam t) v) = (R.CTSimple (R.CName [R.NamePart t, R.NamePart "_p"]), R.sname v)
tmplMemFuncArgToCTypVar _ (Arg (TemplateParamPointer t) v) = (R.CTSimple (R.CName [R.NamePart t, R.NamePart "_p"]), R.sname v)
tmplMemFuncArgToCTypVar _ _ = error "tmplMemFuncArgToString: undefined"

tmplMemFuncReturnCType :: Class -> Types -> R.CType Identity
tmplMemFuncReturnCType _ (CT ctyp isconst) = ctypToCType ctyp isconst
tmplMemFuncReturnCType _ Void = R.CTVoid
tmplMemFuncReturnCType c SelfType = R.CTSimple (R.sname (ffiClassName c <> "_p"))
tmplMemFuncReturnCType _ (CPT (CPTEnum _) _) = ctypToCType CTInt Const
tmplMemFuncReturnCType _ (CPT (CPTClass c) _) = R.CTSimple (R.sname (ffiClassName c <> "_p"))
tmplMemFuncReturnCType _ (CPT (CPTClassRef c) _) = R.CTSimple (R.sname (ffiClassName c <> "_p"))
tmplMemFuncReturnCType _ (CPT (CPTClassCopy c) _) = R.CTSimple (R.sname (ffiClassName c <> "_p"))
tmplMemFuncReturnCType _ (CPT (CPTClassMove c) _) = R.CTSimple (R.sname (ffiClassName c <> "_p"))
tmplMemFuncReturnCType _ (TemplateApp _) = R.CTStar R.CTVoid
tmplMemFuncReturnCType _ (TemplateAppRef _) = R.CTStar R.CTVoid
tmplMemFuncReturnCType _ (TemplateAppMove _) = R.CTStar R.CTVoid
tmplMemFuncReturnCType _ (TemplateType _) = R.CTStar R.CTVoid
tmplMemFuncReturnCType _ (TemplateParam t) = R.CTSimple $ R.CName [R.NamePart t, R.NamePart "_p"]
tmplMemFuncReturnCType _ (TemplateParamPointer t) = R.CTSimple $ R.CName [R.NamePart t, R.NamePart "_p"]

c2HsType :: CTypes -> HsType GhcPs
c2HsType CTBool = Ex.tycon "CBool"
c2HsType CTChar = Ex.tycon "CChar"
c2HsType CTClock = Ex.tycon "CClock"
c2HsType CTDouble = Ex.tycon "CDouble"
c2HsType CTFile = Ex.tycon "CFile"
c2HsType CTFloat = Ex.tycon "CFloat"
c2HsType CTFpos = Ex.tycon "CFpos"
c2HsType CTInt = Ex.tycon "CInt"
c2HsType CTIntMax = Ex.tycon "CIntMax"
c2HsType CTIntPtr = Ex.tycon "CIntPtr"
c2HsType CTJmpBuf = Ex.tycon "CJmpBuf"
c2HsType CTLLong = Ex.tycon "CLLong"
c2HsType CTLong = Ex.tycon "CLong"
c2HsType CTPtrdiff = Ex.tycon "CPtrdiff"
c2HsType CTSChar = Ex.tycon "CSChar"
c2HsType CTSUSeconds = Ex.tycon "CSUSeconds"
c2HsType CTShort = Ex.tycon "CShort"
c2HsType CTSigAtomic = Ex.tycon "CSigAtomic"
c2HsType CTSize = Ex.tycon "CSize"
c2HsType CTTime = Ex.tycon "CTime"
c2HsType CTUChar = Ex.tycon "CUChar"
c2HsType CTUInt = Ex.tycon "CUInt"
c2HsType CTUIntMax = Ex.tycon "CUIntMax"
c2HsType CTUIntPtr = Ex.tycon "CUIntPtr"
c2HsType CTULLong = Ex.tycon "CULLong"
c2HsType CTULong = Ex.tycon "CULong"
c2HsType CTUSeconds = Ex.tycon "CUSeconds"
c2HsType CTUShort = Ex.tycon "CUShort"
c2HsType CTWchar = Ex.tycon "CWchar"
c2HsType CTInt8 = Ex.tycon "Int8"
c2HsType CTInt16 = Ex.tycon "Int16"
c2HsType CTInt32 = Ex.tycon "Int32"
c2HsType CTInt64 = Ex.tycon "Int64"
c2HsType CTUInt8 = Ex.tycon "Word8"
c2HsType CTUInt16 = Ex.tycon "Word16"
c2HsType CTUInt32 = Ex.tycon "Word32"
c2HsType CTUInt64 = Ex.tycon "Word64"
c2HsType CTString = Ex.tycon "CString"
c2HsType CTVoidStar = Ex.tyapp (Ex.tycon "Ptr") Ex.unit_tycon
c2HsType (CPointer t) = Ex.tyapp (Ex.tycon "Ptr") (c2HsType t)
c2HsType (CRef t) = Ex.tyapp (Ex.tycon "Ptr") (c2HsType t)

cxx2HsType :: Maybe Class -> Types -> HsType GhcPs
cxx2HsType _c Void = Ex.unit_tycon
cxx2HsType (Just c) SelfType = Ex.tycon ((fst . hsClassName) c)
cxx2HsType Nothing SelfType = error "cxx2HsType : SelfType but no class "
cxx2HsType _c (CT t _) = c2HsType t
cxx2HsType _c (CPT (CPTEnum _) _) = c2HsType CTInt
cxx2HsType _c (CPT (CPTClass c') _) = (Ex.tycon . fst . hsClassName) c'
cxx2HsType _c (CPT (CPTClassRef c') _) = (Ex.tycon . fst . hsClassName) c'
cxx2HsType _c (CPT (CPTClassCopy c') _) = (Ex.tycon . fst . hsClassName) c'
cxx2HsType _c (CPT (CPTClassMove c') _) = (Ex.tycon . fst . hsClassName) c'
cxx2HsType _c (TemplateApp x) =
  foldl1 Ex.tyapp $
    map Ex.tycon $
      tclass_name (tapp_tclass x) : map hsClassNameForTArg (tapp_tparams x)
cxx2HsType _c (TemplateAppRef x) =
  foldl1 Ex.tyapp $
    map Ex.tycon $
      tclass_name (tapp_tclass x) : map hsClassNameForTArg (tapp_tparams x)
cxx2HsType _c (TemplateAppMove x) =
  foldl1 Ex.tyapp $
    map Ex.tycon $
      tclass_name (tapp_tclass x) : map hsClassNameForTArg (tapp_tparams x)
cxx2HsType _c (TemplateType t) =
  foldl1 Ex.tyapp $
    Ex.tycon (tclass_name t) : map Ex.mkTVar (tclass_params t)
cxx2HsType _c (TemplateParam p) = Ex.mkTVar p
cxx2HsType _c (TemplateParamPointer p) = Ex.mkTVar p

cxx2HsType4Tmpl ::
  -- | self
  HsType GhcPs ->
  Maybe Class ->
  -- | type paramemter splice
  [HsType GhcPs] ->
  Types ->
  HsType GhcPs
cxx2HsType4Tmpl _ c _ Void = cxx2HsType c Void
cxx2HsType4Tmpl _ (Just c) _ SelfType = cxx2HsType (Just c) SelfType
cxx2HsType4Tmpl _ Nothing _ SelfType = cxx2HsType Nothing SelfType
cxx2HsType4Tmpl _ c _ x@(CT _ _) = cxx2HsType c x
cxx2HsType4Tmpl _ c _ x@(CPT (CPTEnum _) _) = cxx2HsType c x
cxx2HsType4Tmpl _ c _ x@(CPT (CPTClass _) _) = cxx2HsType c x
cxx2HsType4Tmpl _ c _ x@(CPT (CPTClassRef _) _) = cxx2HsType c x
cxx2HsType4Tmpl _ c _ x@(CPT (CPTClassCopy _) _) = cxx2HsType c x
cxx2HsType4Tmpl _ c _ x@(CPT (CPTClassMove _) _) = cxx2HsType c x
cxx2HsType4Tmpl _ _ ss (TemplateApp info) =
  let pss = zip (tapp_tparams info) ss
   in foldl1 Ex.tyapp $
        Ex.tycon (tclass_name (tapp_tclass info)) : map (\case (TArg_TypeParam _, s) -> s; (p, _) -> Ex.tycon (hsClassNameForTArg p)) pss
cxx2HsType4Tmpl _ _ ss (TemplateAppRef info) =
  let pss = zip (tapp_tparams info) ss
   in foldl1 Ex.tyapp $
        Ex.tycon (tclass_name (tapp_tclass info)) : map (\case (TArg_TypeParam _, s) -> s; (p, _) -> Ex.tycon (hsClassNameForTArg p)) pss
cxx2HsType4Tmpl _ _ ss (TemplateAppMove info) =
  let pss = zip (tapp_tparams info) ss
   in foldl1 Ex.tyapp $
        Ex.tycon (tclass_name (tapp_tclass info)) : map (\case (TArg_TypeParam _, s) -> s; (p, _) -> Ex.tycon (hsClassNameForTArg p)) pss
cxx2HsType4Tmpl e _ _ (TemplateType _) = e
cxx2HsType4Tmpl _ _ _ (TemplateParam p) = Ex.tySplice . Ex.parenSplice . Ex.mkVar $ p
cxx2HsType4Tmpl _ _ _ (TemplateParamPointer p) = Ex.tySplice . Ex.parenSplice . Ex.mkVar $ p

hsFuncXformer :: Function -> String
hsFuncXformer func@(Constructor {}) =
  let len = length (genericFuncArgs func)
   in if len > 0
        then "xform" <> show (len - 1)
        else "xformnull"
hsFuncXformer func@(Static {}) =
  let len = length (genericFuncArgs func)
   in if len > 0
        then "xform" <> show (len - 1)
        else "xformnull"
hsFuncXformer func =
  let len = length (genericFuncArgs func)
   in "xform" <> show len

classConstraints :: Class -> HsContext GhcPs
classConstraints =
  Ex.cxTuple . map ((\name -> Ex.classA name [Ex.mkTVar "a"]) . typeclassName) . class_parents

extractArgRetTypes ::
  -- | class (Nothing for top-level function)
  Maybe Class ->
  -- | is virtual function?
  Bool ->
  -- | C type signature information for a given function:
  --   (argument types, return type) of a given function
  CFunSig ->
  -- | Haskell type signature information for the function:
  --   (types, class constraints)
  HsFunSig
extractArgRetTypes mc isvirtual (CFunSig args ret) =
  let (typs, s) = flip runState ([], (0 :: Int)) $ do
        as <- mapM (mktyp . arg_type) args
        r <- case ret of
          SelfType -> case mc of
            Nothing -> error "extractArgRetTypes: SelfType return but no class"
            Just c ->
              if isvirtual
                then return (Ex.mkTVar "a")
                else return $ Ex.tycon ((fst . hsClassName) c)
          x -> (return . cxx2HsType Nothing) x
        return (as ++ [Ex.tyapp (Ex.tycon "IO") r])
   in HsFunSig
        { hsSigTypes = typs,
          hsSigConstraints = fst s
        }
  where
    addclass c = do
      (ctxts, n) <- get
      let cname = (fst . hsClassName) c
          iname = typeclassNameFromStr cname
          tvar = Ex.mkTVar ('c' : show n)
          ctxt1 = Ex.classA iname [tvar]
          ctxt2 = Ex.classA "FPtr" [tvar]
      put (ctxt1 : ctxt2 : ctxts, n + 1)
      return tvar
    addstring = do
      (ctxts, n) <- get
      let tvar = Ex.mkTVar ('c' : show n)
          ctxt = Ex.classA "Castable" [tvar, Ex.tycon "CString"]
      put (ctxt : ctxts, n + 1)
      return tvar
    mktyp typ =
      case typ of
        SelfType -> return (Ex.mkTVar "a")
        CT CTString Const -> addstring
        CT _ _ -> pure $ cxx2HsType Nothing typ
        CPT (CPTEnum _e) _ -> pure $ cxx2HsType Nothing (CT CTInt NoConst)
        CPT (CPTClass c') _ -> addclass c'
        CPT (CPTClassRef c') _ -> addclass c'
        CPT (CPTClassCopy c') _ -> addclass c'
        CPT (CPTClassMove c') _ -> addclass c'
        -- it is not clear whether the following is okay or not.
        (TemplateApp x) ->
          pure $
            cxx2HsType Nothing (TemplateApp x)
        (TemplateAppRef x) ->
          pure $
            cxx2HsType Nothing (TemplateAppRef x)
        (TemplateAppMove x) ->
          pure $
            cxx2HsType Nothing (TemplateAppMove x)
        (TemplateType t) ->
          pure $
            foldl1 Ex.tyapp (Ex.tycon (tclass_name t) : map Ex.mkTVar (tclass_params t))
        (TemplateParam p) -> pure (Ex.mkTVar p)
        Void -> pure Ex.unit_tycon
        _ -> error ("No such c type : " <> show typ)

functionSignature :: Class -> Function -> HsType GhcPs
functionSignature c f =
  let HsFunSig typs assts =
        extractArgRetTypes
          (Just c)
          (isVirtualFunc f)
          (CFunSig (genericFuncArgs f) (genericFuncRet f))
      ctxt = Ex.cxTuple assts
      arg0
        | isVirtualFunc f = (Ex.mkTVar "a" :)
        | isNonVirtualFunc f = (Ex.mkTVar (fst (hsClassName c)) :)
        | otherwise = id
   in Ex.qualTy ctxt (foldr1 Ex.tyfun (arg0 typs))

functionSignatureT :: TemplateClass -> TemplateFunction -> HsType GhcPs
functionSignatureT t TFun {..} =
  let (hname, _) = hsTemplateClassName t
      slf = foldl1 Ex.tyapp (Ex.tycon hname : map Ex.mkTVar (tclass_params t))
      ctyp = cxx2HsType Nothing tfun_ret
      lst = slf : map (cxx2HsType Nothing . arg_type) tfun_args
   in foldr1 Ex.tyfun (lst <> [Ex.tyapp (Ex.tycon "IO") (Ex.tyParen ctyp)])
functionSignatureT t TFunNew {..} =
  let ctyp = cxx2HsType Nothing (TemplateType t)
      lst = map (cxx2HsType Nothing . arg_type) tfun_new_args
   in foldr1 Ex.tyfun (lst <> [Ex.tyapp (Ex.tycon "IO") (Ex.tyParen ctyp)])
functionSignatureT t TFunDelete =
  let ctyp = cxx2HsType Nothing (TemplateType t)
   in ctyp `Ex.tyfun` (Ex.tyapp (Ex.tycon "IO") Ex.unit_tycon)
functionSignatureT t TFunOp {..} =
  let (hname, _) = hsTemplateClassName t
      slf = foldl1 Ex.tyapp (Ex.tycon hname : fmap Ex.mkTVar (tclass_params t))
      ctyp = cxx2HsType Nothing tfun_ret
      lst = slf : map (cxx2HsType Nothing . arg_type) (argsFromOpExp tfun_opexp)
   in foldr1 Ex.tyfun (lst <> [Ex.tyapp (Ex.tycon "IO") (Ex.tyParen ctyp)])

-- TODO: rename this and combine this with functionSignatureTMF
functionSignatureTT :: TemplateClass -> TemplateFunction -> HsType GhcPs
functionSignatureTT t f = foldr1 Ex.tyfun (lst <> [Ex.tyapp (Ex.tycon "IO") (Ex.tyParen ctyp)])
  where
    (hname, _) = hsTemplateClassName t
    ctyp = case f of
      TFun {..} -> cxx2HsType4Tmpl e Nothing spls tfun_ret
      TFunNew {} -> cxx2HsType4Tmpl e Nothing spls (TemplateType t)
      TFunDelete -> Ex.unit_tycon
      TFunOp {..} -> cxx2HsType4Tmpl e Nothing spls tfun_ret
    e = foldl1 Ex.tyapp (Ex.tycon hname : spls)
    spls = map (Ex.tySplice . Ex.parenSplice . Ex.mkVar) $ tclass_params t
    lst =
      case f of
        TFun {..} -> e : map (cxx2HsType4Tmpl e Nothing spls . arg_type) tfun_args
        TFunNew {..} -> map (cxx2HsType4Tmpl e Nothing spls . arg_type) tfun_new_args
        TFunDelete -> [e]
        TFunOp {..} -> e : map (cxx2HsType4Tmpl e Nothing spls . arg_type) (argsFromOpExp tfun_opexp)

-- TODO: rename this and combine this with functionSignatureTT
functionSignatureTMF :: Class -> TemplateMemberFunction -> HsType GhcPs
functionSignatureTMF c f =
  foldr1 Ex.tyfun (lst <> [Ex.tyapp (Ex.tycon "IO") ctyp])
  where
    spls = map (Ex.tySplice . Ex.parenSplice . Ex.mkVar) (tmf_params f)
    ctyp = cxx2HsType4Tmpl e Nothing spls (tmf_ret f)
    e = Ex.tycon (fst (hsClassName c))
    lst = e : map (cxx2HsType4Tmpl e Nothing spls . arg_type) (tmf_args f)

tmplAccessorToTFun :: Variable -> Accessor -> TemplateFunction
tmplAccessorToTFun v@(Variable (Arg {..})) a =
  case a of
    Getter ->
      TFun
        { tfun_ret = arg_type,
          tfun_name = tmplAccessorName v Getter,
          tfun_oname = tmplAccessorName v Getter,
          tfun_args = []
        }
    Setter ->
      TFun
        { tfun_ret = Void,
          tfun_name = tmplAccessorName v Setter,
          tfun_oname = tmplAccessorName v Setter,
          tfun_args = [Arg arg_type "value"]
        }

accessorCFunSig :: Types -> Accessor -> CFunSig
accessorCFunSig typ Getter = CFunSig [] typ
accessorCFunSig typ Setter = CFunSig [Arg typ "x"] Void

accessorSignature :: Class -> Variable -> Accessor -> HsType GhcPs
accessorSignature c v accessor =
  let csig = accessorCFunSig (arg_type (unVariable v)) accessor
      HsFunSig typs assts = extractArgRetTypes (Just c) False csig
      ctxt = Ex.cxTuple assts
      arg0 = (Ex.mkTVar (fst (hsClassName c)) :)
   in Ex.qualTy ctxt (foldr1 Ex.tyfun (arg0 typs))

-- | new function
hsFFIFunType :: Maybe (Selfness, Class) -> CFunSig -> HsType GhcPs
hsFFIFunType msc (CFunSig args ret) =
  foldr1 Ex.tyfun allTypes
  where
    allTypes =
      case msc of
        Nothing -> argtyps <> [Ex.tyapp (Ex.tycon "IO") rettyp]
        Just (Self, _) -> selftyp : argtyps <> [Ex.tyapp (Ex.tycon "IO") rettyp]
        Just (NoSelf, _) -> argtyps <> [Ex.tyapp (Ex.tycon "IO") rettyp]
    argtyps :: [HsType GhcPs]
    argtyps = map (hsargtype . arg_type) args
    --
    rettyp :: HsType GhcPs
    rettyp = Ex.tyParen (hsrettype ret)
    --
    selftyp = case msc of
      Just (_, c) -> Ex.tyapp Ex.tyPtr (Ex.tycon (snd (hsClassName c)))
      Nothing -> error "hsFFIFuncTyp: no self for top level function"
    --
    hsargtype :: Types -> HsType GhcPs
    hsargtype (CT ctype _) = c2HsType ctype
    hsargtype (CPT (CPTEnum _) _) = c2HsType CTInt
    hsargtype (CPT (CPTClass d) _) = Ex.tyapp Ex.tyPtr (Ex.tycon rawname)
      where
        rawname = snd (hsClassName d)
    hsargtype (CPT (CPTClassRef d) _) = Ex.tyapp Ex.tyPtr (Ex.tycon rawname)
      where
        rawname = snd (hsClassName d)
    hsargtype (CPT (CPTClassMove d) _) = Ex.tyapp Ex.tyPtr (Ex.tycon rawname)
      where
        rawname = snd (hsClassName d)
    hsargtype (CPT (CPTClassCopy d) _) = Ex.tyapp Ex.tyPtr (Ex.tycon rawname)
      where
        rawname = snd (hsClassName d)
    hsargtype (TemplateApp x) =
      Ex.tyapp Ex.tyPtr $
        foldl1 Ex.tyapp $
          map Ex.tycon $
            rawname : map hsClassNameForTArg (tapp_tparams x)
      where
        rawname = snd (hsTemplateClassName (tapp_tclass x))
    hsargtype (TemplateAppRef x) =
      Ex.tyapp Ex.tyPtr $
        foldl1 Ex.tyapp $
          map Ex.tycon $
            rawname : map hsClassNameForTArg (tapp_tparams x)
      where
        rawname = snd (hsTemplateClassName (tapp_tclass x))
    hsargtype (TemplateAppMove x) =
      Ex.tyapp Ex.tyPtr $
        foldl1 Ex.tyapp $
          map Ex.tycon $
            rawname : map hsClassNameForTArg (tapp_tparams x)
      where
        rawname = snd (hsTemplateClassName (tapp_tclass x))
    hsargtype (TemplateType t) =
      Ex.tyapp Ex.tyPtr $
        foldl1 Ex.tyapp (Ex.tycon rawname : map Ex.mkTVar (tclass_params t))
      where
        rawname = snd (hsTemplateClassName t)
    hsargtype (TemplateParam p) = Ex.mkTVar p
    hsargtype SelfType = selftyp
    hsargtype _ = error "hsFuncTyp: undefined hsargtype"
    ---------------------------------------------------------
    hsrettype Void = Ex.unit_tycon
    hsrettype SelfType = selftyp
    hsrettype (CT ctype _) = c2HsType ctype
    hsrettype (CPT (CPTEnum _) _) = c2HsType CTInt
    hsrettype (CPT (CPTClass d) _) = Ex.tyapp Ex.tyPtr (Ex.tycon rawname)
      where
        rawname = snd (hsClassName d)
    hsrettype (CPT (CPTClassRef d) _) = Ex.tyapp Ex.tyPtr (Ex.tycon rawname)
      where
        rawname = snd (hsClassName d)
    hsrettype (CPT (CPTClassCopy d) _) = Ex.tyapp Ex.tyPtr (Ex.tycon rawname)
      where
        rawname = snd (hsClassName d)
    hsrettype (CPT (CPTClassMove d) _) = Ex.tyapp Ex.tyPtr (Ex.tycon rawname)
      where
        rawname = snd (hsClassName d)
    hsrettype (TemplateApp x) =
      Ex.tyapp Ex.tyPtr $
        foldl1 Ex.tyapp $
          map Ex.tycon $
            rawname : map hsClassNameForTArg (tapp_tparams x)
      where
        rawname = snd (hsTemplateClassName (tapp_tclass x))
    hsrettype (TemplateAppRef x) =
      Ex.tyapp Ex.tyPtr $
        foldl1 Ex.tyapp $
          map Ex.tycon $
            rawname : map hsClassNameForTArg (tapp_tparams x)
      where
        rawname = snd (hsTemplateClassName (tapp_tclass x))
    hsrettype (TemplateAppMove x) =
      Ex.tyapp Ex.tyPtr $
        foldl1 Ex.tyapp $
          map Ex.tycon $
            rawname : map hsClassNameForTArg (tapp_tparams x)
      where
        rawname = snd (hsTemplateClassName (tapp_tclass x))
    hsrettype (TemplateType t) =
      Ex.tyapp Ex.tyPtr $
        foldl1 Ex.tyapp (Ex.tycon rawname : map Ex.mkTVar (tclass_params t))
      where
        rawname = snd (hsTemplateClassName t)
    hsrettype (TemplateParam p) = Ex.mkTVar p
    hsrettype (TemplateParamPointer p) = Ex.mkTVar p

genericFuncRet :: Function -> Types
genericFuncRet f =
  case f of
    Constructor {} -> self_
    Destructor {} -> void_
    _ -> func_ret f

genericFuncArgs :: Function -> [Arg]
genericFuncArgs (Destructor _) = []
genericFuncArgs f = func_args f
