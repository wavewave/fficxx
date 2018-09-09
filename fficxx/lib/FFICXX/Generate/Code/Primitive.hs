{-# LANGUAGE RecordWildCards #-}
module FFICXX.Generate.Code.Primitive where

import           Control.Monad.Trans.State         (runState,put,get)
import           Data.Monoid                       ((<>))
import           Language.Haskell.Exts.Syntax      (Asst(..),Context,Type(..))
--
import           FFICXX.Generate.Name
import           FFICXX.Generate.Type.Class
import           FFICXX.Generate.Util
import           FFICXX.Generate.Util.HaskellSrcExts

data HsFunSig = HsFunSig { hsSigTypes :: [Type ()]
                         , hsSigConstraints :: [Asst ()]
                         }


cvarToStr :: CTypes -> IsConst -> String -> String
cvarToStr ctyp isconst varname = ctypToStr ctyp isconst <> " " <> varname

ctypToStr :: CTypes -> IsConst -> String
ctypToStr ctyp isconst =
  let typword = case ctyp of
        CTString -> "char*"
        CTChar   -> "char"
        CTInt    -> "int"
        CTUInt   -> "unsigned int"
        CTLong   -> "signed long"
        CTULong  -> "long unsigned int"
        CTDouble -> "double"
        CTBool   -> "int"              -- Currently available solution
        CTDoubleStar -> "double *"
        CTVoidStar -> "void*"
        CTIntStar -> "int*"
        CTCharStarStar -> "char**"
        CPointer s -> ctypToStr s NoConst <> "*"
        CRef s -> ctypToStr s NoConst <> "*"
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

intref_ :: Types
intref_ = CT (CRef CTInt) NoConst


charpp_ :: Types
charpp_ = CT CTCharStarStar NoConst

ref_ :: CTypes -> Types
ref_ t = CT (CRef t) NoConst

star_ :: CTypes -> Types
star_ t = CT (CPointer t) NoConst

cstar_ :: CTypes -> Types
cstar_ t = CT (CPointer t) Const

self :: String -> (Types, String)
self var = (self_, var)

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

long :: String -> (Types,String)
long var = (long_, var)

ulong :: String -> (Types,String)
ulong var = (ulong_ , var)

clong :: String -> (Types,String)
clong var = (clong_, var)

culong :: String -> (Types,String)
culong var = (culong_ , var)

cchar :: String -> (Types,String)
cchar var = (cchar_ , var)

char :: String -> (Types,String)
char var = (char_ , var)

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

intref :: String -> (Types, String)
intref var = (intref_, var)

charpp :: String -> (Types, String)
charpp var = (charpp_, var)

ref :: CTypes -> String -> (Types,String)
ref t var = (ref_ t, var)

star :: CTypes -> String -> (Types, String)
star t var = (star_ t, var)

cstar :: CTypes -> String -> (Types, String)
cstar t var = (cstar_ t, var)


cppclass_ :: Class -> Types
cppclass_ c =  CPT (CPTClass c) NoConst

cppclass :: Class -> String -> (Types, String)
cppclass c vname = ( cppclass_ c, vname)



cppclassconst :: Class -> String -> (Types, String)
cppclassconst c vname = ( CPT (CPTClass c) Const, vname)

cppclassref_ :: Class -> Types
cppclassref_ c = CPT (CPTClassRef c) NoConst

cppclassref :: Class -> String -> (Types, String)
cppclassref c vname = (cppclassref_ c, vname)

cppclasscopy_ :: Class -> Types
cppclasscopy_ c = CPT (CPTClassCopy c) NoConst

cppclasscopy :: Class -> String -> (Types, String)
cppclasscopy c vname = (cppclasscopy_ c, vname)

cppclassmove_ :: Class -> Types
cppclassmove_ c = CPT (CPTClassMove c) NoConst

cppclassmove :: Class -> String -> (Types, String)
cppclassmove c vname = (cppclassmove_ c, vname)


argToString :: (Types,String) -> String
argToString (CT ctyp isconst, varname) = cvarToStr ctyp isconst varname
argToString (SelfType, varname) = "Type ## _p " <> varname
argToString (CPT (CPTClass c) isconst, varname) = case isconst of
    Const   -> "const_" <> cname <> "_p " <> varname
    NoConst -> cname <> "_p " <> varname
  where cname = ffiClassName c
argToString (CPT (CPTClassRef c) isconst, varname) = case isconst of
    Const   -> "const_" <> cname <> "_p " <> varname
    NoConst -> cname <> "_p " <> varname
  where cname = ffiClassName c
argToString (CPT (CPTClassCopy c) isconst, varname) = case isconst of
    Const   -> "const_" <> cname <> "_p " <> varname
    NoConst -> cname <> "_p " <> varname
  where cname = ffiClassName c
argToString (CPT (CPTClassMove c) isconst, varname) = case isconst of
    Const   -> "const_" <> cname <> "_p " <> varname
    NoConst -> cname <> "_p " <> varname
  where cname = ffiClassName c
argToString (TemplateApp     _, varname) = "void* " <> varname
argToString (TemplateAppRef  _, varname) = "void* " <> varname
argToString (TemplateAppMove _, varname) = "void* " <> varname
argToString (StdFunction _    , varname) = "STDFUNCTION_ARG " <> varname
-- Void, TemplateType, TemplateParam, TemplateParam case
-- TODO: Make explicit cases and no use of error.
argToString t = error ("argToString: " <> show t)


argsToString :: Args -> String
argsToString args =
  let args' = (SelfType, "p") : args
  in  intercalateWith conncomma argToString args'

argsToStringNoSelf :: Args -> String
argsToStringNoSelf = intercalateWith conncomma argToString

-- TODO: remove this function
argToCallString :: (Types,String) -> String
argToCallString = uncurry castC2Cpp


argsToCallString :: Args -> String
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
rettypeToString (StdFunction _sig)       = "STDFUNCTION_RET"


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
    StdFunction _          -> error "castCpp2C: StdFunction"


tmplArgToString :: Bool -> TemplateClass -> (Types,String) -> String
tmplArgToString _ _  (CT ctyp isconst, varname) = cvarToStr ctyp isconst varname
tmplArgToString _ t (SelfType, varname) = tclass_oname t <> "* " <> varname
tmplArgToString _ _ (CPT (CPTClass c) isconst, varname) =
  case isconst of
    Const   -> "const_" <> ffiClassName c <> "_p " <> varname
    NoConst -> ffiClassName c <> "_p " <> varname
tmplArgToString _ _ (CPT (CPTClassRef c) isconst, varname) =
  case isconst of
    Const   -> "const_" <> ffiClassName c <> "_p " <> varname
    NoConst -> ffiClassName c <> "_p " <> varname
tmplArgToString _ _ (CPT (CPTClassCopy c) isconst, varname) =
  case isconst of
    Const   -> "const_" <> ffiClassName c <> "_p " <> varname
    NoConst -> ffiClassName c <> "_p " <> varname
tmplArgToString _ _ (CPT (CPTClassMove c) isconst, varname) =
  case isconst of
    Const   -> "const_" <> ffiClassName c <> "_p " <> varname
    NoConst -> ffiClassName c <> "_p " <> varname
tmplArgToString _ _ (TemplateApp     _, v) = "void* " <> v
tmplArgToString _ _ (TemplateAppRef  _, v) = "void* " <> v
tmplArgToString _ _ (TemplateAppMove _, v) = "void* " <> v
tmplArgToString _ _ (TemplateType   _,  v) = "void* " <> v
tmplArgToString True  _ (TemplateParam _,v) = "Type " <> v
tmplArgToString False _ (TemplateParam _,v) = "Type ## _p " <> v
tmplArgToString True  _ (TemplateParamPointer _,v) = "Type " <> v
tmplArgToString False _ (TemplateParamPointer _,v) = "Type ## _p " <> v
tmplArgToString _ _ (StdFunction _,v) = "STDFUNCTION_TMPLARG " <> v
-- Void
-- TODO: separate out Void case.
tmplArgToString _ _ _ = error "tmplArgToString: undefined"


tmplAllArgsToString :: Bool
                    -> Selfness
                    -> TemplateClass
                    -> Args
                    -> String
tmplAllArgsToString b s t args =
  let args' = case s of
                Self -> (TemplateType t, "p") : args
                NoSelf -> args
  in  intercalateWith conncomma (tmplArgToString b t) args'



tmplArgToCallString
  :: Bool  -- ^ is primitive type?
  -> (Types,String)
  -> String
tmplArgToCallString _ (CPT (CPTClass c) _,varname) =
  -- TODO: Rewrite this with static_cast.
  "to_nonconst<"<>str<>","<>str<>"_t>("<>varname<>")" where str = ffiClassName c
tmplArgToCallString _ (CPT (CPTClassRef c) _,varname) =
  -- TODO: Rewrite this with static_cast.
  "to_nonconstref<"<>str<>","<>str<>"_t>(*"<>varname<>")" where str = ffiClassName c
tmplArgToCallString _ (CPT (CPTClassMove c) _,varname) =
  -- TODO: Rewrite this with static_cast.
  "std::move(to_nonconstref<"<>str<>","<>str<>"_t>(*"<>varname<>"))" where str = ffiClassName c
tmplArgToCallString _ (CT (CRef _) _,varname) = "(*"<> varname<> ")"
tmplArgToCallString _ (TemplateApp x,varname) =
  case tapp_tparam x of
    TArg_TypeParam _p -> "static_cast<" <> tclass_oname (tapp_tclass x) <> "<Type>*>(" <> varname <> ")"
    _ -> -- TODO: Implement this.
         error "tmplArgToCallString: TemplateApp"
tmplArgToCallString _ (TemplateAppRef x,varname) =
  case tapp_tparam x of
    TArg_TypeParam _p -> "*" <> "(static_cast<" <> tclass_oname (tapp_tclass x) <> "<Type>*>(" <> varname <> "))"
    _ -> -- TODO: Implement this.
         error "tmplArgToCallString: TemplateAppRef"
tmplArgToCallString _ (TemplateAppMove x,varname) =
  case tapp_tparam x of
    TArg_TypeParam _p -> "std::move(*" <> "(static_cast<" <> tclass_oname (tapp_tclass x) <> "<Type>*>(" <> varname <> ")))"
    _ -> -- TODO: Implement this.
         error "tmplArgToCallString: TemplateAppMove"
tmplArgToCallString b (TemplateParam _,varname) =
  case b of
    True  -> varname
    False -> "*(to_nonconst<Type,Type ## _t>(" <> varname <> "))"
tmplArgToCallString b (TemplateParamPointer _,varname) =
  case b of
    True  -> varname
    False -> "to_nonconst<Type,Type ## _t>(" <> varname <> ")"
tmplArgToCallString _ (StdFunction _,varname) = varname
-- Void, SelfType, CT CTString, CT CTChar ...
-- TODO: make this explicit.
tmplArgToCallString _ (_,varname) = varname



tmplAllArgsToCallString
  :: Bool  -- ^ is primitive type?
  -> Args
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
tmplRetTypeToString _ (StdFunction _)          = "STDFUNCTION_TMPLRET"




-- ---------------------------
-- Template Member Function --
-- ---------------------------

tmplMemFuncArgToString :: Class -> (Types,String) -> String
tmplMemFuncArgToString _  (CT ctyp isconst, varname) = cvarToStr ctyp isconst varname
tmplMemFuncArgToString c (SelfType, varname) = ffiClassName c <> "_p " <> varname
tmplMemFuncArgToString _ (CPT (CPTClass c) isconst, varname) =
  case isconst of
    Const   -> "const_" <> ffiClassName c <> "_p " <> varname
    NoConst -> ffiClassName c <> "_p " <> varname
tmplMemFuncArgToString _ (CPT (CPTClassRef c) isconst, varname) =
  case isconst of
    Const   -> "const_" <> ffiClassName c <> "_p " <> varname
    NoConst -> ffiClassName c <> "_p " <> varname
tmplMemFuncArgToString _ (CPT (CPTClassCopy c) isconst, varname) =
  case isconst of
    Const   -> "const_" <> ffiClassName c <> "_p " <> varname
    NoConst -> ffiClassName c <> "_p " <> varname
tmplMemFuncArgToString _ (CPT (CPTClassMove c) isconst, varname) =
  case isconst of
    Const   -> "const_" <> ffiClassName c <> "_p " <> varname
    NoConst -> ffiClassName c <> "_p " <> varname
tmplMemFuncArgToString _ (TemplateApp     _, v) = "void* " <> v
tmplMemFuncArgToString _ (TemplateAppRef  _, v) = "void* " <> v
tmplMemFuncArgToString _ (TemplateAppMove _, v) = "void* " <> v
tmplMemFuncArgToString _ (TemplateType   _,  v) = "void* " <> v
tmplMemFuncArgToString _ (TemplateParam _,v) = "Type##_p " <> v
tmplMemFuncArgToString _ (TemplateParamPointer _,v) = "Type##_p " <> v
tmplMemFuncArgToString _ (StdFunction _,v) = "STDFUNCTION_TMPLMEMFUNCARG " <> v
-- Void
-- TODO: separate Void type.
tmplMemFuncArgToString _ _ = error "tmplMemFuncArgToString: undefined"


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
tmplMemFuncRetTypeToString _ (StdFunction _)          = "STDFUNCTION_TMPLMEMFUNCRET"



-- |
convertC2HS :: CTypes -> Type ()
convertC2HS CTString     = tycon "CString"
convertC2HS CTChar       = tycon "CChar"
convertC2HS CTInt        = tycon "CInt"
convertC2HS CTUInt       = tycon "CUInt"
convertC2HS CTLong       = tycon "CLong"
convertC2HS CTULong      = tycon "CULong"
convertC2HS CTDouble     = tycon "CDouble"
convertC2HS CTDoubleStar = tyapp (tycon "Ptr") (tycon "CDouble")
convertC2HS CTBool       = tycon "CInt"
convertC2HS CTVoidStar   = tyapp (tycon "Ptr") unit_tycon
convertC2HS CTIntStar    = tyapp (tycon "Ptr") (tycon "CInt")
convertC2HS CTCharStarStar = tyapp (tycon "Ptr") (tycon "CString")
convertC2HS (CPointer t) = tyapp (tycon "Ptr") (convertC2HS t)
convertC2HS (CRef t)     = tyapp (tycon "Ptr") (convertC2HS t)

-- |
convertCpp2HS :: Maybe Class -> Types -> Type ()
convertCpp2HS _c Void                      = unit_tycon
convertCpp2HS (Just c) SelfType            = tycon ((fst.hsClassName) c)
convertCpp2HS Nothing SelfType             = error "convertCpp2HS : SelfType but no class "
convertCpp2HS _c (CT t _)                  = convertC2HS t
convertCpp2HS _c (CPT (CPTClass c') _)     = (tycon . fst . hsClassName) c'
convertCpp2HS _c (CPT (CPTClassRef c') _)  = (tycon . fst . hsClassName) c'
convertCpp2HS _c (CPT (CPTClassCopy c') _) = (tycon . fst . hsClassName) c'
convertCpp2HS _c (CPT (CPTClassMove c') _) = (tycon . fst . hsClassName) c'
convertCpp2HS _c (TemplateApp x)           = tyapp
                                               (tycon (tclass_name (tapp_tclass x)))
                                               (tycon (hsClassNameForTArg (tapp_tparam x)))
convertCpp2HS _c (TemplateAppRef x)        = tyapp
                                               (tycon (tclass_name (tapp_tclass x)))
                                               (tycon (hsClassNameForTArg (tapp_tparam x)))
convertCpp2HS _c (TemplateAppMove x)       = tyapp
                                               (tycon (tclass_name (tapp_tclass x)))
                                               (tycon (hsClassNameForTArg (tapp_tparam x)))
convertCpp2HS _c (TemplateType t)          = tyapp
                                               (tycon (tclass_name t))
                                               (mkTVar (tclass_param t))
convertCpp2HS _c (TemplateParam p)         = mkTVar p
convertCpp2HS _c (TemplateParamPointer p)  = mkTVar p
convertCpp2HS _c (StdFunction _)           = tycon "STDFUNCTION_HS"


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
convertCpp2HS4Tmpl _e c s x@(TemplateApp p) =
  case tapp_tparam p of
    TArg_TypeParam _ -> let t = tapp_tclass p
                            (hname,_) = hsTemplateClassName t
                        in tyapp (tycon hname) s
    _ -> convertCpp2HS c x
convertCpp2HS4Tmpl _e c s x@(TemplateAppRef p) =
  case tapp_tparam p of
    TArg_TypeParam _ -> let t = tapp_tclass p
                            (hname,_) = hsTemplateClassName t
                        in tyapp (tycon hname) s
    _ -> convertCpp2HS c x
convertCpp2HS4Tmpl _e c s x@(TemplateAppMove p) =
  case tapp_tparam p of
    TArg_TypeParam _ -> let t = tapp_tclass p
                            (hname,_) = hsTemplateClassName t
                        in tyapp (tycon hname) s
    _ -> convertCpp2HS c x
convertCpp2HS4Tmpl e _c _ (TemplateType _)          = e
convertCpp2HS4Tmpl _ _c s (TemplateParam _)         = s
convertCpp2HS4Tmpl _ _c s (TemplateParamPointer _)  = s
convertCpp2HS4Tmpl _ _c _ (StdFunction _)           = tycon "STDFUNCTION_HSTMPL"


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
                    as <- mapM (mktyp . fst) args
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
           Void                    -> pure unit_tycon
           SelfType                -> pure (mkTVar "a")
           CT CTString Const       -> addstring
           CT _ _                  -> pure (convertCpp2HS Nothing typ)
           CPT (CPTClass c') _     -> addclass c'
           CPT (CPTClassRef c') _  -> addclass c'
           CPT (CPTClassCopy c') _ -> addclass c'
           CPT (CPTClassMove c') _ -> addclass c'
           -- NOTE: It is not clear whether the following is okay or not.
           -- TODO: Clarify this.
           TemplateApp x           -> pure $
                                        tyapp
                                          (tycon (tclass_name (tapp_tclass x)))
                                          (tycon (hsClassNameForTArg (tapp_tparam x)))
           TemplateAppRef x        -> pure $
                                        tyapp
                                          (tycon (tclass_name (tapp_tclass x)))
                                          (tycon (hsClassNameForTArg (tapp_tparam x)))
           TemplateAppMove x       -> pure $
                                        tyapp
                                          (tycon (tclass_name (tapp_tclass x)))
                                          (tycon (hsClassNameForTArg (tapp_tparam x)))
           TemplateType t          -> pure $
                                        tyapp
                                          (tycon (tclass_name t))
                                          (mkTVar (tclass_param t))
           TemplateParam p         -> pure (mkTVar p)
           TemplateParamPointer p  -> pure (mkTVar p)
           StdFunction _           -> pure (tycon "STDFUNCTION_extractArgRetTypes")
           -- _ -> error ("No such c type : " <> show typ)

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
      lst = arg0 (map (convertCpp2HS Nothing . fst) tfun_args)
  in foldr1 tyfun (lst <> [tyapp (tycon "IO") ctyp])
functionSignatureT t TFunNew {..} =
  let ctyp = convertCpp2HS Nothing (TemplateType t)
      lst = map (convertCpp2HS Nothing . fst) tfun_new_args
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
      TFun {..}    -> e : map (convertCpp2HS4Tmpl e Nothing spl . fst) tfun_args
      TFunNew {..} -> map (convertCpp2HS4Tmpl e Nothing spl . fst) tfun_new_args
      TFunDelete -> [e]

-- TODO: rename this and combine this with functionSignatureTT
functionSignatureTMF :: Class -> TemplateMemberFunction -> Type ()
functionSignatureTMF c f = foldr1 tyfun (lst <> [tyapp (tycon "IO") ctyp])
  where
    ctyp = convertCpp2HS4Tmpl e Nothing spl (tmf_ret f)
    e = tycon (fst (hsClassName c))
    spl = tySplice (parenSplice (mkVar (tmf_param f)))
    lst = e : map (convertCpp2HS4Tmpl e Nothing spl . fst) (tmf_args f)


accessorCFunSig :: Types -> Accessor -> CFunSig
accessorCFunSig typ Getter = CFunSig [] typ
accessorCFunSig typ Setter = CFunSig [(typ,"x")] Void


accessorSignature :: Class -> Variable -> Accessor -> Type ()
accessorSignature c v accessor =
  let csig = accessorCFunSig (var_type v) accessor
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
        argtyps = map (hsargtype . fst) args
        rettyp :: Type ()
        rettyp  = hsrettype ret
        selftyp = case msc of
                    Just (_,c) -> tyapp tyPtr (tycon (snd (hsClassName c)))
                    Nothing    -> error "hsFFIFuncTyp: no self for top level function"
        hsargtype :: Types -> Type ()
        hsargtype SelfType                   = selftyp
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
        hsargtype (StdFunction _)            = tycon "STDFUNCTION_hsFFIFuncTyp_arg"
        -- Void
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
        hsrettype (TemplateApp x)            = tyapp
                                                 tyPtr
                                                 (tyapp
                                                    (tycon rawname)
                                                    (tycon (hsClassNameForTArg (tapp_tparam x))))
          where rawname = snd (hsTemplateClassName (tapp_tclass x))
        hsrettype (TemplateAppRef x)         = tyapp
                                                 tyPtr
                                                 (tyapp
                                                    (tycon rawname)
                                                    (tycon (hsClassNameForTArg (tapp_tparam x))))
          where rawname = snd (hsTemplateClassName (tapp_tclass x))
        hsrettype (TemplateAppMove x)        = tyapp
                                                 tyPtr
                                                 (tyapp
                                                    (tycon rawname)
                                                    (tycon (hsClassNameForTArg (tapp_tparam x))))
          where rawname = snd (hsTemplateClassName (tapp_tclass x))
        hsrettype (TemplateType t)           = tyapp tyPtr (tyapp (tycon rawname) (mkTVar (tclass_param t)))
          where rawname = snd (hsTemplateClassName t)
        hsrettype (TemplateParam p)          = mkTVar p
        hsrettype (TemplateParamPointer p)   = mkTVar p
        hsrettype (StdFunction _)            = tycon "STDFUNCTION_hsFFIFuncTyp_ret"



genericFuncRet :: Function -> Types
genericFuncRet f =
  case f of
    Constructor _ _ -> self_
    Virtual t _ _ _ -> t
    NonVirtual t _ _ _-> t
    Static t _ _ _ -> t
    Destructor _ -> void_

genericFuncArgs :: Function -> Args
genericFuncArgs (Destructor _) = []
genericFuncArgs f = func_args f
