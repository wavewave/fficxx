{-# LANGUAGE RecordWildCards #-}

module FFICXX.Generate.Code.Primitive where

import           Control.Monad.Trans.State         (runState,put,get)
import           Data.Char                         (toLower)
import           Data.Maybe                        (fromMaybe,maybe)
import           Data.Monoid                       ((<>))
import           Language.Haskell.Exts.Syntax      (Asst(..),Context,Type(..))
--
import           FFICXX.Generate.Type.Class
import           FFICXX.Generate.Util
import           FFICXX.Generate.Util.HaskellSrcExts

data HaskellTypeSig = HaskellTypeSig { sigTypes :: [Type ()]
                                     , sigConstraints :: [Asst ()]
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


hsCTypeName :: CTypes -> String
hsCTypeName CTString = "CString"
hsCTypeName CTChar   = "CChar"
hsCTypeName CTInt    = "CInt"
hsCTypeName CTUInt   = "CUInt"
hsCTypeName CTLong   = "CLong"
hsCTypeName CTULong  = "CULong"
hsCTypeName CTDouble = "CDouble"
hsCTypeName CTDoubleStar = "(Ptr CDouble)"
hsCTypeName CTBool   = "CInt"
hsCTypeName CTVoidStar = "(Ptr ())"
hsCTypeName CTIntStar = "(Ptr CInt)"
hsCTypeName CTCharStarStar = "(Ptr (CString))"
hsCTypeName (CPointer t) = "(Ptr " <> hsCTypeName t <> ")"
hsCTypeName (CRef t) = "(Ptr " <> hsCTypeName t <> ")"

hsFrontNameForTopLevelFunction :: TopLevelFunction -> String
hsFrontNameForTopLevelFunction tfn =
    let (x:xs) = case tfn of
                   TopLevelFunction {..} -> fromMaybe toplevelfunc_name toplevelfunc_alias
                   TopLevelVariable {..} -> fromMaybe toplevelvar_name  toplevelvar_alias
    in toLower x : xs



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
argToString (CPT (CPTClassMove c) isconst, varname) = case isconst of
    Const   -> "const_" <> cname <> "_p " <> varname
    NoConst -> cname <> "_p " <> varname
  where cname = ffiClassName c
argToString (TemplateApp _ _ _,varname) = "void* " <> varname
argToString (TemplateAppRef _ _ _,varname) = "void* " <> varname
argToString _ = error "undefined argToString"

argsToString :: Args -> String
argsToString args =
  let args' = (SelfType, "p") : args
  in  intercalateWith conncomma argToString args'

argsToStringNoSelf :: Args -> String
argsToStringNoSelf = intercalateWith conncomma argToString


argToCallString :: (Types,String) -> String
argToCallString (CT (CRef _) _,varname) = "(*"<> varname<> ")"
argToCallString (CPT (CPTClass c) _,varname) =
  -- TODO: Rewrite this with static_cast
  "to_nonconst<"<>str<>","<>str<>"_t>("<>varname<>")"
  where str = ffiClassName c
argToCallString (CPT (CPTClassRef c) _,varname) =
  -- TODO: Rewrite this with static_cast
  "to_nonconstref<"<>str<>","<>str<>"_t>(*"<>varname<>")"
  where str = ffiClassName c
argToCallString (CPT (CPTClassMove c) _,varname) =
  -- TODO: Rewrite this withd static_cast
  "std::move(to_nonconstref<"<>str<>","<>str<>"_t>(*"<>varname<>"))"
  where str = ffiClassName c
argToCallString (TemplateApp _ _ cp,varname) =
    "to_nonconst<"<>str<>",void>("<>varname<>")" where str = cp
argToCallString (TemplateAppRef _ _ cp,varname) =
    "*( ("<> str   <> "*) " <>varname<>")" where str = cp
argToCallString (_,varname) = varname

argsToCallString :: Args -> String
argsToCallString = intercalateWith conncomma argToCallString


rettypeToString :: Types -> String
rettypeToString (CT ctyp isconst)        = ctypToStr ctyp isconst
rettypeToString Void                     = "void"
rettypeToString SelfType                 = "Type ## _p"
rettypeToString (CPT (CPTClass c) _)     = ffiClassName c <> "_p"
rettypeToString (CPT (CPTClassRef c) _)  = ffiClassName c <> "_p"
rettypeToString (CPT (CPTClassCopy c) _) = ffiClassName c <> "_p"
rettypeToString (CPT (CPTClassMove c) _) = ffiClassName c <> "_p"
rettypeToString (TemplateApp _ _ _)      = "void*"
rettypeToString (TemplateAppRef _ _ _)   = "void*"
rettypeToString (TemplateType _)         = "void*"
rettypeToString (TemplateParam _)        = "Type ## _p"
rettypeToString (TemplateParamPointer _) = "Type ## _p"


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
tmplArgToString _ _ (CPT (CPTClassMove c) isconst, varname) =
  case isconst of
    Const   -> "const_" <> ffiClassName c <> "_p " <> varname
    NoConst -> ffiClassName c <> "_p " <> varname
tmplArgToString _ _ (TemplateApp _ _ _,_v) = error "tmpArgToString: TemplateApp"
tmplArgToString _ _ (TemplateAppRef _ _ _,_v) = error "tmpArgToString: TemplateAppRef"
tmplArgToString _ _ (TemplateType _,v) = "void* " <> v
tmplArgToString True  _ (TemplateParam _,v) = "Type " <> v
tmplArgToString False _ (TemplateParam _,v) = "Type ## _p " <> v
tmplArgToString True  _ (TemplateParamPointer _,v) = "Type " <> v
tmplArgToString False _ (TemplateParamPointer _,v) = "Type ## _p " <> v
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
tmplArgToCallString b (TemplateParam _,varname) =
  case b of
    True  -> varname
    False -> "*(to_nonconst<Type,Type ## _t>(" <> varname <> "))"
tmplArgToCallString b (TemplateParamPointer _,varname) =
  case b of
    True  -> varname
    False -> "to_nonconst<Type,Type ## _t>(" <> varname <> ")"
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
tmplRetTypeToString _ (TemplateApp _ _ _)      = "void*"
tmplRetTypeToString _ (TemplateAppRef _ _ _)   = "void*"
tmplRetTypeToString _ (TemplateType _)         = "void*"
tmplRetTypeToString b (TemplateParam _)        = if b then "Type" else "Type ## _p"
tmplRetTypeToString b (TemplateParamPointer _) = if b then "Type" else "Type ## _p"

-- |
ctypToHsTyp :: Maybe Class -> Types -> String
ctypToHsTyp _c Void = "()"
ctypToHsTyp (Just c) SelfType = (fst.hsClassName) c
ctypToHsTyp Nothing SelfType = error "ctypToHsTyp : SelfType but no class "
ctypToHsTyp _c (CT CTString _) = "CString"
ctypToHsTyp _c (CT CTInt _) = "CInt"
ctypToHsTyp _c (CT CTUInt _) = "CUInt"
ctypToHsTyp _c (CT CTChar _) = "CChar"
ctypToHsTyp _c (CT CTLong _) = "CLong"
ctypToHsTyp _c (CT CTULong _) = "CULong"
ctypToHsTyp _c (CT CTDouble _) = "CDouble"
ctypToHsTyp _c (CT CTBool _ ) = "CInt"
ctypToHsTyp _c (CT CTDoubleStar _) = "(Ptr CDouble)"
ctypToHsTyp _c (CT CTVoidStar _) = "(Ptr ())"
ctypToHsTyp _c (CT CTIntStar _) = "(Ptr CInt)"
ctypToHsTyp _c (CT CTCharStarStar _) = "(Ptr CString)"
ctypToHsTyp _c (CT (CPointer t) _) = hsCTypeName (CPointer t)
ctypToHsTyp _c (CT (CRef t) _) = hsCTypeName (CRef t)
ctypToHsTyp _c (CPT (CPTClass c') _) = (fst . hsClassName) c'
ctypToHsTyp _c (CPT (CPTClassRef c') _) = (fst . hsClassName) c'
ctypToHsTyp _c (CPT (CPTClassCopy c') _) = (fst . hsClassName) c'
ctypToHsTyp _c (CPT (CPTClassMove c') _) = (fst . hsClassName) c'
ctypToHsTyp _c (TemplateApp t p _) = "("<> tclass_name t <> " " <> hsClassNameForTArg p <> ")"
ctypToHsTyp _c (TemplateAppRef t p _) = "("<> tclass_name t <> " " <> hsClassNameForTArg p <> ")"
ctypToHsTyp _c (TemplateType t) = "("<> tclass_name t <> " " <> tclass_param t <> ")"
ctypToHsTyp _c (TemplateParam p) = "("<> p <> ")"
ctypToHsTyp _c (TemplateParamPointer p) = "("<> p <> ")"


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
convertCpp2HS _c Void                  = unit_tycon
convertCpp2HS (Just c) SelfType        = tycon ((fst.hsClassName) c)
convertCpp2HS Nothing SelfType         = error "convertCpp2HS : SelfType but no class "
convertCpp2HS _c (CT t _)              = convertC2HS t
convertCpp2HS _c (CPT (CPTClass c') _)     = (tycon . fst . hsClassName) c'
convertCpp2HS _c (CPT (CPTClassRef c') _)  = (tycon . fst . hsClassName) c'
convertCpp2HS _c (CPT (CPTClassCopy c') _) = (tycon . fst . hsClassName) c'
convertCpp2HS _c (CPT (CPTClassMove c') _) = (tycon . fst . hsClassName) c'
convertCpp2HS _c (TemplateApp t p _)       = tyapp (tycon (tclass_name t)) (tycon (hsClassNameForTArg p))
convertCpp2HS _c (TemplateAppRef t p _)    = tyapp (tycon (tclass_name t)) (tycon (hsClassNameForTArg p))
convertCpp2HS _c (TemplateType t)          = tyapp (tycon (tclass_name t)) (mkTVar (tclass_param t))
convertCpp2HS _c (TemplateParam p)         = mkTVar p
convertCpp2HS _c (TemplateParamPointer p)  = mkTVar p

-- |
convertCpp2HS4Tmpl :: Type () -> Maybe Class -> Type () -> Types -> Type ()
convertCpp2HS4Tmpl _ _c _ Void                  = unit_tycon
convertCpp2HS4Tmpl _ (Just c) _ SelfType        = tycon ((fst.hsClassName) c)
convertCpp2HS4Tmpl _ Nothing _ SelfType         = error "convertCpp2HS4Tmpl : SelfType but no class "
convertCpp2HS4Tmpl _ _c _ (CT t _)              = convertC2HS t
convertCpp2HS4Tmpl _ _c _ (CPT (CPTClass c') _)     = (tycon . fst . hsClassName) c'
convertCpp2HS4Tmpl _ _c _ (CPT (CPTClassRef c') _)  = (tycon . fst . hsClassName) c'
convertCpp2HS4Tmpl _ _c _ (CPT (CPTClassCopy c') _) = (tycon . fst . hsClassName) c'
convertCpp2HS4Tmpl _ _c _ (CPT (CPTClassMove c') _) = (tycon . fst . hsClassName) c'
convertCpp2HS4Tmpl e _c _ (TemplateApp _ _ _ )     = e
convertCpp2HS4Tmpl e _c _ (TemplateAppRef _ _ _ )  = e
convertCpp2HS4Tmpl e _c _ (TemplateType _)         = e
convertCpp2HS4Tmpl _ _c t (TemplateParam _)        = t
convertCpp2HS4Tmpl _ _c t (TemplateParamPointer _) = t


typeclassName :: Class -> String
typeclassName c = 'I' : fst (hsClassName c)

typeclassNameT :: TemplateClass -> String
typeclassNameT c = 'I' : fst (hsTemplateClassName c)



typeclassNameFromStr :: String -> String
typeclassNameFromStr = ('I':)

hsClassName :: Class -> (String, String)  -- ^ High-level, 'Raw'-level
hsClassName c =
  let cname = maybe (class_name c) caHaskellName (class_alias c)
  in (cname, "Raw" <> cname)

hsClassNameForTArg :: TemplateArgType -> String
hsClassNameForTArg (TArg_Class c) = fst (hsClassName c)
hsClassNameForTArg (TArg_Other s) = s

hsTemplateClassName :: TemplateClass -> (String, String)  -- ^ High-level, 'Raw'-level
hsTemplateClassName t =
  let tname = tclass_name t
  in (tname, "Raw" <> tname)

existConstructorName :: Class -> String
existConstructorName c = 'E' : (fst.hsClassName) c


ffiClassName :: Class -> String
ffiClassName c = maybe (class_name c) caFFIName (class_alias c)

hscFuncName :: Class -> Function -> String
hscFuncName c f =    "c_"
                  <> toLowers (ffiClassName c)
                  <> "_"
                  <> toLowers (aliasedFuncName c f)

hsFuncName :: Class -> Function -> String
hsFuncName c f = let (x:xs) = aliasedFuncName c f
                 in (toLower x) : xs

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


aliasedFuncName :: Class -> Function -> String
aliasedFuncName c f =
  case f of
    Constructor _ a      -> fromMaybe (constructorName c) a
    Virtual _ str _ a    -> fromMaybe str a
    NonVirtual _ str _ a -> fromMaybe (nonvirtualName c str) a
    Static _ str _ a     -> fromMaybe (nonvirtualName c str) a
    Destructor a         -> fromMaybe destructorName a

accessorName :: Class -> Variable -> Accessor -> String
accessorName c v a =    nonvirtualName c (var_name v)
                     <> "_"
                     <> case a of
                          Getter -> "get"
                          Setter -> "set"


cppStaticName :: Class -> Function -> String
cppStaticName c f = class_name c <> "::" <> func_name f

cppFuncName :: Class -> Function -> String
cppFuncName c f =   case f of
    Constructor _ _ -> "new"
    Virtual _ _  _ _ -> func_name f
    NonVirtual _ _ _ _-> func_name f
    Static _ _ _ _-> cppStaticName c f
    Destructor _ -> destructorName

constructorName :: Class -> String
constructorName c = "new" <> (fst.hsClassName) c

nonvirtualName :: Class -> String -> String
nonvirtualName c str = (firstLower.fst.hsClassName) c <> "_" <> str

destructorName :: String
destructorName = "delete"


classConstraints :: Class -> Context ()
classConstraints = cxTuple . map ((\n->classA (unqual n) [mkTVar "a"]) . typeclassName) . class_parents

extractArgRetTypes
  :: Maybe Class            -- ^ class (Nothing for top-level function)
  -> Bool                   -- ^ is virtual function?
  -> (Args,Types)           -- ^ (argument types, return type) of a given function
  -> HaskellTypeSig         -- ^ Haskell type signature information for the function    --   ([Type ()],[Asst ()])  -- ^ (types, class constraints)
extractArgRetTypes mc isvirtual (args,ret) =
  let  (typs,s) = flip runState ([],(0 :: Int)) $ do
                    as <- mapM (mktyp . fst) args
                    r <- case ret of
                           SelfType -> case mc of
                                         Nothing -> error "extractArgRetTypes: SelfType return but no class"
                                         Just c -> if isvirtual then return (mkTVar "a") else return $ tycon ((fst.hsClassName) c)
                           x -> (return . tycon . ctypToHsTyp Nothing) x
                    return (as ++ [tyapp (tycon "IO") r])
  in   HaskellTypeSig { sigTypes = typs
                      , sigConstraints = fst s
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
           CT _ _   -> return $ tycon (ctypToHsTyp Nothing typ)
           CPT (CPTClass c') _    -> addclass c'
           CPT (CPTClassRef c') _ -> addclass c'
           CPT (CPTClassCopy c') _ -> addclass c'
           CPT (CPTClassMove c') _ -> addclass c'
           -- it is not clear whether the following is okay or not.
           (TemplateApp t p _)    -> return (tyapp (tycon (tclass_name t)) (tycon (hsClassNameForTArg p)))
           (TemplateAppRef t p _) -> return (tyapp (tycon (tclass_name t)) (tycon (hsClassNameForTArg p)))
           (TemplateType t)       -> return (tyapp (tycon (tclass_name t)) (mkTVar (tclass_param t)))
           (TemplateParam p)      -> return (mkTVar p)
           Void -> return unit_tycon
           _ -> error ("No such c type : " <> show typ)

functionSignature :: Class -> Function -> Type ()
functionSignature c f =
  let HaskellTypeSig typs assts =
        extractArgRetTypes
          (Just c)
          (isVirtualFunc f)
          (genericFuncArgs f,genericFuncRet f)
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


{-
accessorSignature :: Class -> Variable -> Type ()
accessorSignature c f =
  let (typs,assts) = extractArgRetTypes (Just c) (isVirtualFunc f) (genericFuncArgs f,genericFuncRet f)
      ctxt = cxTuple assts
      arg0
        | isVirtualFunc f    = (mkTVar "a" :)
        | isNonVirtualFunc f = (mkTVar (fst (hsClassName c)) :)
        | otherwise          = id
  in TyForall () Nothing (Just ctxt) (foldr1 tyfun (arg0 typs))
-}

-- | this is for FFI type.
hsFFIFuncTyp :: Maybe (Selfness, Class) -> (Args,Types) -> Type ()
hsFFIFuncTyp msc (args,ret) =
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
        hsargtype (CT ctype _) = tycon (hsCTypeName ctype)
        hsargtype (CPT (CPTClass d) _)       = tyapp tyPtr (tycon rawname)
          where rawname = snd (hsClassName d)
        hsargtype (CPT (CPTClassRef d) _)    = tyapp tyPtr (tycon rawname)
          where rawname = snd (hsClassName d)
        hsargtype (CPT (CPTClassMove d) _)    = tyapp tyPtr (tycon rawname)
          where rawname = snd (hsClassName d)
        hsargtype (CPT (CPTClassCopy d) _)    = tyapp tyPtr (tycon rawname)
          where rawname = snd (hsClassName d)
        hsargtype (TemplateApp t p _)        = tyapp tyPtr (tyapp (tycon rawname) (tycon (hsClassNameForTArg p)))
          where rawname = snd (hsTemplateClassName t)
        hsargtype (TemplateAppRef t p _)     = tyapp tyPtr (tyapp (tycon rawname) (tycon (hsClassNameForTArg p)))
          where rawname = snd (hsTemplateClassName t)

        hsargtype (TemplateType t)           = tyapp tyPtr (tyapp (tycon rawname) (mkTVar (tclass_param t)))
          where rawname = snd (hsTemplateClassName t)
        hsargtype (TemplateParam p)          = mkTVar p
        hsargtype SelfType                   = selftyp
        hsargtype _ = error "hsFuncTyp: undefined hsargtype"
        ---------------------------------------------------------
        hsrettype Void                       = unit_tycon
        hsrettype SelfType                   = selftyp
        hsrettype (CT ctype _)               = tycon (hsCTypeName ctype)
        hsrettype (CPT (CPTClass d) _)       = tyapp tyPtr (tycon rawname)
          where rawname = snd (hsClassName d)
        hsrettype (CPT (CPTClassRef d) _)    = tyapp tyPtr (tycon rawname)
          where rawname = snd (hsClassName d)
        hsrettype (CPT (CPTClassCopy d) _)   = tyapp tyPtr (tycon rawname)
          where rawname = snd (hsClassName d)
        hsrettype (CPT (CPTClassMove d) _)   = tyapp tyPtr (tycon rawname)
          where rawname = snd (hsClassName d)
        hsrettype (TemplateApp t p _)        = tyapp tyPtr (tyapp (tycon rawname) (tycon (hsClassNameForTArg p)))
          where rawname = snd (hsTemplateClassName t)
        hsrettype (TemplateAppRef t p _)     = tyapp tyPtr (tyapp (tycon rawname) (tycon (hsClassNameForTArg p)))
          where rawname = snd (hsTemplateClassName t)
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

genericFuncArgs :: Function -> Args
genericFuncArgs (Destructor _) = []
genericFuncArgs f = func_args f
