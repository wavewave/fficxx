{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      : FFICXX.Generate.Type.Class
-- Copyright   : (c) 2011-2016 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module FFICXX.Generate.Type.Class where

import           Control.Applicative               ((<$>),(<*>))
import           Data.Char
import           Data.Default                      (Default(def))
import           Data.List
import qualified Data.Map                     as M
import           Language.Haskell.Exts.Syntax      (Context, Asst(..), Type(..), unit_tycon)
import           System.FilePath
--
import           FFICXX.Generate.Util
import           FFICXX.Generate.Util.HaskellSrcExts

-- some type aliases


-- | C types
data CTypes = CTString
            | CTChar
            | CTInt
            | CTUInt
            | CTLong
            | CTULong
            | CTDouble
            | CTBool
            | CTDoubleStar
            | CTVoidStar
            | CTIntStar
            | CTCharStarStar
            | CPointer CTypes
            | CRef CTypes
            deriving Show

-- | C++ types
data CPPTypes = CPTClass Class
              | CPTClassRef Class
              deriving Show

-- | const flag
data IsConst = Const | NoConst
             deriving Show

data Types = Void
           | SelfType
           | CT  CTypes IsConst
           | CPT CPPTypes IsConst
           | TemplateType TemplateClass
           deriving Show

cvarToStr :: CTypes -> IsConst -> String -> String
cvarToStr ctyp isconst varname = (ctypToStr ctyp isconst) `connspace` varname

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
        CPointer s -> ctypToStr s NoConst ++ "*"
        CRef s -> ctypToStr s NoConst ++ "*"
  in case isconst of
        Const   -> "const" `connspace` typword
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
hsCTypeName (CPointer t) = "(Ptr " ++ hsCTypeName t ++ ")"
hsCTypeName (CRef t) = "(Ptr " ++ hsCTypeName t ++ ")"

-------------

type Args = [(Types,String)]

data Function = Constructor { func_args :: Args
                            , func_alias :: Maybe String
                            }
              | Virtual { func_ret :: Types
                        , func_name :: String
                        , func_args :: Args
                        , func_alias :: Maybe String
                        }
              | NonVirtual { func_ret :: Types
                           , func_name :: String
                           , func_args :: Args
                           , func_alias :: Maybe String
                           }
              | Static     { func_ret :: Types
                           , func_name :: String
                           , func_args :: Args
                           , func_alias :: Maybe String
                           }
              | Destructor  { func_alias :: Maybe String }
              deriving Show


data TopLevelFunction = TopLevelFunction { toplevelfunc_ret :: Types
                                         , toplevelfunc_name :: String
                                         , toplevelfunc_args :: Args
                                         , toplevelfunc_alias :: Maybe String
                                         }
                      | TopLevelVariable { toplevelvar_ret :: Types
                                         , toplevelvar_name :: String
                                         , toplevelvar_alias :: Maybe String }
                      deriving Show

hsFrontNameForTopLevelFunction :: TopLevelFunction -> String
hsFrontNameForTopLevelFunction tfn =
    let (x:xs) = case tfn of
                   TopLevelFunction {..} -> maybe toplevelfunc_name id toplevelfunc_alias
                   TopLevelVariable {..} -> maybe toplevelvar_name id toplevelvar_alias
    in toLower x : xs




isNewFunc :: Function -> Bool
isNewFunc (Constructor _ _) = True
isNewFunc _ = False

isDeleteFunc :: Function -> Bool
isDeleteFunc (Destructor _) = True
isDeleteFunc _ = False

isVirtualFunc :: Function -> Bool
isVirtualFunc (Destructor _)          = True
isVirtualFunc (Virtual _ _ _ _)       = True
isVirtualFunc _                       = False

isNonVirtualFunc :: Function -> Bool
isNonVirtualFunc (NonVirtual _ _ _ _) = True
isNonVirtualFunc _                    = False



isStaticFunc :: Function -> Bool
isStaticFunc (Static _ _ _ _) = True
isStaticFunc _ = False

virtualFuncs :: [Function] -> [Function]
virtualFuncs = filter isVirtualFunc

constructorFuncs :: [Function] -> [Function]
constructorFuncs = filter isNewFunc

nonVirtualNotNewFuncs :: [Function] -> [Function]
nonVirtualNotNewFuncs =
  filter (\x -> (not.isVirtualFunc) x && (not.isNewFunc) x && (not.isDeleteFunc) x && (not.isStaticFunc) x )

staticFuncs :: [Function] -> [Function]
staticFuncs = filter isStaticFunc

argToString :: (Types,String) -> String
argToString (CT ctyp isconst, varname) = cvarToStr ctyp isconst varname
argToString (SelfType, varname) = "Type ## _p " ++ varname
argToString (CPT (CPTClass c) isconst, varname) = case isconst of
    Const   -> "const_" ++ cname ++ "_p " ++ varname
    NoConst -> cname ++ "_p " ++ varname
  where cname = class_name c
argToString (CPT (CPTClassRef c) isconst, varname) = case isconst of
    Const   -> "const_" ++ cname ++ "_p " ++ varname
    NoConst -> cname ++ "_p " ++ varname
  where cname = class_name c
argToString _ = error "undefined argToString"

argsToString :: Args -> String
argsToString args =
  let args' = (SelfType, "p") : args
  in  intercalateWith conncomma argToString args'

argsToStringNoSelf :: Args -> String
argsToStringNoSelf = intercalateWith conncomma argToString


argToCallString :: (Types,String) -> String
argToCallString (CPT (CPTClass c) _,varname) =
    "to_nonconst<"++str++","++str++"_t>("++varname++")" where str = class_name c
argToCallString (CPT (CPTClassRef c) _,varname) =
    "to_nonconstref<"++str++","++str++"_t>(*"++varname++")" where str = class_name c
argToCallString (CT (CRef _) _,varname) = "(*"++ varname++ ")"
argToCallString (_,varname) = varname

argsToCallString :: Args -> String
argsToCallString = intercalateWith conncomma argToCallString


rettypeToString :: Types -> String
rettypeToString (CT ctyp isconst) = ctypToStr ctyp isconst
rettypeToString Void = "void"
rettypeToString SelfType = "Type ## _p"
rettypeToString (CPT (CPTClass c) _) = class_name c ++ "_p"
rettypeToString (CPT (CPTClassRef c) _) = class_name c ++ "_p"
rettypeToString (TemplateType t) = tclass_name t ++ "_p"


--------

newtype ProtectedMethod = Protected { unProtected :: [String] }
    deriving (Monoid)

data Cabal = Cabal  { cabal_pkgname       :: String
                    , cabal_cheaderprefix :: String
                    , cabal_moduleprefix  :: String
                    }

data CabalAttr = CabalAttr  { cabalattr_license          :: Maybe String
                            , cabalattr_licensefile      :: Maybe String
                            , cabalattr_extraincludedirs :: [FilePath]
                            , cabalattr_extralibdirs     :: [FilePath]
                            }

instance Default CabalAttr where
    def = CabalAttr { cabalattr_license          = Nothing
                    , cabalattr_licensefile      = Nothing
                    , cabalattr_extraincludedirs = []
                    , cabalattr_extralibdirs     = []
                    }

data Class = Class { class_cabal :: Cabal
                   , class_name :: String
                   , class_parents :: [Class]
                   , class_protected :: ProtectedMethod
                   , class_alias :: Maybe String
                   , class_funcs :: [Function]
                   }
           | AbstractClass { class_cabal :: Cabal
                           , class_name :: String
                           , class_parents :: [Class]
                           , class_protected :: ProtectedMethod
                           , class_alias :: Maybe String
                           , class_funcs :: [Function]
                           }

data TemplateFunction = TFun { tfun_ret :: Types
                             , tfun_name :: String
                             , tfun_args :: Args
                             , tfun_alias :: Maybe String }


data TemplateClass = TmplCls { tclass_cabal :: Cabal
                             , tclass_name :: String
                             , tclass_funcs :: [TemplateFunction]
                             }

instance Show TemplateClass where
  show x = show (tclass_name x)



data ClassGlobal = ClassGlobal
                   { cgDaughterSelfMap :: DaughterMap
                   , cgDaughterMap :: DaughterMap
                   }

-- | Check abstract class

isAbstractClass :: Class -> Bool
isAbstractClass Class{}         = False
isAbstractClass AbstractClass{} = True

instance Show Class where
  show x = show (class_name x)

instance Eq Class where
  (==) x y = class_name x == class_name y

instance Ord Class where
  compare x y = compare (class_name x) (class_name y)

type DaughterMap = M.Map String [Class]

class_allparents :: Class -> [Class]
class_allparents c = let ps = class_parents c
                     in  if null ps
                           then []
                           else nub (ps ++ (concatMap class_allparents ps))


getClassModuleBase :: Class -> String
getClassModuleBase = (<.>) <$> (cabal_moduleprefix.class_cabal) <*> (fst.hsClassName)

getTClassModuleBase :: TemplateClass -> String
getTClassModuleBase = (<.>) <$> (cabal_moduleprefix.tclass_cabal) <*> (fst.hsTemplateClassName)



-- | Daughter map not including itself
mkDaughterMap :: [Class] -> DaughterMap
mkDaughterMap = foldl mkDaughterMapWorker M.empty
  where mkDaughterMapWorker m c = let ps = map getClassModuleBase (class_allparents c)
                                  in  foldl (addmeToYourDaughterList c) m ps
        addmeToYourDaughterList c m p = let f Nothing = Just [c]
                                            f (Just cs)  = Just (c:cs)
                                        in  M.alter f p m



-- | Daughter Map including itself as a daughter
mkDaughterSelfMap :: [Class] -> DaughterMap
mkDaughterSelfMap = foldl worker M.empty
  where worker m c = let ps = map getClassModuleBase (c:class_allparents c)
                     in  foldl (addToList c) m ps
        addToList c m p = let f Nothing = Just [c]
                              f (Just cs)  = Just (c:cs)
                          in  M.alter f p m

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
ctypToHsTyp _c (CPT (CPTClass c') _) = class_name c'
ctypToHsTyp _c (CPT (CPTClassRef c') _) = class_name c'
ctypToHsTyp _c (TemplateType t) = "("++ tclass_name t ++ " a)"


-- |
convertC2HS :: CTypes -> Type
convertC2HS CTString     = tycon "CString"
convertC2HS CTChar       = tycon "CChar"
convertC2HS CTInt        = tycon "CInt"
convertC2HS CTUInt       = tycon "CUInt"
convertC2HS CTLong       = tycon "CLong"
convertC2HS CTULong      = tycon "CULong"
convertC2HS CTDouble     = tycon "CDouble"
convertC2HS CTDoubleStar = TyApp (tycon "Ptr") (tycon "CDouble")
convertC2HS CTBool       = tycon "CInt"
convertC2HS CTVoidStar   = TyApp (tycon "Ptr") unit_tycon
convertC2HS CTIntStar    = TyApp (tycon "Ptr") (tycon "CInt")
convertC2HS CTCharStarStar = TyApp (tycon "Ptr") (tycon "CString")
convertC2HS (CPointer t) = TyApp (tycon "Ptr") (convertC2HS t)
convertC2HS (CRef t)     = TyApp (tycon "Ptr") (convertC2HS t)

-- |
convertCpp2HS :: Maybe Class -> Types -> Type
convertCpp2HS _c Void                  = unit_tycon
convertCpp2HS (Just c) SelfType        = tycon ((fst.hsClassName) c)
convertCpp2HS Nothing SelfType         = error "convertCpp2HS : SelfType but no class "
convertCpp2HS _c (CT t _)              = convertC2HS t
convertCpp2HS _c (CPT (CPTClass c') _)    = tycon (class_name c')
convertCpp2HS _c (CPT (CPTClassRef c') _) = tycon (class_name c')
convertCpp2HS _c (TemplateType t)         = TyApp (tycon (tclass_name t)) (mkTVar "a")



typeclassName :: Class -> String
typeclassName c = 'I' : fst (hsClassName c)

typeclassNameT :: TemplateClass -> String
typeclassNameT c = 'I' : fst (hsTemplateClassName c)



typeclassNameFromStr :: String -> String
typeclassNameFromStr = ('I':)

hsClassName :: Class -> (String, String)  -- ^ High-level, 'Raw'-level
hsClassName c =
  let cname = maybe (class_name c) id (class_alias c)
  in (cname, "Raw" ++ cname)

hsTemplateClassName :: TemplateClass -> (String, String)  -- ^ High-level, 'Raw'-level
hsTemplateClassName t =
  let tname = tclass_name t
  in (tname, "Raw" ++ tname)

existConstructorName :: Class -> String
existConstructorName c = 'E' : (fst.hsClassName) c


hscFuncName :: Class -> Function -> String
hscFuncName c f = "c_" ++ toLowers (class_name c) ++ "_" ++ toLowers (aliasedFuncName c f)

hsFuncName :: Class -> Function -> String
hsFuncName c f = let (x:xs) = aliasedFuncName c f
                 in (toLower x) : xs

hsFuncXformer :: Function -> String
hsFuncXformer func@(Constructor _ _) = let len = length (genericFuncArgs func)
                                       in if len > 0
                                          then "xform" ++ show (len - 1)
                                          else "xformnull"
hsFuncXformer func@(Static _ _ _ _) =
  let len = length (genericFuncArgs func)
  in if len > 0
     then "xform" ++ show (len - 1)
     else "xformnull"
hsFuncXformer func = let len = length (genericFuncArgs func)
                     in "xform" ++ show len


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

aliasedFuncName :: Class -> Function -> String
aliasedFuncName c f =
  case f of
    Constructor _ a -> maybe (constructorName c) id a
    Virtual _ str _ a -> maybe str id a
    NonVirtual _ str _ a-> maybe (nonvirtualName c str) id a
    Static _ str _ a -> maybe (nonvirtualName c str) id a
    Destructor a -> maybe destructorName id a

cppStaticName :: Class -> Function -> String
cppStaticName c f = class_name c ++ "::" ++ func_name f

cppFuncName :: Class -> Function -> String
cppFuncName c f =   case f of
    Constructor _ _ -> "new"
    Virtual _ _  _ _ -> func_name f
    NonVirtual _ _ _ _-> func_name f
    Static _ _ _ _-> cppStaticName c f
    Destructor _ -> destructorName

constructorName :: Class -> String
constructorName c = "new" ++ (fst.hsClassName) c

nonvirtualName :: Class -> String -> String
nonvirtualName c str = (firstLower.fst.hsClassName) c ++ str

destructorName :: String
destructorName = "delete"


classConstraints :: Class -> Context
classConstraints = map ((\n->ClassA (unqual n) [mkTVar "a"]) . typeclassName) . class_parents 

functionSignature :: Class -> Function -> Type
functionSignature c f =
  let ctyp = tycon $ (ctypToHsTyp (Just c) . genericFuncRet) f
      arg0
        | isVirtualFunc f    = (mkTVar "a" :)
        | isNonVirtualFunc f = (mkTVar (class_name c) :)
        | otherwise          = id
      lst = arg0 (map (convertCpp2HS (Just c) . fst) (genericFuncArgs f))
  in foldr1 TyFun (lst ++ [TyApp (tycon "IO") ctyp])



functionSignatureT :: TemplateClass -> TemplateFunction -> Type
functionSignatureT t f =
  let (hname,_) = hsTemplateClassName t
      ctyp = tycon $ (ctypToHsTyp Nothing . tfun_ret) f
      arg0 =  (TyApp (tycon hname) (mkTVar "a") :)
      lst = arg0 (map (convertCpp2HS Nothing . fst) (tfun_args f))
  in foldr1 TyFun (lst ++ [TyApp (tycon "IO") ctyp])



-- | this is for FFI type.
hsFuncTyp :: Class -> Function -> Type
hsFuncTyp c f = foldr1 TyFun (selftyp: argtyps ++ [TyApp (tycon "IO") rettyp])
  where argtyps = map (hsargtype . fst) $ genericFuncArgs f
        rettyp  = hsrettype (genericFuncRet f)
        (_hcname,rcname) = hsClassName c

        selftyp = TyApp tyPtr (tycon rcname)

        hsargtype (CT ctype _) = tycon (hsCTypeName ctype)
        hsargtype (CPT (CPTClass d) _)    = TyApp tyPtr (tycon rawname)
          where rawname = snd (hsClassName d)
        hsargtype (CPT (CPTClassRef d) _)    = TyApp tyPtr (tycon rawname)
          where rawname = snd (hsClassName d)
        hsargtype (TemplateType t) = TyApp tyPtr (TyApp (tycon rawname) (mkTVar "a"))
          where rawname = snd (hsTemplateClassName t)
        hsargtype SelfType     = selftyp
        hsargtype _ = error "undefined hsargtype"

        hsrettype Void         = unit_tycon
        hsrettype SelfType     = selftyp
        hsrettype (CT ctype _) = tycon (hsCTypeName ctype)
        hsrettype (CPT (CPTClass d) _)    = TyApp tyPtr (tycon rawname)
          where rawname = snd (hsClassName d)
        hsrettype (CPT (CPTClassRef d) _)    = TyApp tyPtr (tycon rawname)
          where rawname = snd (hsClassName d)
        hsrettype (TemplateType t) = TyApp tyPtr (TyApp (tycon rawname) (mkTVar "a"))
          where rawname = snd (hsTemplateClassName t)


-- | this is for FFI
hsFuncTypNoSelf :: Class -> Function -> Type
hsFuncTypNoSelf c f = foldr1 TyFun (argtyps ++ [TyApp (tycon "IO") rettyp])
  where argtyps = map (hsargtype . fst) $ genericFuncArgs f
        rettyp  = hsrettype (genericFuncRet f)
        (_hcname,rcname) = hsClassName c
        
        selftyp = TyApp tyPtr (tycon rcname)

        hsargtype (CT ctype _) = tycon (hsCTypeName ctype)
        hsargtype (CPT (CPTClass d) _)    = TyApp tyPtr (tycon rawname)
          where rawname = snd (hsClassName d)
        hsargtype (CPT (CPTClassRef d) _)    = TyApp tyPtr (tycon rawname)
          where rawname = snd (hsClassName d)
        hsargtype (TemplateType t) = TyApp tyPtr (TyApp (tycon rawname) (mkTVar "a"))
          where rawname = snd (hsTemplateClassName t)
        hsargtype SelfType     = selftyp
        hsargtype _ = error "undefined hsargtype"

        hsrettype Void         = unit_tycon
        hsrettype SelfType     = selftyp
        hsrettype (CT ctype _) = tycon (hsCTypeName ctype)
        hsrettype (CPT (CPTClass d) _)    = TyApp tyPtr (tycon rawname)
          where rawname = snd (hsClassName d)
        hsrettype (CPT (CPTClassRef d) _)    = TyApp tyPtr (tycon rawname)
          where rawname = snd (hsClassName d)
        hsrettype (TemplateType t) = TyApp tyPtr (TyApp (tycon rawname) (mkTVar "a"))
          where rawname = snd (hsTemplateClassName t)

