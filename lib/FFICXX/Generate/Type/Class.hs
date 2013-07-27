{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Module      : FFICXX.Generate.Type.Class
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module FFICXX.Generate.Type.Class where

import Control.Applicative ((<$>),(<*>))
import Data.Char
import Data.List 
import Data.Monoid 
import qualified Data.Map as M
import System.FilePath 
-- 
import FFICXX.Generate.Util

-- | C types
data PrimitiveTypes c = CPTChar
                      | CPTInt
                      | CPTLong
                      | CPTUChar
                      | CPTUInt
                      | CPTULong
                      | CPTLongLong
                      | CPTULongLong
                      | CPTDouble
                      | CPTLongDouble
                      | CPTBool
                      | CPTVoid
                      | CPTClass c   -- String
                    deriving (Show, Eq)
nsa
data CPPType c = Ptr        (CPPType c) -- ^ Pointer to a type
               | Ref        (CPPType c) -- ^ Reference to a type
               | Arr        Int       (CPPType c) -- ^ Array of a type
               | Fun        [CPPType c] (CPPType c) -- ^ A function
               | QConst     (CPPType c) -- ^ const type
               | QVolatile  (CPPType c) -- ^ volatile type
               | QRestrict  (CPPType c) -- ^ restrict type, not supported, will give an error if passed such type
               | MPtr       c    (CPPType c) -- ^ Member pointer to class
               | PrimType   PrimitiveTypes -- ^ Primitive c/c++ types
               deriving (Show, Eq)


type SimpleCPPType = CPPType String

type FullCPPType = CPPType Class

class CPPNameable c where
  cppname :: c -> String

cvarToStr :: (CPPNameable c) => CPPType c -> String -> String
cvarToStr t varname = (ctypToStr t) `connspace` varname 

ctypToStr :: (CPPNameable c) => CPPType c -> String
ctypToStr (Ptr t) = ctypToStr t ++ "*"
ctypToStr (Ref t) = ctypToStr t ++ "&"
ctypToStr (Arr n t) = ctypToStr t ++ "[" ++ show n ++ "]"
ctypToStr (Fun ts t) = "(" ++ ctypToStr t ++ "*) (" ++ ((intercalate "," . map ctypToStr) ts)  
ctypToStr (QConst t) = "const " ++ ctypToStr t 
ctypToStr (QVolatile t) = "volatile " ++ ctypToStr t
ctypToStr (QRestrict t) = "restrict " ++ ctypToStr t
ctypToStr (MPtr s t) = ctypToStr t ++ " " ++ cppname s + "::*r" 
ctypToStr (PrimiType prim) = 
  case prim of 
    CPTChar -> "char"
    CPTInt -> "int"
    CPTLong -> "long"
    CPTUChar -> "unsigned char"
    CPTUInt -> "unsigned int"
    CPTULong -> "unsinged long"
    CPTLongLong -> "long long"
    CPTDouble -> "double"
    CPTLongDouble -> "long double"
    CPTBool -> "bool"
    CPTVoid -> "void"
    CPTClass str -> cppname str -- template class may have problem here.


-- self_ :: Types 
-- self_ = SelfType

-- | const char* type
cstring_ :: CPPType
cstring_ = QConst (Ptr CPTChar) 

-- | const int type
cint_ :: CPPType
cint_ = QConst CPThInt

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
char_ = CT CTChar NoConst 

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


-- star_ :: CTypes -> Types 
-- star_ t = CT (CPointer t) NoConst

-- cstar_ :: CTypes -> Types 
-- cstar_ t = CT (CPointer t) Cons
s
-- self :: String -> (Types, String)
-- self var = (self_, var)

makeTypVar :: CPPType -> String -> (CPPType,String) 

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
aIio
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

charpp :: String -> (Types, String)
charpp var = (charpp_, var)

star :: CTypes -> String -> (Types, String)
star t var = (star_ t, var) 

cstar :: CTypes -> String -> (Types, String)
cstar t var = (cstar_ t, var) 
-}

{-
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
-}

-- | haskell representation of C++ types
hsCPPTypeName :: CPPType Class -> String 
hsCPPTypeName (PrimType prim) = 
  case prim of 
    CPTChar -> "CChar"
    CPTInt -> "CInt"
    CPTLong -> "CLong"
    CPTUChar -> "CUChar"
    CPTUInt -> "CUInt"
    CPTULong -> "CULong"
    CPTLongLong -> "CLLong"
    CPTDouble -> "CDouble"
    CPTLongDouble -> "CDouble" -- not long double.. maybe problematic 
    CPTBool -> "CInt"
    CPTVoid -> "()"
    CPTClass c -> "(Ptr "++rawname++")"  where rawname = snd (hsClassName c)
    
mkFullCPPType :: SomeGlobalNameMap -> CPPType String -> FFICXXMONAD (CPPType Class)

    
    strTy -- template class may have problem here.                             

hsCppTypeName :: CPPTypes -> String
hsCppTypeName (CPTClass c) =  
hsCppTypeName (CPTClassRef c) = "(Ptr "++rawname++")" where rawname = snd (hsClassName c)


{-
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
-}


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
              | NonVirtual { func_ret :: Types hicic
                           , func_name :: String
                           , func_args :: Args 
                           , func_alias :: Maybe String 
                           }
              | Static     { func_ret :: Types 
                           , func_name :: String
                           , func_args :: Args 
                           , func_alias :: Maybe String 
                           }
{-              | AliasVirtual { func_ret :: Types 
                             , func_name :: String
                             , func_args :: Args 
                             , func_alias :: String } -}
              | Destructor  { func_alias :: Maybe String } 
              deriving Show


data TopLevelFunction = TopLevelFunction { toplevelfunc_ret :: Types 
                                         , toplevelfunc_name :: String
                                         , toplevelfunc_args :: Args 
                                         , toplevelfunc_alias :: Maybe String 
                                         }
                      deriving Show 

hsFrontNameForTopLevelFunction :: TopLevelFunction -> String 
hsFrontNameForTopLevelFunction tfn = 
    let (x:xs) = maybe (toplevelfunc_name tfn) id (toplevelfunc_alias tfn) 
    in toLower x : xs 


data TopLevelImportHeader = TopLevelImportHeader { tihHeaderFileName :: String 
                                                 , tihClassDep :: [ClassImportHeader] 
                                                 , tihFuncs :: [TopLevelFunction] 
                                                 } 


  
isNewFunc :: Function -> Bool 
isNewFunc (Constructor _ _) = True 
isNewFunc _ = False

isDeleteFunc :: Function -> Bool 
isDeleteFunc (Destructor _) = True 
isDeleteFunc _ = False
       
isVirtualFunc :: Function -> Bool 
isVirtualFunc (Destructor _)           = True
isVirtualFunc (Virtual _ _ _ _)        = True 
-- isVirtualFunc (AliasVirtual _ _ _ _) = True 
isVirtualFunc _ = False 

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
argToCallString (_,varname) = varname

argsToCallString :: Args -> String
argsToCallString = intercalateWith conncomma argToCallString


rettypeToString :: Types -> String 
rettypeToString (CT ctyp isconst) = ctypToStr ctyp isconst
rettypeToString Void = "void"
rettypeToString SelfType = "Type ## _p"
rettypeToString (CPT (CPTClass c) _) = str ++ "_p"
  where str = class_name c 
rettypeToString (CPT (CPTClassRef c) _) = str ++ "_p" 
  where str = class_name c 

--------

newtype ProtectedMethod = Protected { unProtected :: [String] } 
    deriving (Monoid) 

data Cabal = Cabal { cabal_pkgname :: String
                   , cabal_cheaderprefix :: String
                   , cabal_moduleprefix :: String } 


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


newtype Namespace = NS { unNamespace :: String } deriving (Show)

data ClassImportHeader = ClassImportHeader
                       { cihClass :: Class 
                       , cihSelfHeader :: String 
                       , cihNamespace :: [Namespace] 
                       , cihSelfCpp :: String
                       , cihIncludedHPkgHeadersInH :: [String] 
                       , cihIncludedHPkgHeadersInCPP :: [String] 
                       , cihIncludedCPkgHeaders :: [String] 
                       } deriving (Show)

data ClassModule = ClassModule 
                   { cmModule :: String
                   , cmClass :: [Class] 
                   , cmCIH :: [ClassImportHeader] 
                   , cmImportedModulesHighNonSource :: [String]
                   , cmImportedModulesRaw :: [String] 
                   , cmImportedModulesHighSource :: [String]
                   , cmImportedModulesForFFI :: [String]
                   } deriving (Show)

data ClassGlobal = ClassGlobal 
                   { cgDaughterSelfMap :: DaughterMap 
                   , cgDaughterMap :: DaughterMap
                   } 

-- | Check abstract class

isAbstractClass :: Class -> Bool 
isAbstractClass (Class _ _ _ _ _ _) = False 
isAbstractClass (AbstractClass _ _ _ _ _ _) = True            

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

-- | this function will be deprecated        
ctypeToHsType :: Class -> Types -> String
ctypeToHsType _c Void = "()" 
ctypeToHsType c SelfType = (fst.hsClassName) c
ctypeToHsType _c (CT CTString _) = "String"
ctypeToHsType _c (CT CTInt _) = "Int" 
ctypeToHsType _c (CT CTUInt _) = "Word"
ctypeToHsType _c (CT CTChar _) = "Word8"
ctypeToHsType _c (CT CTLong _) = "CLong"
ctypeToHsType _c (CT CTULong _) = "CULong" 
ctypeToHsType _c (CT CTDouble _) = "Double"
ctypeToHsType _c (CT CTBool _ ) = "Int"
ctypeToHsType _c (CT CTDoubleStar _) = "[Double]"
ctypeToHsType _c (CT CTVoidStar _) = "(Ptr ())"
ctypeToHsType _c (CT CTIntStar _) = "[Int]" 
ctypeToHsType _c (CT CTCharStarStar _) = "[String]"
ctypeToHsType _c (CT (CPointer t) _) = hsCTypeName (CPointer t) 
ctypeToHsType _c (CPT (CPTClass c') _) = class_name c'
ctypeToHsType _c (CPT (CPTClassRef c') _) = class_name c'

-- | 
ctypToHsTyp :: Maybe Class -> Types -> String
ctypToHsTyp _c Void = "()" 
ctypToHsTyp (Just c) SelfType = (fst.hsClassName) c
ctypToHsTyp Nothing SelfType = error "ctypToHsTyp : SelfType but no class " 
ctypToHsTyp _c (CT CTString _) = "String"
ctypToHsTyp _c (CT CTInt _) = "Int" 
ctypToHsTyp _c (CT CTUInt _) = "Word"
ctypToHsTyp _c (CT CTChar _) = "Word8"
ctypToHsTyp _c (CT CTLong _) = "CLong"
ctypToHsTyp _c (CT CTULong _) = "CULong" 
ctypToHsTyp _c (CT CTDouble _) = "Double"
ctypToHsTyp _c (CT CTBool _ ) = "Int"
ctypToHsTyp _c (CT CTDoubleStar _) = "[Double]"
ctypToHsTyp _c (CT CTVoidStar _) = "(Ptr ())"
ctypToHsTyp _c (CT CTIntStar _) = "[Int]" 
ctypToHsTyp _c (CT CTCharStarStar _) = "[String]"
ctypToHsTyp _c (CT (CPointer t) _) = hsCTypeName (CPointer t) 
ctypToHsTyp _c (CPT (CPTClass c') _) = class_name c'
ctypToHsTyp _c (CPT (CPTClassRef c') _) = class_name c'


typeclassName :: Class -> String
typeclassName c = 'I' : fst (hsClassName c)

typeclassNameFromStr :: String -> String 
typeclassNameFromStr = ('I':)

hsClassName :: Class -> (String, String)  -- ^ High-level, 'Raw'-level
hsClassName c = 
  let cname = maybe (class_name c) id (class_alias c)
  in (cname, "Raw" ++ cname) 

existConstructorName :: Class -> String 
existConstructorName c = 'E' : (fst.hsClassName) c 

-- | this is for FFI type.
hsFuncTyp :: Class -> Function -> String
hsFuncTyp c f = let args = genericFuncArgs f 
                    ret  = genericFuncRet f 
                in  selfstr ++ " -> " ++ concatMap ((++ " -> ") . hsargtype . fst) args ++ hsrettype ret 
                    
  where (_hcname,rcname) = hsClassName c
        selfstr = "(Ptr " ++ rcname ++ ")" 

        hsargtype (CT ctype _) = hsCTypeName ctype
        hsargtype (CPT x _) = hsCppTypeName x 
        hsargtype SelfType = selfstr 
        hsargtype _ = error "undefined hsargtype"
        
        hsrettype Void = "IO ()"
        hsrettype SelfType = "IO " ++ selfstr
        hsrettype (CT ctype _) = "IO " ++ hsCTypeName ctype
        hsrettype (CPT x _ ) = "IO " ++ hsCppTypeName x 

-- | this is for FFI
hsFuncTypNoSelf :: Class -> Function -> String
hsFuncTypNoSelf c f = let args = genericFuncArgs f 
                          ret  = genericFuncRet f 
                      in  intercalateWith connArrow id $ map (hsargtype . fst) args ++ [hsrettype ret]  
                          
  where (_hcname,rcname) = hsClassName c
        selfstr = "(Ptr " ++ rcname ++ ")" 

        hsargtype (CT ctype _) = hsCTypeName ctype
        hsargtype (CPT x _) = hsCppTypeName x 
        hsargtype SelfType = selfstr
        hsargtype _ = error "undefined hsargtype"
        
        hsrettype Void = "IO ()"
        hsrettype SelfType = "IO " ++ selfstr
        hsrettype (CT ctype _) = "IO " ++ hsCTypeName ctype
        hsrettype (CPT x _ ) = "IO " ++ hsCppTypeName x 


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
    -- AliasVirtual t _ _ _ -> t
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
    -- AliasVirtual _ _  _ alias -> alias 
    Destructor a -> maybe destructorName id a

cppStaticName :: Class -> Function -> String 
cppStaticName c f = class_name c ++ "::" ++ func_name f

cppFuncName :: Class -> Function -> String 
cppFuncName c f =   case f of 
    Constructor _ _ -> "new"
    Virtual _ _  _ _ -> func_name f 
    NonVirtual _ _ _ _-> func_name f  
    Static _ _ _ _-> cppStaticName c f 
    -- AliasVirtual _ _  _ _ _ -> func_name f 
    Destructor _ -> destructorName

constructorName :: Class -> String
constructorName c = "new" ++ (fst.hsClassName) c 
 
nonvirtualName :: Class -> String -> String
nonvirtualName c str = (firstLower.fst.hsClassName) c ++ str 

destructorName :: String 
destructorName = "delete" 




