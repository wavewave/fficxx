{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module FFICXX.Generate.Type.Class where

import Data.List (intercalate)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import FFICXX.Generate.Type.Cabal (Cabal)
import FFICXX.Runtime.Types (FFISafety (..))

-- | C types
data CTypes
  = CTBool
  | CTChar
  | CTClock
  | CTDouble
  | CTFile
  | CTFloat
  | CTFpos
  | CTInt
  | CTIntMax
  | CTIntPtr
  | CTJmpBuf
  | CTLLong
  | CTLong
  | CTPtrdiff
  | CTSChar
  | CTSUSeconds
  | CTShort
  | CTSigAtomic
  | CTSize
  | CTTime
  | CTUChar
  | CTUInt
  | CTUIntMax
  | CTUIntPtr
  | CTULLong
  | CTULong
  | CTUSeconds
  | CTUShort
  | CTWchar
  | CTInt8
  | CTInt16
  | CTInt32
  | CTInt64
  | CTUInt8
  | CTUInt16
  | CTUInt32
  | CTUInt64
  | CTVoidStar
  | CTString
  | CPointer CTypes
  | CRef CTypes
  deriving (Show)

-- TODO: Enum needs to be handled in the same way as Class with ModuleUnit
data EnumType = EnumType
  { enum_name :: String,
    enum_cases :: [String],
    -- TODO: this must go to ModuleUnitImports
    enum_header :: String
  }
  deriving (Show)

-- | C++ types
data CPPTypes
  = CPTEnum EnumType
  | CPTClass Class
  | CPTClassRef Class
  | CPTClassCopy Class
  | CPTClassMove Class
  deriving (Show)

-- | const flag
data IsConst = Const | NoConst
  deriving (Show)

-- | Argument type which can be used as an template argument like float
--   in vector<float>.
--   For now, this distinguishes Class and non-Class.
data TemplateArgType
  = TArg_Class Class
  | TArg_TypeParam String
  | TArg_Other String
  deriving (Show)

data TemplateAppInfo = TemplateAppInfo
  { tapp_tclass :: TemplateClass,
    tapp_tparams :: [TemplateArgType],
    tapp_CppTypeForParam :: String -- TODO: remove this
  }
  deriving (Show)

-- | Supported C++ types.
data Types
  = Void
  | SelfType
  | CT CTypes IsConst
  | CPT CPPTypes IsConst
  | -- | like vector<float>*
    TemplateApp TemplateAppInfo
  | -- | like vector<float>&
    TemplateAppRef TemplateAppInfo
  | -- | like unique_ptr<float> (using std::move)
    TemplateAppMove TemplateAppInfo
  | -- | template self? TODO: clarify this.
    TemplateType TemplateClass
  | TemplateParam String
  | -- | this is A* with template<A>
    TemplateParamPointer String
  deriving (Show)

-------------

-- | Function argument, type and variable name.
data Arg = Arg
  { arg_type :: Types,
    arg_name :: String
  }
  deriving (Show)

-- | Regular member functions in a ordinary class
data Function
  = Constructor
      { func_args :: [Arg],
        func_alias :: Maybe String
      }
  | Virtual
      { func_safety :: FFISafety,
        func_ret :: Types,
        func_name :: String,
        func_args :: [Arg],
        func_alias :: Maybe String
      }
  | NonVirtual
      { func_safety :: FFISafety,
        func_ret :: Types,
        func_name :: String,
        func_args :: [Arg],
        func_alias :: Maybe String
      }
  | Static
      { func_safety :: FFISafety,
        func_ret :: Types,
        func_name :: String,
        func_args :: [Arg],
        func_alias :: Maybe String
      }
  | Destructor
      { func_alias :: Maybe String
      }
  deriving (Show)

-- | Member variable. Isomorphic to Arg
newtype Variable = Variable {unVariable :: Arg}
  deriving (Show)

-- | Member functions of a template class.
data TemplateMemberFunction = TemplateMemberFunction
  { tmf_safety :: FFISafety,
    tmf_params :: [String],
    tmf_ret :: Types,
    tmf_name :: String,
    tmf_args :: [Arg],
    tmf_alias :: Maybe String
  }
  deriving (Show)

-- | Function defined at top level like ordinary C functions,
--   i.e. no owning class.
data TopLevel
  = TLOrdinary TLOrdinary
  | TLTemplate TLTemplate
  deriving (Show)

filterTLOrdinary :: [TopLevel] -> [TLOrdinary]
filterTLOrdinary = mapMaybe (\case TLOrdinary f -> Just f; _ -> Nothing)

filterTLTemplate :: [TopLevel] -> [TLTemplate]
filterTLTemplate = mapMaybe (\case TLTemplate f -> Just f; _ -> Nothing)

data TLOrdinary
  = TopLevelFunction
      { toplevelfunc_safety :: FFISafety,
        toplevelfunc_ret :: Types,
        toplevelfunc_name :: String,
        toplevelfunc_args :: [Arg],
        toplevelfunc_alias :: Maybe String
      }
  | TopLevelVariable
      { toplevelvar_ret :: Types,
        toplevelvar_name :: String,
        toplevelvar_alias :: Maybe String
      }
  deriving (Show)

data TLTemplate = TopLevelTemplateFunction
  { topleveltfunc_safety :: FFISafety,
    topleveltfunc_params :: [String],
    topleveltfunc_ret :: Types,
    topleveltfunc_name :: String,
    topleveltfunc_oname :: String,
    topleveltfunc_args :: [Arg]
  }
  deriving (Show)

getFunSafety :: Function -> FFISafety
getFunSafety (Constructor {}) = FFIUnsafe
getFunSafety (Destructor {}) = FFIUnsafe
getFunSafety f = func_safety f

isNewFunc :: Function -> Bool
isNewFunc (Constructor {}) = True
isNewFunc _ = False

isDeleteFunc :: Function -> Bool
isDeleteFunc (Destructor {}) = True
isDeleteFunc _ = False

isVirtualFunc :: Function -> Bool
isVirtualFunc (Destructor {}) = True
isVirtualFunc (Virtual {}) = True
isVirtualFunc _ = False

isNonVirtualFunc :: Function -> Bool
isNonVirtualFunc (NonVirtual {}) = True
isNonVirtualFunc _ = False

isStaticFunc :: Function -> Bool
isStaticFunc (Static {}) = True
isStaticFunc _ = False

virtualFuncs :: [Function] -> [Function]
virtualFuncs = filter isVirtualFunc

constructorFuncs :: [Function] -> [Function]
constructorFuncs = filter isNewFunc

nonVirtualNotNewFuncs :: [Function] -> [Function]
nonVirtualNotNewFuncs =
  filter (\x -> (not . isVirtualFunc) x && (not . isNewFunc) x && (not . isDeleteFunc) x && (not . isStaticFunc) x)

staticFuncs :: [Function] -> [Function]
staticFuncs = filter isStaticFunc

--------

newtype ProtectedMethod = Protected {unProtected :: [String]}
  deriving (Semigroup, Monoid)

data ClassAlias = ClassAlias
  { caHaskellName :: String,
    caFFIName :: String
  }

-- TODO: partial record must be avoided.
data Class
  = Class
      { class_cabal :: Cabal,
        class_name :: String,
        class_parents :: [Class],
        class_protected :: ProtectedMethod,
        class_alias :: Maybe ClassAlias,
        class_funcs :: [Function],
        class_vars :: [Variable],
        class_tmpl_funcs :: [TemplateMemberFunction],
        class_has_proxy :: Bool
      }
  | AbstractClass
      { class_cabal :: Cabal,
        class_name :: String,
        class_parents :: [Class],
        class_protected :: ProtectedMethod,
        class_alias :: Maybe ClassAlias,
        class_funcs :: [Function],
        class_vars :: [Variable],
        class_tmpl_funcs :: [TemplateMemberFunction]
      }

-- TODO: we had better not override standard definitions
instance Show Class where
  show x = show (class_name x)

-- TODO: we had better not override standard definitions
instance Eq Class where
  (==) x y = class_name x == class_name y

-- TODO: we had better not override standard definitions
instance Ord Class where
  compare x y = compare (class_name x) (class_name y)

data OpExp
  = -- | unary * (deRef) operator
    OpStar
  | -- | unary prefix ++ operator
    --    | OpAdd Arg Arg
    --    | OpMul Arg Arg
    OpFPPlus

data TemplateFunction
  = TFun
      { tfun_safety :: FFISafety,
        tfun_ret :: Types,
        tfun_name :: String,
        tfun_oname :: String,
        tfun_args :: [Arg]
      }
  | TFunNew
      { tfun_new_args :: [Arg],
        tfun_new_alias :: Maybe String
      }
  | TFunDelete
  | TFunOp
      { tfun_safety :: FFISafety,
        tfun_ret :: Types,
        -- | haskell alias for the operator
        tfun_name :: String,
        tfun_opexp :: OpExp
      }

getTFunSafety :: TemplateFunction -> FFISafety
getTFunSafety TFunNew {} = FFIUnsafe
getTFunSafety TFunDelete {} = FFIUnsafe
getTFunSafety f = tfun_safety f

argsFromOpExp :: OpExp -> [Arg]
argsFromOpExp OpStar = []
argsFromOpExp OpFPPlus = []

-- argsFromOpExp (OpAdd x y) = [x,y]
-- argsFromOpExp (OpMul x y) = [x,y]

opSymbol :: OpExp -> String
opSymbol OpStar = "*"
opSymbol OpFPPlus = "++"

-- opSymbol (OpAdd _ _) = "+"
-- opSymbol (OpMul _ _) = "*"

-- TODO: Generalize this further.

-- | Positional string interpolation form.
--   For example, "std::map<K,V>::iterator" is FormNested "std::map" "iterator"].
data Form
  = FormSimple String
  | FormNested String String

data TemplateClass = TmplCls
  { tclass_cabal :: Cabal,
    tclass_name :: String,
    tclass_cxxform :: Form,
    tclass_params :: [String],
    tclass_funcs :: [TemplateFunction],
    tclass_vars :: [Variable]
  }

-- TODO: we had better not override standard definitions
instance Show TemplateClass where
  show x = show (tclass_name x <> " " <> intercalate " " (tclass_params x))

-- TODO: we had better not override standard definitions
instance Eq TemplateClass where
  (==) x y = tclass_name x == tclass_name y

-- TODO: we had better not override standard definitions
instance Ord TemplateClass where
  compare x y = compare (tclass_name x) (tclass_name y)

data ClassGlobal = ClassGlobal
  { cgDaughterSelfMap :: DaughterMap,
    cgDaughterMap :: DaughterMap
  }

data Selfness = Self | NoSelf

-- | Check abstract class
isAbstractClass :: Class -> Bool
isAbstractClass Class {} = False
isAbstractClass AbstractClass {} = True

-- | Check having Proxy
hasProxy :: Class -> Bool
hasProxy c@Class {} = class_has_proxy c
hasProxy AbstractClass {} = False

type DaughterMap = M.Map String [Class]

data Accessor = Getter | Setter
  deriving (Show, Eq)
