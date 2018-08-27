{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      : FFICXX.Generate.Type.Class
-- Copyright   : (c) 2011-2018 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module FFICXX.Generate.Type.Class where

import qualified Data.Map                     as M
import           Data.Monoid                       ( (<>) )
--
import           FFICXX.Generate.Type.Cabal

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
              | CPTClassCopy Class
              | CPTClassMove Class
              deriving Show

-- | const flag
data IsConst = Const | NoConst
             deriving Show

-- | Argument type which can be used as an template argument like float
--   in vector<float>.
--   For now, this distinguishes Class and non-Class.
data TemplateArgType = TArg_Class Class
                     | TArg_Other String
                     deriving Show

-- TODO: merge this with CFunSig. change String to Types.
data FunctionType = FunctionType {
                      ft_ret :: String
                    , ft_args :: [String]
                    }
                  deriving Show

-- | Template parameter type T. template<typename T> C ;
--   Distinguish function pointer type parameter and simple type parameter.
--   Later, we will adapt this to multi-parameters.
data TemplateParamType = TParam_Simple String
                       | TParam_Function String FunctionType

instance Show TemplateParamType where
  show (TParam_Simple s) = s
  show (TParam_Function s _) = "function<" <> s <> ">"


data Types = Void
           | SelfType
           | CT  CTypes IsConst
           | CPT CPPTypes IsConst
           | TemplateApp { tapp_hstemplate :: TemplateClass
                         , tapp_HaskellTypeForParam :: TemplateArgType
                         , tapp_CppTypeForParam :: String }
             -- ^ like vector<float>
           | TemplateAppRef { tappref_hstemplate :: TemplateClass
                            , tappref_HaskellTypeForParam :: TemplateArgType
                            , tappref_CppTypeForParam :: String }
             -- ^ like vector<float>&
           | TemplateType TemplateClass  -- ^ the template class itself. myclass<T> in template<typename T> myclass;
           | TemplateParam String        -- ^ parameter class. T in template<typename T> myclass;
           | TemplateParamPointer String -- ^ this is A* with template<A>
           deriving Show

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


data Variable = Variable { var_type :: Types
                         , var_name :: String
                         }
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

--------

newtype ProtectedMethod = Protected { unProtected :: [String] }
                        deriving (Monoid)


data ClassAlias = ClassAlias { caHaskellName :: String
                             , caFFIName :: String
                             }

-- TODO: partial record must be avoided.
data Class = Class { class_cabal :: Cabal
                   , class_name :: String
                   , class_parents :: [Class]
                   , class_protected :: ProtectedMethod
                   , class_alias :: Maybe ClassAlias
                   , class_funcs :: [Function]
                   , class_vars :: [Variable]
                   }
           | AbstractClass { class_cabal :: Cabal
                           , class_name :: String
                           , class_parents :: [Class]
                           , class_protected :: ProtectedMethod
                           , class_alias :: Maybe ClassAlias
                           , class_funcs :: [Function]
                           , class_vars :: [Variable]
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


data TemplateFunction = TFun { tfun_ret :: Types
                             , tfun_name :: String
                             , tfun_oname :: String
                             , tfun_args :: Args
                             , tfun_alias :: Maybe String }
                      | TFunNew { tfun_new_args :: Args
                                , tfun_new_alias :: Maybe String
                                }
                      | TFunDelete


data TemplateClass = TmplCls { tclass_cabal :: Cabal
                             , tclass_name :: String
                             , tclass_oname :: String
                             , tclass_param :: TemplateParamType -- String
                             , tclass_funcs :: [TemplateFunction]
                             }

-- TODO: we had better not override standard definitions
instance Show TemplateClass where
  show x = show (tclass_name x <> "<" <> show (tclass_param x) <> ">")

-- TODO: we had better not override standard definitions
instance Eq TemplateClass where
  (==) x y = tclass_name x == tclass_name y

-- TODO: we had better not override standard definitions
instance Ord TemplateClass where
  compare x y = compare (tclass_name x) (tclass_name y)


data ClassGlobal = ClassGlobal
                   { cgDaughterSelfMap :: DaughterMap
                   , cgDaughterMap :: DaughterMap
                   }

data Selfness = Self | NoSelf



-- | Check abstract class

isAbstractClass :: Class -> Bool
isAbstractClass Class{}         = False
isAbstractClass AbstractClass{} = True



type DaughterMap = M.Map String [Class]

data Accessor = Getter | Setter
              deriving (Show, Eq)
