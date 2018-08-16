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
              deriving Show

-- | const flag
data IsConst = Const | NoConst
             deriving Show

data Types = Void
           | SelfType
           | CT  CTypes IsConst
           | CPT CPPTypes IsConst
           | TemplateApp { tapp_hstemplate :: TemplateClass
                         , tapp_HaskellTypeForParam :: String
                         , tapp_CppTypeForParam :: String }
           | TemplateAppRef { tappref_hstemplate :: TemplateClass
                            , tappref_HaskellTypeForParam :: String
                            , tappref_CppTypeForParam :: String }
           | TemplateType TemplateClass
           | TemplateParam String
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

data AddCInc = AddCInc FilePath String

data AddCSrc = AddCSrc FilePath String

newtype CabalName = CabalName { unCabalName :: String }
                  deriving (Show,Eq,Ord)

data Cabal = Cabal  { cabal_pkgname       :: CabalName
                    , cabal_cheaderprefix :: String
                    , cabal_moduleprefix  :: String
                    , cabal_additional_c_incs :: [AddCInc]
                    , cabal_additional_c_srcs :: [AddCSrc]
                    , cabal_additional_pkgdeps :: [CabalName]
                    , cabal_license          :: Maybe String
                    , cabal_licensefile      :: Maybe String
                    , cabal_extraincludedirs :: [FilePath]
                    , cabal_extralibdirs     :: [FilePath]
                    , cabal_extrafiles       :: [FilePath]
                    , cabal_pkg_config_depends :: [String]
                    }

type ClassAlias = String

data Class = Class { class_cabal :: Cabal
                   , class_name :: String
                   , class_parents :: [Class]
                   , class_protected :: ProtectedMethod
                   , class_alias :: Maybe ClassAlias
                   , class_funcs :: [Function]
                   }
           | AbstractClass { class_cabal :: Cabal
                           , class_name :: String
                           , class_parents :: [Class]
                           , class_protected :: ProtectedMethod
                           , class_alias :: Maybe String
                           , class_funcs :: [Function]
                           }

instance Show Class where
  show x = show (class_name x)

instance Eq Class where
  (==) x y = class_name x == class_name y

instance Ord Class where
  compare x y = compare (class_name x) (class_name y)


data TemplateFunction = TFun { tfun_ret :: Types
                             , tfun_name :: String
                             , tfun_oname :: String
                             , tfun_args :: Args
                             , tfun_alias :: Maybe String }
                      | TFunNew { tfun_new_args :: Args }
                      | TFunDelete
--                       deriving (Show,Eq,Ord)

data TemplateClass = TmplCls { tclass_cabal :: Cabal
                             , tclass_name :: String
                             , tclass_oname :: String
                             , tclass_param :: String
                             , tclass_funcs :: [TemplateFunction]
                             }
--                     deriving (Show,Eq,Ord)

instance Show TemplateClass where
  show x = show (tclass_name x <> " " <> tclass_param x)

instance Eq TemplateClass where
  (==) x y = tclass_name x == tclass_name y

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



