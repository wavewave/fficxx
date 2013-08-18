-- | "FFICXX.Generate.Type.Internal" contains all ADT definitions used for class and top-level functions
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module FFICXX.Generate.Type.Internal where

import Data.Monoid
import qualified Data.Map as M

newtype ProtectedMethod = Protected { unProtected :: [String] }
    deriving (Show, Monoid)

-- | Cabal configs
data Cabal = Cabal { cabal_pkgname :: String
                   , cabal_cheaderprefix :: String
                   , cabal_moduleprefix :: String }
             deriving (Show)

-- | CPPType used for describing a C/C++ type
data CPPType c = Ptr        (CPPType c) -- ^ Pointer to a type
               | Ref        (CPPType c) -- ^ Reference to a type
               | Arr        Int       (CPPType c) -- ^ Array of a type
               | Fun        [CPPType c] (CPPType c) -- ^ A function
               | QConst     (CPPType c) -- ^ const type
               | QVolatile  (CPPType c) -- ^ volatile type
               | QRestrict  (CPPType c) -- ^ restrict type, not supported, will give an error if passed such type
               | MPtr       c (PrimitiveTypes c) -- ^ Member pointer to class, the `CPPType` here should be a primitive type
               | PrimType  (PrimitiveTypes c) -- ^ Primitive c/c++ types
             deriving (Show, Eq)

-- | Primitive types of C/C++
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
                      | CPTClass c  -- ^ c denotes classes or templates
                    deriving (Show, Eq)

-- | The name of a class
newtype ClassName = ClassName { unClassName :: String }
                  deriving (Show)

-- | SimpleCPPType indicates a class declaration or an identifier appeared in cdecls,
-- it only contains the name of the class
type SimpleCPPType = CPPType ClassName

class CPPNameable c where -- ^ CPPType that can be identified by names
  cppname :: c -> String

-- | Class definition
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
            deriving (Show)


-- | A wrapper for C++ namespaces
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

-- | A class module indicates a separate module in Haskell, it contains all related info of a C++ class
data ClassModule = ClassModule
                   { cmModule :: String
                   , cmClass :: [Class]
                   , cmCIH :: [ClassImportHeader]
                   , cmImportedModulesHighNonSource :: [String]
                   , cmImportedModulesRaw :: [String]
                   , cmImportedModulesHighSource :: [String]
                   , cmImportedModulesForFFI :: [String]
                   } deriving (Show)

-- | Global map of classes
data ClassGlobal = ClassGlobal
                   { cgDaughterSelfMap :: DaughterMap
                   , cgDaughterMap :: DaughterMap
                   }
-- ==== Funtion related types ===
-- | Member function type
type Function = SimpleCPPType

-- | static and extern keywords
data StaticExtern = Static | Extern 
                  deriving Show 

-- | Function arguments, used in both top-level and member functions
type Args c = [(CPPType c,String)]

-- | ADT used for only top-level functions
data TopLevelFunction c = TopLevelFunction { toplevelfunc_ret :: CPPType c
                                           , toplevelfunc_name :: String
                                           , toplevelfunc_args :: Args c
                                           , toplevelfunc_alias :: Maybe String
                                           , toplevelfunc_staticextern :: Maybe StaticExtern  
                                           }
                        deriving Show

-- A DaughterMap is a map from one class to all its subclasses
type DaughterMap = M.Map
                   String -- ^ Class name
                   [Class] -- ^ subclasses
