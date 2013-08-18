-- | "FFICXX.Generate.Type.Internal" contains all ADT definitions used for class and top-level functions
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module FFICXX.Generate.Type.Internal where

import Data.Monoid
import Data.List (intercalate)
import qualified Data.Map as M

-- TODO: Add support for class templates

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

-- C++ class ADTs
-- | The name of a class
newtype ClassName = ClassName { unClassName :: String }
                  deriving (Show)

-- | SimpleCPPType indicates a class declaration or an identifier appeared in cdecls,
-- it only contains the name of the class
type SimpleCPPType = CPPType ClassName

-- | CPPType that can be identified by names
class CPPNameable c where
  cppname :: c -> String

-- | Convert a primitive C/C++ type to corresponding Haskell CType name
class HSCPPTypeNameable c where
  hsCPPTypeName :: c -> String

-- | Class definition
data Class = Class { class_cabal :: Cabal
                   , class_name :: String
                   , class_parents :: [Class]
                   , class_protected :: ProtectedMethod
                   , class_alias :: Maybe String
                   , class_members :: [ClassMember Class] -- ^ Class data members and methods
                   }
           | AbstractClass { class_cabal :: Cabal
                           , class_name :: String
                           , class_parents :: [Class]
                           , class_protected :: ProtectedMethod
                           , class_alias :: Maybe String
                           , class_members :: [ClassMember Class] -- ^ Class data members and methods
                           }

-- | A wrapper for C++ namespaces
newtype Namespace = NS { unNamespace :: String } deriving (Show)

data TopLevelImportHeader c = TopLevelImportHeader { tihHeaderFileName :: String
                                                 , tihClassDep :: [ClassImportHeader]
                                                 , tihFuncs :: [TopLevelFunction c]
                                                 }
data ClassImportHeader = ClassImportHeader
                       { cihClass :: Class
                       , cihSelfHeader :: String
                       , cihNamespace :: [Namespace]
                       , cihSelfCpp :: String
                       , cihIncludedHPkgHeadersInH :: [String]
                       , cihIncludedHPkgHeadersInCPP :: [String]
                       , cihIncludedCPkgHeaders :: [String]
                       } deriving (Show)

-- | A class module indicates a separate module in Haskell,
-- it contains all related info of a C++ class
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

-- | Storage classes in C++, note that 'auto' and 'register' have no effects in
-- the generation, so we omit them.
data StorageClass = Auto    -- ^ auto can only be used for non-member data, auto rules out all other storage classes below
                  | Extern  -- ^ extern can only be used for non-member data
                  | Static  -- ^ static can used for both member and non-member data
                  | Virtual -- ^ virtual can only be used for member functions
                  | Friend  -- ^ friend can only be used in class definition
                  deriving Show

-- | A declaration of a class member. We treat constructors and destructors differently from other members
data ClassMember c = DataMember  (DataMamberType c)
                   | MethodMember (MethodMemberType c)
                   deriving Show

-- | A data member of a class
data DataMamberType c = DataMemberType { mem_storage :: StorageClass,
                                         mem_name :: String,
                                         mem_cdelc :: Args c}
                      deriving Show

-- | A member function of a class
data MethodMemberType c = Constructor { func_name :: String,
                                        func_args :: Args c, -- ^ arguments
                                        func_alias :: Maybe String }
                        | Destructor  { func_name :: String,
                                        func_storage :: StorageClass , -- ^ virtual or non-virtual, others are invalid
                                        func_args :: Args c,
                                        func_alias :: Maybe String }
                        | NormalMethod { func_storage :: StorageClass,
                                         func_name :: String,   -- ^ Original function name
                                         func_args :: Args c,
                                         func_ret :: CPPType c,
                                         func_alias :: Maybe String }
                      deriving Show

-- ==== Funtion related types ===

-- | Function arguments, used in both top-level and member functions, it contains both argument type and name(identifier)
type Args c = [(CPPType c,String)]
type SimpleArgs = Args ClassName

-- | ADT used for only top-level functions
data TopLevelFunction c = TopLevelFunction { toplevelfunc_ret :: CPPType c
                                           , toplevelfunc_name :: String
                                           , toplevelfunc_args :: Args c
                                           , toplevelfunc_alias :: Maybe String
                                           , toplevelfunc_staticextern :: StorageClass
                                           }
                        deriving Show

-- A DaughterMap is a map from one class to all its subclasses
type DaughterMap = M.Map
                   String -- ^ Class name
                   [Class] -- ^ subclasses

-- Instances

instance Show Class where
  show x = show (class_name x)

instance Eq Class where
  (==) x y = class_name x == class_name y

instance Ord Class where
  compare x y = compare (class_name x) (class_name y)

instance CPPNameable ClassName where
  cppname = unClassName

instance CPPNameable Class where
  cppname = class_name

instance (CPPNameable c) => CPPNameable (PrimitiveTypes c) where
  cppname CPTChar       = "char"
  cppname CPTInt        = "int"
  cppname CPTLong       = "long int"
  cppname CPTLongLong   = "long long"
  cppname CPTULong      = "unsigned long"
  cppname CPTUChar      = "unsigned char"
  cppname CPTUInt       = "unsigned int"
  cppname CPTULongLong  = "unsigned long long"
  cppname CPTDouble     = "double"
  cppname CPTLongDouble = "long double"
  cppname CPTBool       = "bool"
  cppname CPTVoid       = "void"
  cppname (CPTClass c)  = cppname c


instance (CPPNameable c) => CPPNameable (CPPType c) where
  cppname (Ptr fun@(Fun _ _))   = cppname fun
  cppname (Ptr t)               = cppname t ++ "*"
  cppname (Ref t)               = cppname t ++ "&"
  cppname (Arr n t)             = cppname t ++ "[" ++ (show n) ++ "]"
  cppname (Fun ts t)            = "(" ++ cppname t ++ ")" ++ " (*)(" ++ ((intercalate "," . map cppname) ts) ++ ")"
  cppname (QConst p@(Ptr _))    = cppname p ++ " const"
  cppname (QConst t)            = "const " ++ cppname t
  cppname (QVolatile p@(Ptr _)) = cppname p ++ " volatile"
  cppname (QVolatile t)         = "volatile " ++ cppname t
  cppname (QRestrict p@(Ptr _)) = cppname p ++ " restrict"
  cppname (QRestrict t)         = "restrict " ++ cppname t
  cppname (MPtr s t)            = cppname t ++ " " ++ cppname s ++ "::*"
  cppname (PrimType prim)       = cppname prim

-- Be careful of bool type: In ANSI C, bool is not definied. In C99, bool is defined as
-- an equivlant type of int, but in Visual C++ 5.0 and later, bool is defined as 1 byte.
-- And the size of bool varies in different platforms.
instance (CPPNameable c) => HSCPPTypeNameable (CPPType c) where
  -- Pointers
  hsCPPTypeName (Ptr (PrimType CPTChar))                  = "CString"
  hsCPPTypeName (Ptr (QConst (PrimType CPTChar)))         = "CString"
  hsCPPTypeName (Ptr (QVolatile (PrimType CPTChar)))      = "CString"
  hsCPPTypeName (Ptr (QRestrict (PrimType CPTChar)))      = "CString"
  hsCPPTypeName (Ptr cpt@(PrimType _))                    = "(Ptr " ++ hsCPPTypeName cpt ++ ")"
  -- References
  hsCPPTypeName (Ref (PrimType CPTChar))                  = "CString"
  hsCPPTypeName (Ref (QConst (PrimType CPTChar)))         = "CString"
  hsCPPTypeName (Ref (QVolatile (PrimType CPTChar)))      = "CString"
  hsCPPTypeName (Ref (QRestrict (PrimType CPTChar)))      = "CString"
  hsCPPTypeName (Ref cpt@(PrimType _))                    = "(Ref " ++ hsCPPTypeName cpt ++ ")"
  -- Arrays
  hsCPPTypeName (Arr _ (PrimType CPTChar))                = "CString"
  hsCPPTypeName (Arr _ cpt@(PrimType _))                  = "Ptr " ++ hsCPPTypeName cpt
  -- Primitive types
  hsCPPTypeName (PrimType CPTInt)                         = "CInt"
  hsCPPTypeName (PrimType CPTUInt)                        = "CUInt"
  hsCPPTypeName (PrimType CPTLong)                        = "CLong"
  hsCPPTypeName (PrimType CPTULong)                       = "CULong"
  hsCPPTypeName (PrimType CPTDouble)                      = "CDouble"
  hsCPPTypeName (PrimType CPTVoid)                        = "()"
  hsCPPTypeName (PrimType CPTBool)                        = "CInt"
  hsCPPTypeName (PrimType (CPTClass c))                   = cppname c
  hsCPPTypeName _                                         = error "Error: a non-convertable type passed to hsCPPTypeName"

