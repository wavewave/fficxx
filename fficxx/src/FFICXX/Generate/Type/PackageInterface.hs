{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FFICXX.Generate.Type.PackageInterface where 

import           Data.Hashable
import qualified Data.HashMap.Strict as HM
import           Data.String

newtype PackageName = PkgName String  deriving (Hashable, Show, Eq, Ord)
newtype ClassName = ClsName String deriving (Hashable, Show, Eq, Ord)

newtype HeaderName = HdrName { unHdrName :: String }
                   deriving (Hashable, Show, Eq, Ord)


instance IsString HeaderName where
  fromString = HdrName

newtype Namespace = NS { unNamespace :: String } deriving (Show)

type PackageInterface = HM.HashMap (PackageName, ClassName) HeaderName 

newtype TypeMacro = TypMcro { unTypMcro :: String } 
                  deriving (Show,Eq,Ord)
