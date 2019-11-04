{-# LANGUAGE DeriveGeneric #-}
module FFICXX.Generate.Type.Config where

import Data.Hashable (Hashable(..))
import Data.HashMap.Strict (HashMap)
import GHC.Generics (Generic)
--
import FFICXX.Generate.Type.PackageInterface (HeaderName(..),Namespace(..))

data ModuleUnit = MU_TopLevel 
                | MU_Class String
                deriving (Show,Eq,Generic)

instance Hashable ModuleUnit

data ModuleUnitImports =
  ModuleUnitImports {
    muimports_namespaces :: [Namespace]
  , muimports_headers    :: [HeaderName]
  }
  deriving (Show)

emptyModuleUnitImports = ModuleUnitImports [] []

newtype ModuleUnitMap = ModuleUnitMap { unModuleUnitMap :: HashMap ModuleUnit ModuleUnitImports }
