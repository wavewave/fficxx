{-# LANGUAGE DeriveGeneric #-}

module FFICXX.Generate.Type.Config where

import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable (..))
--
import FFICXX.Runtime.CodeGen.Cxx (HeaderName (..), Namespace (..))
import GHC.Generics (Generic)

data ModuleUnit
  = MU_TopLevel
  | MU_Class String
  deriving (Show, Eq, Generic)

instance Hashable ModuleUnit

data ModuleUnitImports = ModuleUnitImports
  { muimports_namespaces :: [Namespace],
    muimports_headers :: [HeaderName]
  }
  deriving (Show)

emptyModuleUnitImports = ModuleUnitImports [] []

newtype ModuleUnitMap = ModuleUnitMap {unModuleUnitMap :: HashMap ModuleUnit ModuleUnitImports}

modImports ::
  String ->
  [String] ->
  [HeaderName] ->
  (ModuleUnit, ModuleUnitImports)
modImports n ns hs =
  ( MU_Class n,
    ModuleUnitImports
      { muimports_namespaces = map NS ns,
        muimports_headers = hs
      }
  )
