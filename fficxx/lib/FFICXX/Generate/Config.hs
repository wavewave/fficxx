-----------------------------------------------------------------------------
-- |
-- Module      : FFICXX.Generate.Config
-- Copyright   : (c) 2011-2019 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------


module FFICXX.Generate.Config where

import FFICXX.Generate.Type.Cabal            ( Cabal )
import FFICXX.Generate.Type.Class            ( Class, TemplateClass, TopLevelFunction )
import FFICXX.Generate.Type.Config           ( ModuleUnitMap(..) )
import FFICXX.Generate.Type.PackageInterface ( HeaderName )


data FFICXXConfig = FFICXXConfig { 
  fficxxconfig_scriptBaseDir :: FilePath, 
  fficxxconfig_workingDir :: FilePath, 
  fficxxconfig_installBaseDir :: FilePath
} deriving Show

data SimpleBuilderConfig =
  SimpleBuilderConfig {
    sbcTopModule  :: String
  , sbcModUnitMap :: ModuleUnitMap
  , sbcCabal      :: Cabal
  , sbcClasses    :: [Class]
  , sbcTopLevels  :: [TopLevelFunction]
  , sbcTemplates  :: [(TemplateClass,HeaderName)]
  , sbcExtraLibs  :: [String]
  , sbcExtraDeps  :: [(String,[String])]
  }

