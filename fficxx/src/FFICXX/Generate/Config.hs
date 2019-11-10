module FFICXX.Generate.Config where

import FFICXX.Generate.Type.Cabal            ( Cabal )
import FFICXX.Generate.Type.Class            ( Class, TemplateClass, TopLevelFunction )
import FFICXX.Generate.Type.Config           ( ModuleUnitMap(..) )
import FFICXX.Generate.Type.Module           ( TemplateClassImportHeader )
import FFICXX.Generate.Type.PackageInterface ( HeaderName )


data FFICXXConfig = FFICXXConfig {
    fficxxconfig_workingDir     :: FilePath
  , fficxxconfig_installBaseDir :: FilePath
  , fficxxconfig_staticFileDir  :: FilePath
  } deriving Show

data SimpleBuilderConfig =
  SimpleBuilderConfig {
    sbcTopModule     :: String
  , sbcModUnitMap    :: ModuleUnitMap
  , sbcCabal         :: Cabal
  , sbcClasses       :: [Class]
  , sbcTopLevels     :: [TopLevelFunction]
  , sbcTemplates     :: [TemplateClassImportHeader]
  , sbcExtraLibs     :: [String]
  , sbcExtraDeps     :: [(String,[String])]
  , sbcStaticFiles   :: [String]
  }
