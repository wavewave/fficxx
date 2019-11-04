-----------------------------------------------------------------------------
-- |
-- Module      : FFICXX.Generate.Type.Module
-- Copyright   : (c) 2011-2018 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module FFICXX.Generate.Type.Module where

import FFICXX.Generate.Type.Cabal (AddCInc,AddCSrc)
import FFICXX.Generate.Type.Class
import FFICXX.Generate.Type.PackageInterface (HeaderName(..),Namespace(..))

-- | C++ side
--   HPkg is generated C++ headers by fficxx, CPkg is original C++ headers
data ClassImportHeader = ClassImportHeader
                       { cihClass :: Class
                       , cihSelfHeader :: HeaderName
                       , cihNamespace :: [Namespace]
                       , cihSelfCpp :: String
                       , cihImportedClasses :: [Either TemplateClass Class]  -- ^ Dependencies TODO: clarify this.
                       , cihIncludedHPkgHeadersInH   :: [HeaderName]         -- TODO: Explain why we need to have these two
                       , cihIncludedHPkgHeadersInCPP :: [HeaderName]         --       separately.
                       , cihIncludedCPkgHeaders      :: [HeaderName]
                       } deriving (Show)


-- | Haskell side
data ClassModule = ClassModule
                   { cmModule :: String
                   , cmClass :: [Class]
                   , cmCIH :: [ClassImportHeader]
                   , cmImportedModulesHighNonSource :: [Either TemplateClass Class]  -- ^ imported modules that do not need source
                                                                         -- NOTE: source means the same cabal package.
                                                                         -- TODO: rename Source to something more clear.
                   , cmImportedModulesRaw           :: [Either TemplateClass Class]  -- ^ imported modules for raw types.
                   , cmImportedModulesHighSource    :: [Either TemplateClass Class]  -- ^ imported modules that need source
                   , cmImportedModulesForFFI        :: [Either TemplateClass Class]
                   , cmExtraImport :: [String]
                   } deriving (Show)

data TemplateClassModule = TCM { tcmModule :: String
                               , tcmTemplateClasses :: [TemplateClass]
                               , tcmTCIH :: [TemplateClassImportHeader]
                               } deriving (Show)


data TemplateClassImportHeader = TCIH { tcihTClass :: TemplateClass
                                      , tcihSelfHeader :: HeaderName
                                      } deriving (Show)

data TopLevelImportHeader = TopLevelImportHeader {
                              tihHeaderFileName    :: String
                            , tihClassDep          :: [ClassImportHeader]
                            , tihExtraClassDep     :: [Either TemplateClass Class]
                              -- ^ Extra class dependencies outside current package.
                              --   NOTE: we cannot fully construct ClassImportHeader for them.
                            , tihFuncs             :: [TopLevelFunction]
                            , tihNamespaces        :: [Namespace]
                            , tihExtraHeadersInH   :: [HeaderName]
                            , tihExtraHeadersInCPP :: [HeaderName]
                            } deriving (Show)

data PackageConfig = PkgConfig { pcfg_classModules :: [ClassModule]
                               , pcfg_classImportHeaders :: [ClassImportHeader]
                               , pcfg_topLevelImportHeader :: TopLevelImportHeader
                               , pcfg_templateClassModules :: [TemplateClassModule]
                               , pcfg_templateClassImportHeaders :: [TemplateClassImportHeader]
                               , pcfg_additional_c_incs :: [AddCInc]
                               , pcfg_additional_c_srcs :: [AddCSrc]
                               }
