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

data ClassImportHeader = ClassImportHeader
                       { cihClass :: Class
                       , cihSelfHeader :: HeaderName
                       , cihNamespace :: [Namespace]
                       , cihSelfCpp :: String
                       , cihIncludedHPkgHeadersInH :: [HeaderName]    -- TODO: Explain why we need to have these two
                       , cihIncludedHPkgHeadersInCPP :: [HeaderName]  --       separately.
                       , cihIncludedCPkgHeaders :: [HeaderName]
                       } deriving (Show)

data ClassModule = ClassModule
                   { cmModule :: String
                   , cmClass :: [Class]
                   , cmCIH :: [ClassImportHeader]
                   , cmImportedModulesHighNonSource :: [String]  -- ^ imported modules that do not need source
                   , cmImportedModulesRaw :: [String]            -- ^ imported modules for raw types.
                   , cmImportedModulesHighSource :: [String]     -- ^ imported modules that need source
                   , cmImportedModulesForFFI :: [String]
                   , cmExtraImport :: [String]
                   } deriving (Show)

data TemplateClassModule = TCM { tcmModule :: String
                               , tcmTemplateClasses :: [TemplateClass]
                               , tcmTCIH :: [TemplateClassImportHeader]
                               } deriving (Show)


data TemplateClassImportHeader = TCIH { tcihTClass :: TemplateClass
                                      , tcihSelfHeader :: HeaderName
                                      } deriving (Show)

data TopLevelImportHeader = TopLevelImportHeader { tihHeaderFileName :: String
                                                 , tihClassDep :: [ClassImportHeader]
                                                 , tihFuncs :: [TopLevelFunction]
                                                 , tihNamespaces :: [Namespace]
                                                 , tihExtraHeaders :: [HeaderName]
                                                 } deriving (Show)

data PackageConfig = PkgConfig { pcfg_classModules :: [ClassModule]
                               , pcfg_classImportHeaders :: [ClassImportHeader]
                               , pcfg_topLevelImportHeader :: TopLevelImportHeader
                               , pcfg_templateClassModules :: [TemplateClassModule]
                               , pcfg_templateClassImportHeaders :: [TemplateClassImportHeader]
                               , pcfg_additional_c_incs :: [AddCInc]
                               , pcfg_additional_c_srcs :: [AddCSrc]
                               }
