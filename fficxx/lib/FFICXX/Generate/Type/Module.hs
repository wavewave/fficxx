-----------------------------------------------------------------------------
-- |
-- Module      : FFICXX.Generate.Type.Module
-- Copyright   : (c) 2011-2016 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module FFICXX.Generate.Type.Module where

import           FFICXX.Generate.Type.Class
import           FFICXX.Generate.Type.PackageInterface


newtype Namespace = NS { unNamespace :: String } deriving (Show)

data ClassImportHeader = ClassImportHeader
                       { cihClass :: Class
                       , cihSelfHeader :: HeaderName
                       , cihNamespace :: [Namespace]
                       , cihSelfCpp :: String
                       , cihIncludedHPkgHeadersInH :: [HeaderName]
                       , cihIncludedHPkgHeadersInCPP :: [HeaderName]
                       , cihIncludedCPkgHeaders :: [HeaderName]
                       } deriving (Show)

data ClassModule = ClassModule
                   { cmModule :: String
                   , cmClass :: [Class]
                   , cmCIH :: [ClassImportHeader]
                   , cmImportedModulesHighNonSource :: [String]
                   , cmImportedModulesRaw :: [String]
                   , cmImportedModulesHighSource :: [String]
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
                                                 } deriving (Show)

data PackageConfig = PkgConfig { pcfg_classModules :: [ClassModule]
                               , pcfg_classImportHeaders :: [ClassImportHeader]
                               , pcfg_topLevelImportHeader :: TopLevelImportHeader
                               , pcfg_templateClassModules :: [TemplateClassModule]
                               , pcfg_templateClassImportHeaders :: [TemplateClassImportHeader]
                               , pcfg_additional_c_includes :: [AddCInclude]
                               , pcfg_additional_c_srcs :: [AddCSrc]
                               }


