module FFICXX.Generate.Type.Module where

--
import FFICXX.Generate.Type.Cabal (AddCInc, AddCSrc)
import FFICXX.Generate.Type.Class (Class, TemplateClass, TopLevel)
import FFICXX.Runtime.CodeGen.Cxx (HeaderName (..), Namespace (..))

-- | C++ side
--   HPkg is generated C++ headers by fficxx, CPkg is original C++ headers
data ClassImportHeader = ClassImportHeader
  { cihClass :: Class,
    -- | fficxx-side main header
    cihSelfHeader :: HeaderName,
    cihNamespace :: [Namespace],
    cihSelfCpp :: String,
    -- | Dependencies TODO: clarify this.
    cihImportedClasses :: [Either TemplateClass Class],
    cihIncludedHPkgHeadersInH :: [HeaderName], -- TODO: Explain why we need to have these two
    cihIncludedHPkgHeadersInCPP :: [HeaderName], --       separately.

    -- | C++-side headers
    cihIncludedCPkgHeaders :: [HeaderName]
  }
  deriving (Show)

-------------------------
-- Haskell side module --
-------------------------

data ClassModule = ClassModule
  { cmModule :: String,
    cmCIH :: ClassImportHeader,
    -- | imported modules external to the current package unit.
    cmImportedModulesExternal :: [Either TemplateClass Class],
    -- | imported modules for raw types.
    cmImportedModulesRaw :: [Either TemplateClass Class],
    -- | imported modules in the current package-in-place
    cmImportedModulesInplace :: [Either TemplateClass Class],
    cmImportedModulesForFFI :: [Either TemplateClass Class],
    cmExtraImport :: [String]
  }
  deriving (Show)

data TemplateClassModule = TCM
  { tcmModule :: String,
    tcmTCIH :: TemplateClassImportHeader
  }
  deriving (Show)

data TemplateClassImportHeader = TCIH
  { tcihTClass :: TemplateClass,
    -- | C++-side headers
    tcihCxxHeaders :: [HeaderName]
  }
  deriving (Show)

data TopLevelImportHeader = TopLevelImportHeader
  { tihHeaderFileName :: String,
    tihClassDep :: [ClassImportHeader],
    -- | Extra class dependencies outside current package.
    --   NOTE: we cannot fully construct ClassImportHeader for them.
    tihExtraClassDep :: [Either TemplateClass Class],
    tihFuncs :: [TopLevel],
    tihNamespaces :: [Namespace],
    tihExtraHeadersInH :: [HeaderName],
    tihExtraHeadersInCPP :: [HeaderName]
  }
  deriving (Show)

data PackageConfig = PkgConfig
  { pcfg_classModules :: [ClassModule],
    pcfg_classImportHeaders :: [ClassImportHeader],
    pcfg_topLevelImportHeader :: TopLevelImportHeader,
    pcfg_templateClassModules :: [TemplateClassModule],
    pcfg_templateClassImportHeaders :: [TemplateClassImportHeader],
    pcfg_additional_c_incs :: [AddCInc],
    pcfg_additional_c_srcs :: [AddCSrc]
  }
