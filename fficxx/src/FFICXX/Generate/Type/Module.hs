module FFICXX.Generate.Type.Module where

--
import FFICXX.Generate.Type.Cabal (AddCInc, AddCSrc)
import FFICXX.Generate.Type.Class (Class, TemplateClass, TopLevel)
import FFICXX.Runtime.CodeGen.Cxx (HeaderName (..), Namespace (..))

--
-- Import/Header
--
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

--
-- Submodule
--

data ClassSubmoduleType
  = CSTRawType
  | CSTInterface
  | CSTImplementation
  | CSTFFI
  | CSTCast
  deriving (Show)

data TemplateClassSubmoduleType
  = TCSTTH
  | TCSTTemplate
  deriving (Show)

-- | UClass = Unified Class, either template class or ordinary class
type UClass = Either TemplateClass Class

type UClassSubmodule =
  Either (TemplateClassSubmoduleType, TemplateClass) (ClassSubmoduleType, Class)

-- | Dependency cycle information. Currently just a string
--                  self,    former,   latter
type DepCycles = [[(String, ([String], [String]))]]

--
-- Module
--

data ClassModule = ClassModule
  { cmModule :: String,
    cmCIH :: ClassImportHeader,
    -- | imported submodules for Interface.hs
    cmImportedSubmodulesForInterface :: [UClassSubmodule],
    -- | imported submodules for FFI.hs
    cmImportedSubmodulesForFFI :: [UClassSubmodule],
    -- | imported submodules for Cast.hs
    cmImportedSubmodulesForCast,
    -- imported submodules for Implementation.hs
    cmImportedSubmodulesForImplementation ::
      [UClassSubmodule],
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

--
-- Package-level
--

data PackageConfig = PkgConfig
  { pcfg_classModules :: [ClassModule],
    pcfg_classImportHeaders :: [ClassImportHeader],
    pcfg_topLevelImportHeader :: TopLevelImportHeader,
    pcfg_templateClassModules :: [TemplateClassModule],
    pcfg_templateClassImportHeaders :: [TemplateClassImportHeader],
    pcfg_additional_c_incs :: [AddCInc],
    pcfg_additional_c_srcs :: [AddCSrc]
  }
