{-# LANGUAGE DeriveGeneric #-}

module FFICXX.Generate.Type.Cabal where

import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
    defaultOptions,
    genericParseJSON,
    genericToJSON,
  )
import Data.Aeson.Types (fieldLabelModifier)
import Data.Text (Text)
import GHC.Generics (Generic)

data AddCInc = AddCInc FilePath String

data AddCSrc = AddCSrc FilePath String

-- TODO: change String to Text
newtype CabalName = CabalName {unCabalName :: String}
  deriving (Show, Eq, Ord)

data BuildType
  = Simple
  | -- | dependencies
    Custom [CabalName]

-- TODO: change String to Text
data Cabal = Cabal
  { cabal_pkgname :: CabalName,
    cabal_version :: String,
    cabal_cheaderprefix :: String,
    cabal_moduleprefix :: String,
    cabal_additional_c_incs :: [AddCInc],
    cabal_additional_c_srcs :: [AddCSrc],
    cabal_additional_pkgdeps :: [CabalName],
    cabal_license :: Maybe String,
    cabal_licensefile :: Maybe String,
    cabal_extraincludedirs :: [FilePath],
    cabal_extralibdirs :: [FilePath],
    cabal_extrafiles :: [FilePath],
    cabal_pkg_config_depends :: [String],
    cabal_buildType :: BuildType
  }

data GeneratedCabalInfo = GeneratedCabalInfo
  { gci_pkgname :: Text,
    gci_version :: Text,
    gci_synopsis :: Text,
    gci_description :: Text,
    gci_homepage :: Text,
    gci_license :: Text,
    gci_licenseFile :: Text,
    gci_author :: Text,
    gci_maintainer :: Text,
    gci_category :: Text,
    gci_buildtype :: Text,
    gci_extraFiles :: [Text],
    gci_csrcFiles :: [Text],
    gci_sourcerepository :: Text,
    gci_cxxOptions :: [Text],
    gci_pkgdeps :: [Text],
    gci_exposedModules :: [Text],
    gci_otherModules :: [Text],
    gci_extraLibDirs :: [Text],
    gci_extraLibraries :: [Text],
    gci_extraIncludeDirs :: [Text],
    gci_pkgconfigDepends :: [Text],
    gci_includeFiles :: [Text],
    gci_cppFiles :: [Text]
  }
  deriving (Show, Generic)

instance ToJSON GeneratedCabalInfo where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 4}

instance FromJSON GeneratedCabalInfo where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 4}
