-----------------------------------------------------------------------------
-- |
-- Module      : FFICXX.Generate.Type.Cabal
-- Copyright   : (c) 2011-2018 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module FFICXX.Generate.Type.Cabal where

data AddCInc = AddCInc FilePath String

data AddCSrc = AddCSrc FilePath String

newtype CabalName = CabalName { unCabalName :: String }
                  deriving (Show,Eq,Ord)

data Cabal = Cabal  { cabal_pkgname       :: CabalName
                    , cabal_version       :: String
                    , cabal_cheaderprefix :: String
                    , cabal_moduleprefix  :: String
                    , cabal_additional_c_incs :: [AddCInc]
                    , cabal_additional_c_srcs :: [AddCSrc]
                    , cabal_additional_pkgdeps :: [CabalName]
                    , cabal_license          :: Maybe String
                    , cabal_licensefile      :: Maybe String
                    , cabal_extraincludedirs :: [FilePath]
                    , cabal_extralibdirs     :: [FilePath]
                    , cabal_extrafiles       :: [FilePath]
                    , cabal_pkg_config_depends :: [String]
                    }
