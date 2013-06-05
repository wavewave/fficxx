-----------------------------------------------------------------------------
-- |
-- Module      : FFICXX.Generate.Config
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------


module FFICXX.Generate.Config where

data FFICXXConfig = FFICXXConfig { 
  fficxxconfig_scriptBaseDir :: FilePath, 
  fficxxconfig_workingDir :: FilePath, 
  fficxxconfig_installBaseDir :: FilePath
} deriving Show

