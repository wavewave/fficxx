-----------------------------------------------------------------------------
-- |
-- Module      : FFICXX.Generate.Type.Module
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module FFICXX.Generate.Type.Module where

data Module = Module { module_name :: String         
                     , module_exports :: [String] 
                     } 

mkModuleExports :: Module -> String
mkModuleExports _mod = "" 

-- "\n  ( " ++ intercalate "\n  , " (module_exports mod) ++ ")"
