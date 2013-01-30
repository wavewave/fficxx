module Bindings.Cxx.Generate.Config where

data FFICXXConfig = FFICXXConfig { 
  fficxxconfig_scriptBaseDir :: FilePath, 
  fficxxconfig_workingDir :: FilePath, 
  fficxxconfig_installBaseDir :: FilePath
} deriving Show

