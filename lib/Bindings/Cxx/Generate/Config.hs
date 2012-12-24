module Bindings.Cxx.Generate.Config where

import Control.Applicative 
import Control.Monad.Identity 

data FFICXXConfig = FFICXXConfig { 
  fficxxconfig_scriptBaseDir :: FilePath, 
  fficxxconfig_workingDir :: FilePath, 
  fficxxconfig_installBaseDir :: FilePath
} deriving Show

