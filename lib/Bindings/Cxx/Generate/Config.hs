module Bindings.Cxx.Generate.Config where

import Control.Applicative 
import Control.Monad.Identity 

import HEP.Parser.Config
import Text.Parsec 

data HROOTConfig = HROOTConfig { 
  hrootConfig_scriptBaseDir :: FilePath, 
  hrootConfig_workingDir :: FilePath, 
  hrootConfig_installBaseDir :: FilePath
} deriving Show

hrootconfigParse :: ParsecT String () Identity HROOTConfig 
hrootconfigParse = 
  oneGroupFieldInput "HROOTconf" $ 
    HROOTConfig <$> (oneFieldInput "scriptbase")
                <*> (oneFieldInput "workingdir")
                <*> (oneFieldInput "installbase")
