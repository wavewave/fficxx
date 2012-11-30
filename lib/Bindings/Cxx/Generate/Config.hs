module Bindings.Cxx.Generate.Config where

import Control.Applicative 
import Control.Monad.Identity 

-- import HEP.Parser.Config
-- import Text.Parsec 

data FFICXXConfig = FFICXXConfig { 
  fficxxconfig_scriptBaseDir :: FilePath, 
  fficxxconfig_workingDir :: FilePath, 
  fficxxconfig_installBaseDir :: FilePath
} deriving Show

{-
fficxxconfigParse :: ParsecT String () Identity FFICXXConfig 
fficxxconfigParse = 
  oneGroupFieldInput "fficxxconf" $ 
    FFICXXConfig <$> (oneFieldInput "scriptbase")
                <*> (oneFieldInput "workingdir")
                <*> (oneFieldInput "installbase")
-}