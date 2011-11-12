module Bindings.Cxx.Generate.Type.Module where

import Data.List 

data Module = Module { module_name :: String         
                     , module_exports :: [String] 
                     } 

mkModuleExports :: Module -> String
mkModuleExports mod = "" 

-- "\n  ( " ++ intercalate "\n  , " (module_exports mod) ++ ")"
