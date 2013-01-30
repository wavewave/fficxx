module Bindings.Cxx.Generate.Type.Module where

data Module = Module { module_name :: String         
                     , module_exports :: [String] 
                     } 

mkModuleExports :: Module -> String
mkModuleExports _mod = "" 

-- "\n  ( " ++ intercalate "\n  , " (module_exports mod) ++ ")"
