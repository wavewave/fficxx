module Bindings.Cxx.Generate.Code.Cabal where

import Bindings.Cxx.Generate.Type.Class 

cabalIndentation :: String 
cabalIndentation = replicate 23 ' ' 


-- | generate exposed module list in cabal file 
genExposedModules :: [ClassModule] -> String
genExposedModules cmods = 
    let indentspace = cabalIndentation
        cmodstrs = map ((\x->indentspace++x).cmModule) cmods 
        rawType = map ((\x->indentspace++x++".RawType").cmModule) cmods
        ffi = map ((\x->indentspace++x++".FFI").cmModule) cmods
        interface= map ((\x->indentspace++x++".Interface").cmModule) cmods
        cast = map ((\x->indentspace++x++".Cast").cmModule) cmods 
        implementation = map ((\x->indentspace++x++".Implementation").cmModule) cmods
    in  unlines (cmodstrs++rawType++ffi++interface++cast++implementation)

-- | generate other modules in cabal file 
genOtherModules :: [ClassModule] -> String 
genOtherModules cmods = "" 
{-  let indentspace = cabalIndentation 
      rawType = map ((\x->indentspace++x++".RawType").cmModule) cmods
      ffi = map ((\x->indentspace++x++".FFI").cmModule) cmods
      interface= map ((\x->indentspace++x++".Interface").cmModule) cmods
      cast = map ((\x->indentspace++x++".Cast").cmModule) cmods 
      implementation = map ((\x->indentspace++x++".Implementation").cmModule) cmods
  in  unlines (rawType++ffi++interface++cast++implementation)
-}