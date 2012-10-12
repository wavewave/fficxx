module Bindings.Cxx.Generate.Code.Cabal where

import Bindings.Cxx.Generate.Type.Class 

import Data.List

cabalIndentation = replicate 23 ' ' 


-- | generate exposed module list in cabal file 
genExposedModules :: String          -- ^ prefix 
                  -> [ClassModule]
                  -> String
genExposedModules prefix cmods = 
  let indentspace = cabalIndentation
      cmodstrs = map ((\x->indentspace++prefix++"."++x).cmModule) cmods 
  in  unlines cmodstrs  

-- | generate other modules in cabal file 
genOtherModules :: String         -- ^ prefix
                -> [ClassModule] 
                -> String 
genOtherModules prefix cmods = 
  let indentspace = cabalIndentation 
      rawType = map ((\x->indentspace++prefix++"."++x++".RawType").cmModule) cmods
      ffi = map ((\x->indentspace++prefix++"."++x++".FFI").cmModule) cmods
      interface= map ((\x->indentspace++prefix++"."++x++".Interface").cmModule) cmods
      cast = map ((\x->indentspace++prefix++"."++x++".Cast").cmModule) cmods 
      implementation = map ((\x->indentspace++prefix++"."++x++".Implementation").cmModule) cmods
      --   existential = map ((\x->indentspace++"Pkg.Class."++x++".Existential").cmModule) cmods 
  in  unlines (rawType++ffi++interface++cast++implementation {- ++existential -})
