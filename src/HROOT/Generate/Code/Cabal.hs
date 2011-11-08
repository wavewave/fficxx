module HROOT.Generate.Code.Cabal where

import HROOT.Generate.Type.Class 

import Data.List

cabalIndentation = replicate 23 ' ' 

genExposedModules :: [ClassModule] -> String
genExposedModules cmods = 
  let indentspace = cabalIndentation
      cmodstrs = map ((\x->indentspace++"HROOT.Class."++x).cmModule) cmods 
  in  unlines cmodstrs  


genOtherModules cmods = 
  let indentspace = cabalIndentation 
      rawType = map ((\x->indentspace++"HROOT.Class."++x++".RawType").cmModule) cmods
      ffi = map ((\x->indentspace++"HROOT.Class."++x++".FFI").cmModule) cmods
      interface= map ((\x->indentspace++"HROOT.Class."++x++".Interface").cmModule) cmods
      cast = map ((\x->indentspace++"HROOT.Class."++x++".Cast").cmModule) cmods 
      implementation = map ((\x->indentspace++"HROOT.Class."++x++".Implementation").cmModule) cmods
      --   existential = map ((\x->indentspace++"HROOT.Class."++x++".Existential").cmModule) cmods 
  in  unlines (rawType++ffi++interface++cast++implementation {- ++existential -})
