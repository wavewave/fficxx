module Bindings.Cxx.Generate.Code.Cabal where

import Bindings.Cxx.Generate.Type.Class 

import Data.List

cabalIndentation = replicate 23 ' ' 

genExposedModules :: [ClassModule] -> String
genExposedModules cmods = 
  let indentspace = cabalIndentation
      cmodstrs = map ((\x->indentspace++"Pkg.Class."++x).cmModule) cmods 
  in  unlines cmodstrs  

genOtherModules :: [ClassModule] -> String 
genOtherModules cmods = 
  let indentspace = cabalIndentation 
      rawType = map ((\x->indentspace++"Pkg.Class."++x++".RawType").cmModule) cmods
      ffi = map ((\x->indentspace++"Pkg.Class."++x++".FFI").cmModule) cmods
      interface= map ((\x->indentspace++"Pkg.Class."++x++".Interface").cmModule) cmods
      cast = map ((\x->indentspace++"Pkg.Class."++x++".Cast").cmModule) cmods 
      implementation = map ((\x->indentspace++"Pkg.Class."++x++".Implementation").cmModule) cmods
      --   existential = map ((\x->indentspace++"Pkg.Class."++x++".Existential").cmModule) cmods 
  in  unlines (rawType++ffi++interface++cast++implementation {- ++existential -})
