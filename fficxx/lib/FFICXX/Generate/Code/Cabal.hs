-----------------------------------------------------------------------------
-- |
-- Module      : FFICXX.Generate.Code.Cabal
-- Copyright   : (c) 2011-2016 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module FFICXX.Generate.Code.Cabal where

import           FFICXX.Generate.Type.Module

cabalIndentation :: String 
cabalIndentation = replicate 23 ' ' 


-- | generate exposed module list in cabal file 
genExposedModules :: String -> ([ClassModule],[TemplateClassModule]) -> String
genExposedModules summarymod (cmods,tmods) = 
    let indentspace = cabalIndentation
        summarystrs = indentspace ++ summarymod 
        cmodstrs = map ((\x->indentspace++x).cmModule) cmods 
        rawType = map ((\x->indentspace++x++".RawType").cmModule) cmods
        ffi = map ((\x->indentspace++x++".FFI").cmModule) cmods
        interface= map ((\x->indentspace++x++".Interface").cmModule) cmods
        cast = map ((\x->indentspace++x++".Cast").cmModule) cmods 
        implementation = map ((\x->indentspace++x++".Implementation").cmModule) cmods
        template = map ((\x->indentspace++x++".Template").tcmModule) tmods
        th = map ((\x->indentspace++x++".TH").tcmModule) tmods        
    in  unlines ([summarystrs]++cmodstrs++rawType++ffi++interface++cast++implementation++template++th)

-- | generate other modules in cabal file 
genOtherModules :: [ClassModule] -> String 
genOtherModules _cmods = "" 
