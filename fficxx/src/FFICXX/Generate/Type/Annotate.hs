-----------------------------------------------------------------------------
-- |
-- Module      : FFICXX.Generate.Type.Annotate
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module FFICXX.Generate.Type.Annotate where

import qualified Data.Map as M

data PkgType = PkgModule | PkgClass | PkgMethod 
               deriving (Show,Eq,Ord)

type AnnotateMap = M.Map (PkgType,String) String



