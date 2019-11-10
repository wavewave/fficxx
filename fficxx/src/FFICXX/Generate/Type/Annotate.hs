module FFICXX.Generate.Type.Annotate where

import qualified Data.Map as M

data PkgType = PkgModule | PkgClass | PkgMethod 
               deriving (Show,Eq,Ord)

type AnnotateMap = M.Map (PkgType,String) String



