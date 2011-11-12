module Bindings.Cxx.Generate.Type.Annotate where

import qualified Data.Map as M

data HROOTType = HROOTModule | HROOTClass | HROOTMethod 
               deriving (Show,Eq,Ord)

type AnnotateMap = M.Map (HROOTType,String) String



