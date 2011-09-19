{-# LANGUAGE QuasiQuotes #-}

module HROOT.Generate.ROOTAnnotate where

import HROOT.Generate.Type.Annotate 
import HROOT.Generate.QQ.Verbatim


import qualified Data.Map as M

annotateMap :: AnnotateMap 
annotateMap = M.fromList [tNamedAnn] 


tNamedAnn = ((HROOTClass,"TNamed"),[verbatim|
Class TNamed
reference : http://root.cern.ch
|])