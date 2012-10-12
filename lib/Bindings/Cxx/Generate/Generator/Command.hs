{-# LANGUAGE DeriveDataTypeable #-}

module Bindings.Cxx.Generate.Generator.Command where

import System.Console.CmdArgs

data FFICXX = Generate { config :: FilePath } 
            deriving (Show,Data,Typeable)

generate :: FFICXX
generate = Generate { config = "Pkg.conf" } 

mode :: FFICXX
mode = modes [generate] 

