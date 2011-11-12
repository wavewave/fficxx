{-# LANGUAGE DeriveDataTypeable #-}

module Bindings.Cxx.Generate.Generator.Command where

import System.Console.CmdArgs

data HROOT_Generate = Generate { config :: FilePath } 
                    deriving (Show,Data,Typeable)

generate :: HROOT_Generate
generate = Generate { config = "HROOT.conf" } 

mode :: HROOT_Generate
mode = modes [generate] 

