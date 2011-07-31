{-# LANGUAGE DeriveDataTypeable #-}

module HROOT.Generate.CommandType where

import System.Console.CmdArgs

data HROOT_Generate = Generate { config :: FilePath } 
                    deriving (Show,Data,Typeable)

generate :: HROOT_Generate
generate = Generate { config = "test.conf" } 

mode :: HROOT_Generate
mode = modes [generate] 

