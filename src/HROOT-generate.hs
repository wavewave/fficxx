module Main where

import System.IO
import System.Directory
import System.Process
import System.FilePath ((</>))

import Text.StringTemplate hiding (render)
import Text.StringTemplate.Helpers

import CType
import Util
import Templates
import Function
import Class
import ROOT
import FFI 

hline = putStrLn "--------------------------------------------------------"

main :: IO () 
main = do 
  putStrLn "Automatic HROOT binding generation" 
  setCurrentDirectory scriptBaseDir
  templates <- directoryGroup templateDir 
  putStrLn $ mkDeclHeader templates root_all_classes
  hline
  putStrLn $ mkDefMain templates root_all_classes
  hline
  putStrLn $ ( mkDaughterDef . mkDaughterMap) root_all_classes 
  hline
  putStrLn $ mkFFIClasses root_concrete_classes
  hline
  putStrLn $ mkHighLevelClasses root_concrete_classes