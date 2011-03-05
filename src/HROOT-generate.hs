module Main where

import System.IO
import System.Directory
import System.Process
import System.FilePath ((</>))

import Text.StringTemplate
import Text.StringTemplate.Helpers

import CType
import Util
import Templates
import Function
import Class
import ROOT

hline = putStrLn "--------------------------------------------------------"

main :: IO () 
main = do 
  putStrLn "Automatic HROOT binding generation" 
  setCurrentDirectory scriptBaseDir
  
  templates <- directoryGroup templateDir 
  
  putStrLn $ mkDeclHeader templates classes

  hline
  
  putStrLn $ mkDefMain templates classes
  
  hline
    
  putStrLn $ ( mkDaughterDef . mkDaughterMap) classes 
                              
  
  