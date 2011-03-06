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
  templates <- directoryGroup templateDir 
  
  putStrLn "header file generation"
  withFile (workingDir </> headerFileName) WriteMode $ 
    \h -> hPutStrLn h (mkDeclHeader templates root_all_classes)
  
  let dmap = mkDaughterMap root_concrete_classes  

  putStrLn "cpp file generation" 
  withFile (workingDir </> cppFileName) WriteMode $ 
    \h -> do 
      hPutStrLn h (mkDefMain templates root_all_classes)
      hPutStrLn h ( ( mkDaughterDef . mkDaughterMap) root_concrete_classes )
  
  putStrLn "hsc file generation" 
  withFile (workingDir </> hscFileName) WriteMode $ 
    \h -> hPutStrLn h (mkFFIClasses root_concrete_classes)
  
  putStrLn "hs file generation"
  withFile (workingDir </> hsFileName) WriteMode $ 
    \h -> do
      hPutStrLn h (mkRawClasses root_concrete_classes)
      hPutStrLn h (mkClassInstances dmap)
--  putStrLn $ mkClassDeclarations root_abstract_classes
