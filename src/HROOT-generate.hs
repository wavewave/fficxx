{-# LANGUAGE ScopedTypeVariables #-}

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
import CppCode
import FFI 
import HsCode


import ROOT

import FileGeneration

hline = putStrLn "--------------------------------------------------------"

main :: IO () 
main = do 
  putStrLn "Automatic HROOT binding generation" 
  (templates :: STGroup String) <- directoryGroup templateDir 
  
  putStrLn "header file generation"
  withFile (workingDir </> headerFileName) WriteMode $ 
    \h -> do 
      hPutStrLn h (mkDeclHeader templates root_all_classes)
      
  let dmap = mkDaughterMap root_all_classes  

  putStrLn "cpp file generation" 
  withFile (workingDir </> cppFileName) WriteMode $ 
    \h -> do 
      hPutStrLn h (mkDefMain templates root_all_classes)
      hPutStrLn h ( ( mkDaughterDef . mkDaughterMap) root_all_classes )
      hPutStrLn h ( classesSelfDefs root_all_classes) 
      
      
  putStrLn "hsc file generation" 
  withFile (workingDir </> hscFileName) WriteMode $ 
    \h -> hPutStrLn h (mkFunctionHsc templates root_all_classes) 
      
  putStrLn "Type.hs file generation" 
  withFile (workingDir </> typeHsFileName) WriteMode $ 
    \h -> hPutStrLn h (mkTypeHs templates root_all_classes )
  
  
  putStrLn "Class.hs file generation"
  withFile (workingDir </> hsFileName) WriteMode $ 
    \h -> hPutStrLn h (mkClassHs templates root_all_classes)

  copyFile (workingDir </> headerFileName) ( csrcDir </> headerFileName) 
  copyFile (workingDir </> cppFileName) ( csrcDir </> cppFileName) 
  
  copyFile (workingDir </> hscFileName) ( srcDir </> hscFileName) 
  copyFile (workingDir </> typeHsFileName) ( srcDir </> typeHsFileName) 
  copyFile (workingDir </> hsFileName) ( srcDir </> hsFileName) 
  


--  putStrLn $  classesToHsDecls root_abstract_classes 

  return ()