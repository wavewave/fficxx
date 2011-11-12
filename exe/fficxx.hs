{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Executable  : HROOT-generate
-- Copyright   : (c) 2011 Ian-Woo Kim
-- 
-- License     : GPL-3
-- Maintainer  : ianwookim@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Generate source code for HROOT  
--

module Main where

import System.IO
import System.Directory
import System.FilePath 
import System.Console.CmdArgs

import Text.StringTemplate hiding (render)

import Bindings.Cxx.Generate.ROOT
import Bindings.Cxx.Generate.ROOTAnnotate
import Bindings.Cxx.Generate.ROOTModule

-- import Bindings.Cxx.Generate.ROOTsmall
-- import Bindings.Cxx.Generate.ROOTAnnotatesmall
-- import Bindings.Cxx.Generate.ROOTModulesmall

import Bindings.Cxx.Generate.Generator.Driver
import Bindings.Cxx.Generate.Generator.Command hiding (config)

import Text.Parsec
import Paths_fficxx

import Bindings.Cxx.Generate.Config
import Bindings.Cxx.Generate.Type.Class
import Bindings.Cxx.Generate.Code.Dependency
import Bindings.Cxx.Generate.Generator.ContentMaker 

import qualified Data.Map as M
import Data.Maybe

main :: IO () 
main = do 
  param <- cmdArgs mode
  putStrLn $ show param 
  commandLineProcess param 
  -- putStrLn $ show $ mkModuleDepHigh tKey


commandLineProcess :: HROOT_Generate -> IO () 
commandLineProcess (Generate conf) = do 
  putStrLn "Automatic HROOT binding generation" 
  str <- readFile conf 
  let config = case (parse hrootconfigParse "" str) of 
                 Left msg -> error (show msg)
                 Right ans -> ans
  templateDir <- getDataDir >>= return . (</> "template")
  (templates :: STGroup String) <- directoryGroup templateDir 
  
  let workingDir = hrootConfig_workingDir config 
      ibase = hrootConfig_installBaseDir config
      cabalFileName = "HROOT.cabal"

      (root_all_modules,root_all_classes_imports) = mkAllClassModulesAndCIH root_all_classes 
  
  putStrLn "cabal file generation" 
  getHROOTVersion config
  withFile (workingDir </> cabalFileName) WriteMode $ 
    \h -> do 
      mkCabalFile config templates h root_all_modules 

  let cglobal = mkGlobal root_all_classes
   
  putStrLn "header file generation"
  writeTypeDeclHeaders templates cglobal workingDir root_all_classes_imports
  mapM_ (writeDeclHeaders templates cglobal workingDir) root_all_classes_imports

  putStrLn "cpp file generation" 
  mapM_ (writeCppDef templates workingDir) root_all_classes_imports

  putStrLn "RawType.hs file generation" 
  mapM_ (writeRawTypeHs templates workingDir) root_all_modules 

  putStrLn "FFI.hsc file generation"
  mapM_ (writeFFIHsc templates workingDir) root_all_modules

  putStrLn "Interface.hs file generation" 
  mapM_ (writeInterfaceHs annotateMap templates workingDir) root_all_modules

  putStrLn "Cast.hs file generation"
  mapM_ (writeCastHs templates workingDir) root_all_modules

  putStrLn "Implementation.hs file generation"
  mapM_ (writeImplementationHs annotateMap templates workingDir) root_all_modules

  {-
  putStrLn "Existential.hs generation"
  mapM_ (writeExistentialHs templates cglobal workingDir) root_all_modules
  -}
  
  putStrLn "module file generation" 
  mapM_ (writeModuleHs templates workingDir) root_all_modules

  putStrLn "HROOT.hs file generation"
  writeHROOTHs templates workingDir root_all_modules
  
  copyFile (workingDir </> cabalFileName)  ( ibase </> cabalFileName ) 
  copyPredefined templateDir (srcDir ibase)
  mapM_ (copyCppFiles workingDir (csrcDir ibase)) root_all_classes_imports
  mapM_ (copyModule workingDir (srcDir ibase)) root_all_modules 

  return ()

