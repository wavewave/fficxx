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

--import HROOT.Generate.ROOT
--import HROOT.Generate.ROOTAnnotate
--import HROOT.Generate.ROOTModule

import HROOT.Generate.ROOTsmall
import HROOT.Generate.ROOTAnnotatesmall
import HROOT.Generate.ROOTModulesmall

import HROOT.Generate.Generator.Driver
import HROOT.Generate.Generator.Command hiding (config)

import Text.Parsec
import Paths_HROOT_generate

import HROOT.Generate.Config
import HROOT.Generate.Type.Class

import qualified Data.Map as M
import Data.Maybe

main :: IO () 
main = do 
  param <- cmdArgs mode
  putStrLn $ show param 
  commandLineProcess param 

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

  putStrLn "cabal file generation" 
  getHROOTVersion config
  withFile (workingDir </> cabalFileName) WriteMode $ 
    \h -> do 
      mkCabalFile config templates h root_all_modules 

  let cglobal = mkGlobal root_all_classes
   
  putStrLn "header file generation"
  mapM_ (writeDeclHeaders templates cglobal workingDir) root_all_classes_imports

  putStrLn "cpp file generation" 
  mapM_ (writeCppDef templates workingDir) root_all_classes_imports

  putStrLn "RawType.hs file generation" 
  mapM_ (writeRawTypeHs templates workingDir) root_all_modules 

  putStrLn "FFI.hsc file generation"
  mapM_ (writeFFIHsc templates workingDir) root_all_modules

  putStrLn "Interface.hs file generation" 
  mapM_ (writeInterfaceHs annotateMap templates workingDir) root_all_modules

  putStrLn "Implementation.hs file generation"
  mapM_ (writeImplementationHs annotateMap templates workingDir) root_all_modules

  putStrLn "Existential.hs generation"
  mapM_ (writeExistentialHs templates cglobal workingDir) root_all_modules

  putStrLn "module file generation" 
  mapM_ (writeModuleHs templates workingDir) root_all_modules

  
  copyFile (workingDir </> cabalFileName)  ( ibase </> cabalFileName ) 
  mapM_ (copyCppFiles workingDir (csrcDir ibase)) root_all_classes_imports
  mapM_ (copyModule workingDir (srcDir ibase)) root_all_modules 

  return ()

