{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import System.IO
import System.Directory
import System.FilePath ((</>))
import System.Console.CmdArgs

import Text.StringTemplate hiding (render)

import HROOT.Generate.ROOT
import HROOT.Generate.ROOTAnnotate

import HROOT.Generate.Generator.Driver
import HROOT.Generate.Generator.Command hiding (config)

import Text.Parsec
import Paths_HROOT_generate

import HROOT.Generate.Config

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
      mkCabalFile config templates h  

  putStrLn "header file generation"
  withFile (workingDir </> headerFileName) WriteMode $ 
    \h -> do 
      hPutStrLn h (mkDeclHeader templates root_all_classes)

  putStrLn "cpp file generation" 
  withFile (workingDir </> cppFileName) WriteMode $ 
    \h -> do 
      hPutStrLn h (mkDefMain templates root_all_classes)
      
  putStrLn "hsc file generation" 
  withFile (workingDir </> hscFileName) WriteMode $ 
    \h -> hPutStrLn h (mkFFIHsc templates root_all_classes) 
      
  putStrLn "Interface.hs file generation" 
  withFile (workingDir </> typeHsFileName) WriteMode $ 
    \h -> hPutStrLn h (mkInterfaceHs annotateMap templates moduleInterface root_all_classes )
    
  putStrLn "Implementation.hs file generation"
  withFile (workingDir </> hsFileName) WriteMode $ 
    \h -> hPutStrLn h (mkImplementationHs templates root_all_classes)
 
  copyFile (workingDir </> cabalFileName)  ( ibase </> cabalFileName ) 
  copyFile (workingDir </> headerFileName) ( csrcDir ibase </> headerFileName) 
  copyFile (workingDir </> cppFileName) ( csrcDir ibase </> cppFileName) 
  copyFile (workingDir </> hscFileName) ( srcDir ibase </> hscFileName) 
  copyFile (workingDir </> typeHsFileName) ( srcDir ibase </> typeHsFileName) 
  copyFile (workingDir </> hsFileName) ( srcDir ibase </> hsFileName)  
  
  return ()