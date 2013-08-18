{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      : FFICXX.Generate.Builder 
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module FFICXX.Generate.Builder where 

import Data.Char (toUpper)
import Data.Monoid (mempty)
import System.FilePath ((</>))
import System.Directory (getCurrentDirectory)
import           Text.StringTemplate hiding (render)
--
import           FFICXX.Generate.Code.Cabal
import           FFICXX.Generate.Code.Cpp
import           FFICXX.Generate.Code.Dependency
import           FFICXX.Generate.Config
import           FFICXX.Generate.Code.Cpp
import           FFICXX.Generate.Code.Dependency
import           FFICXX.Generate.Config
import           FFICXX.Generate.Generator.ContentMaker 
import           FFICXX.Generate.Generator.Driver
import           FFICXX.Generate.Type.Annotate
import           FFICXX.Generate.Type.Class
import           FFICXX.Generate.Type.PackageInterface
import           FFICXX.Generate.Util
-- 
import qualified FFICXX.Paths_fficxx as F

-- | 
cabalTemplate :: String 
cabalTemplate = "Pkg.cabal"


cabalFileTemplate :: String -> TopLevelImportHeader -> [ClassModule] -> [(String, String)]
cabalFileTemplate pkgname tih classmodules =
  [ ("pkgname", pkgname
  , ("version",  "0.0") 
  , ("license", "" ) 
  , ("buildtype", "Simple")
  , ("deps", "" ) 
  , ("csrcFiles", genCsrcFiles (tih,classmodules))
  , ("includeFiles", genIncludeFiles pkgname classmodules) 
  , ("cppFiles", genCppFiles (tih,classmodules))
  , ("exposedModules", genExposedModules pkgname classmodules) 
  , ("otherModules", genOtherModules classmodules)
  , ("extralibdirs", "" )  
  , ("extraincludedirs", "" )  
  , ("extralib", ", snappy")
  , ("cabalIndentation", cabalIndentation)
  ]

-- | 
mkCabalFile :: FFICXXConfig
            -> STGroup String  
            -> Cabal
            -> (TopLevelImportHeader,[ClassModule])
            -> FilePath  
            -> IO () 
mkCabalFile config templates cabal (tih,classmodules) cabalfile = do 
  cpath <- getCurrentDirectory -- TODO: Remove this
  let str = renderTemplateGroup
              templates
              (cabalFileTemplate (cabal_pkgname cabal) tih classmodules)
              cabalTemplate 
  in writeFile cabalfile str


macrofy :: String -> String 
macrofy = map ((\x->if x=='-' then '_' else x) . toUpper)

simpleBuilder :: String -> (Cabal,[Class],[TopLevelFunction], [String]) ->  IO ()
simpleBuilder nspace (cabal,myclasses, toplevelfunctions, incHeaders) = do 
  putStrLn "generate " ++ pkgname
  cwd <- getCurrentDirectory 

  let cfg =  FFICXXConfig { fficxxconfig_scriptBaseDir = cwd 
                          , fficxxconfig_workingDir = cwd </> "working"
                          , fficxxconfig_installBaseDir = cwd </> (cabal_pkgname cabal)
                          } 
      workingDir = fficxxconfig_workingDir cfg
      installDir = fficxxconfig_installBaseDir cfg 
      pkgname = cabal_pkgname cabal
      (mods,cihs,tih) = mkAll_ClassModules_CIH_TIH 
                          (pkgname, (const ([NS nspace],incHeaders))) 
                          (myclasses, toplevelfunctions)
      hsbootlst = mkHSBOOTCandidateList mods 
      cglobal = mkGlobal myclasses 
      summarymodule = pkgname
      cabalFileName = pkgname ++ ".cabal"
  templateDir <- F.getDataDir >>= return . (</> "template")
  (templates :: STGroup String) <- directoryGroup templateDir 
  -- 
  notExistThenCreate workingDir
  notExistThenCreate installDir 
  notExistThenCreate (installDir </> "src") 
  notExistThenCreate (installDir </> "csrc")
  -- 
  putStrLn "cabal file generation" 
  mkCabalFile cfg templates cabal (tih,mods) (workingDir </> cabalFileName)
  -- 
  putStrLn "header file generation"
  let typmacro = TypMcro ("__"  ++ macrofy (cabal_pkgname cabal) ++ "__")  {- "__SNAPPY__" -}
  writeTypeDeclHeaders templates workingDir typmacro pkgname cihs
  mapM_ (writeDeclHeaders templates workingDir typmacro pkgname) cihs
  writeTopLevelFunctionHeaders templates workingDir typmacro pkgname tih
  -- 
  putStrLn "cpp file generation" 
  mapM_ (writeCppDef templates workingDir) cihs
  writeTopLevelFunctionCppDef templates workingDir typmacro pkgname tih
  -- 
  putStrLn "RawType.hs file generation" 
  mapM_ (writeRawTypeHs templates workingDir) mods 
  -- 
  putStrLn "FFI.hsc file generation"
  mapM_ (writeFFIHsc templates workingDir) mods
  -- 
  putStrLn "Interface.hs file generation" 
  mapM_ (writeInterfaceHs mempty templates workingDir) mods
  -- 
  putStrLn "Cast.hs file generation"
  mapM_ (writeCastHs templates workingDir) mods
  -- 
  putStrLn "Implementation.hs file generation"
  mapM_ (writeImplementationHs mempty templates workingDir) mods
  -- 
  putStrLn "hs-boot file generation" 
  mapM_ (writeInterfaceHSBOOT templates workingDir) hsbootlst  
  -- 
  putStrLn "module file generation" 
  mapM_ (writeModuleHs templates workingDir) mods
  -- 
  putStrLn "summary module generation generation"
  writePkgHs summarymodule templates workingDir mods tih
  -- 
  putStrLn "copying"
  copyFileWithMD5Check (workingDir </> cabalFileName)  (installDir </> cabalFileName) 
  -- copyPredefined templateDir (srcDir ibase) pkgname
  copyCppFiles workingDir (csrcDir installDir) pkgname (tih,cihs)
  mapM_ (copyModule workingDir (srcDir installDir) summarymodule) mods 



