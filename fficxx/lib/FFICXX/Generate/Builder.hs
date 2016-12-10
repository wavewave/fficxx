{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      : FFICXX.Generate.Builder
-- Copyright   : (c) 2011-2016 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module FFICXX.Generate.Builder where

import           Data.Char (toUpper)
import qualified Data.HashMap.Strict as HM
import           Data.List (intercalate)
import           Data.Monoid (mempty)
import           System.FilePath ((</>), (<.>))
import           System.Directory (getCurrentDirectory)
import           System.Process (readProcess)
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
import           FFICXX.Generate.Type.Class ( Cabal(..)
                                            , CabalAttr(..)
                                            , Class
                                            , ClassModule
                                            , Namespace(NS)
                                            , TopLevelFunction
                                            , TopLevelImportHeader
                                            )
import           FFICXX.Generate.Type.PackageInterface
import           FFICXX.Generate.Util
--
import qualified FFICXX.Paths_fficxx as F

-- |
cabalTemplate :: String
cabalTemplate = "Pkg.cabal"


-- |
mkCabalFile :: FFICXXConfig
            -> STGroup String
            -> (Cabal, CabalAttr)
            -> String
            -> (TopLevelImportHeader,[ClassModule])
            -> [String] -- ^ extra libs
            -> FilePath
            -> IO ()
mkCabalFile config
            templates
            (cabal, cabalattr)
            summarymodule
            (tih,classmodules)
            extralibs
            cabalfile
            = do
  cpath <- getCurrentDirectory

  let str = renderTemplateGroup
              templates
              ( [ ("licenseField", "license: " ++ license)
                  | Just license <- [cabalattr_license cabalattr] ] ++
                [ ("licenseFileField", "license-file: " ++ licensefile)
                  | Just licensefile <- [cabalattr_licensefile cabalattr] ] ++
                [ ("pkgname", cabal_pkgname cabal)
                , ("version",  "0.0")
                , ("buildtype", "Simple")
                , ("deps", "")
                , ("csrcFiles", genCsrcFiles (tih,classmodules))
                , ("includeFiles", genIncludeFiles (cabal_pkgname cabal) classmodules)
                , ("cppFiles", genCppFiles (tih,classmodules))
                , ("exposedModules", genExposedModules summarymodule classmodules)
                , ("otherModules", genOtherModules classmodules)
                , ("extralibdirs", intercalate ", " $ cabalattr_extralibdirs cabalattr)
                , ("extraincludedirs", intercalate ", " $ cabalattr_extraincludedirs cabalattr)
                , ("extraLibraries", concatMap (", " ++) extralibs)
                , ("cabalIndentation", cabalIndentation)
                ]
              )
              cabalTemplate
  writeFile cabalfile str


macrofy :: String -> String
macrofy = map ((\x->if x=='-' then '_' else x) . toUpper)

simpleBuilder :: String -> [(String,([Namespace],[HeaderName]))]
              -> (Cabal, CabalAttr, [Class], [TopLevelFunction])
              -> [String] -- ^ extra libs
              ->  IO ()
simpleBuilder summarymodule m (cabal, cabalattr, myclasses, toplevelfunctions) extralibs = do
  let pkgname = cabal_pkgname cabal
  putStrLn ("generating " ++ pkgname)
  cwd <- getCurrentDirectory
  let cfg =  FFICXXConfig { fficxxconfig_scriptBaseDir = cwd
                          , fficxxconfig_workingDir = cwd </> "working"
                          , fficxxconfig_installBaseDir = cwd </> pkgname
                          }
      workingDir = fficxxconfig_workingDir cfg
      installDir = fficxxconfig_installBaseDir cfg

      (mods,cihs,tih) = mkAll_ClassModules_CIH_TIH
                          (pkgname, mkClassNSHeaderFromMap (HM.fromList m))
                          (myclasses, toplevelfunctions)
      hsbootlst = mkHSBOOTCandidateList mods
      cglobal = mkGlobal myclasses
      cabalFileName = pkgname <.> "cabal" 
  templateDir <- F.getDataDir >>= return . (</> "template")
  (templates :: STGroup String) <- directoryGroup templateDir
  --
  notExistThenCreate workingDir
  notExistThenCreate installDir
  notExistThenCreate (installDir </> "src")
  notExistThenCreate (installDir </> "csrc")
  --
  putStrLn "cabal file generation"
  mkCabalFile cfg templates (cabal, cabalattr) summarymodule (tih,mods) extralibs (workingDir </> cabalFileName)
  --
  putStrLn "header file generation"
  let typmacro = TypMcro ("__"  ++ macrofy (cabal_pkgname cabal) ++ "__") 
  writeTypeDeclHeaders workingDir typmacro pkgname cihs
  mapM_ (writeDeclHeaders workingDir typmacro pkgname) cihs
  writeTopLevelFunctionHeaders workingDir typmacro pkgname tih
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
  mapM_ (writeInterfaceHSBOOT workingDir) hsbootlst
  --
  putStrLn "module file generation"
  mapM_ (writeModuleHs workingDir) mods
  --
  putStrLn "summary module generation generation"
  writePkgHs summarymodule workingDir mods tih
  --
  putStrLn "copying"
  touch (workingDir </> "LICENSE")
  copyFileWithMD5Check (workingDir </> cabalFileName)  (installDir </> cabalFileName)
  copyFileWithMD5Check (workingDir </> "LICENSE") (installDir </> "LICENSE")
  -- copyPredefined templateDir (srcDir ibase) pkgname

  copyCppFiles workingDir (csrcDir installDir) pkgname (tih,cihs)
  mapM_ (copyModule workingDir (srcDir installDir) summarymodule) mods


-- | some dirty hack. later, we will do it with more proper approcah.

touch :: FilePath -> IO ()
touch fp = do
    readProcess "touch" [fp] ""
    return ()
