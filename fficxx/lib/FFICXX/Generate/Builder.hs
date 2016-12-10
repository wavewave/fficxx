{-# LANGUAGE OverloadedStrings #-}
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
import           Data.Text                              (Text)
import qualified Data.Text                         as T
import qualified Data.Text.Lazy                    as TL
import           Data.Text.Template                     hiding (render)
import           System.FilePath ((</>), (<.>))
import           System.Directory (getCurrentDirectory)
import           System.Process (readProcess)
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
cabalTemplate :: Text
cabalTemplate =
  "Name:                $pkgname\n\
  \Version:     $version\n\
  \Synopsis:    $synopsis\n\
  \Description:         $description\n\
  \Homepage:       $homepage\n\
  \$licenseField\n\
  \$licenseFileField\n\
  \Author:              $author\n\
  \Maintainer:  $maintainer\n\
  \Category:       $category\n\
  \Tested-with:    GHC >= 7.6\n\
  \Build-Type:  $buildtype\n\
  \cabal-version:  >=1.10\n\
  \Extra-source-files:\n\
  \${cabalIndentation}CHANGES\n\
  \${cabalIndentation}Config.hs\n\
  \$csrcFiles\n\
  \\n\
  \$sourcerepository\n\
  \\n\
  \Library\n\
  \  default-language: Haskell2010\n\
  \  hs-source-dirs: src\n\
  \  ghc-options:  -Wall -funbox-strict-fields -fno-warn-unused-do-bind -fno-warn-orphans -fno-warn-unused-imports\n\
  \  ghc-prof-options: -caf-all -auto-all\n\
  \  cc-options: $ccOptions\n\
  \  Build-Depends:      base>4 && < 5, fficxx-runtime >= 0.2 $deps\n\
  \  Exposed-Modules:\n\
  \$exposedModules\n\
  \  Other-Modules:\n\
  \$otherModules\n\
  \  extra-lib-dirs: $extralibdirs\n\
  \  extra-libraries:    stdc++ $extraLibraries\n\
  \  Include-dirs:       csrc $extraincludedirs\n\
  \  Install-includes:\n\
  \$includeFiles\n\
  \  C-sources:\n\
  \$cppFiles\n"

-- |
mkCabalFile :: FFICXXConfig
            -> (Cabal, CabalAttr)
            -> String
            -> (TopLevelImportHeader,[ClassModule])
            -> [String] -- ^ extra libs
            -> FilePath
            -> IO ()
mkCabalFile config
            (cabal, cabalattr)
            summarymodule
            (tih,classmodules)
            extralibs
            cabalfile
            = do
  cpath <- getCurrentDirectory

  let txt = substitute cabalTemplate
              (context ([ ("licenseField", "license: " ++ license)
                          | Just license <- [cabalattr_license cabalattr] ] ++
                        [ ("licenseFileField", "license-file: " ++ licensefile)
                          | Just licensefile <- [cabalattr_licensefile cabalattr] ] ++
                        [ ("pkgname", cabal_pkgname cabal)
                        , ("version",  "0.0")
                        , ("buildtype", "Simple")
                        , ("synopsis", "")
                        , ("description", "")
                        , ("homepage","")
                        , ("author","")
                        , ("maintainer","")
                        , ("category","")
                        , ("sourcerepository","")
                        , ("ccOptions","")
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
                        ]))
  writeFile cabalfile (TL.unpack txt)


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
  -- templateDir <- F.getDataDir >>= return . (</> "template")
  -- (templates :: STGroup String) <- directoryGroup templateDir
  --
  notExistThenCreate workingDir
  notExistThenCreate installDir
  notExistThenCreate (installDir </> "src")
  notExistThenCreate (installDir </> "csrc")
  --
  putStrLn "cabal file generation"
  mkCabalFile cfg (cabal, cabalattr) summarymodule (tih,mods) extralibs (workingDir </> cabalFileName)
  --
  putStrLn "header file generation"
  let typmacro = TypMcro ("__"  ++ macrofy (cabal_pkgname cabal) ++ "__") 
  writeTypeDeclHeaders workingDir typmacro pkgname cihs
  mapM_ (writeDeclHeaders workingDir typmacro pkgname) cihs
  writeTopLevelFunctionHeaders workingDir typmacro pkgname tih
  --
  putStrLn "cpp file generation"
  mapM_ (writeCppDef workingDir) cihs
  writeTopLevelFunctionCppDef workingDir typmacro pkgname tih
  --
  putStrLn "RawType.hs file generation"
  mapM_ (writeRawTypeHs workingDir) mods
  --
  putStrLn "FFI.hsc file generation"
  mapM_ (writeFFIHsc workingDir) mods
  --
  putStrLn "Interface.hs file generation"
  mapM_ (writeInterfaceHs mempty workingDir) mods
  --
  putStrLn "Cast.hs file generation"
  mapM_ (writeCastHs workingDir) mods
  --
  putStrLn "Implementation.hs file generation"
  mapM_ (writeImplementationHs mempty workingDir) mods
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
