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

import           Control.Monad                           ( forM_, void, when )
import qualified Data.ByteString.Lazy.Char8        as L
import           Data.Char                               ( toUpper )
import           Data.Digest.Pure.MD5                    ( md5 )
import qualified Data.HashMap.Strict               as HM
import           Data.List                               ( intercalate )
import           Data.Monoid                             ( (<>), mempty )
import           Data.Text                               ( Text )
import           Language.Haskell.Exts.Pretty            ( prettyPrint )
import           System.FilePath                         ( (</>), (<.>), splitExtension )
import           System.Directory                        ( copyFile, doesDirectoryExist
                                                         , doesFileExist, getCurrentDirectory )
import           System.IO                               ( hPutStrLn, withFile, IOMode(..) )
import           System.Process                          ( readProcess, system )
--
import           FFICXX.Generate.Code.Cabal
import           FFICXX.Generate.Code.Cpp
import           FFICXX.Generate.Code.Dependency
import           FFICXX.Generate.Config
import           FFICXX.Generate.Generator.ContentMaker
-- import           FFICXX.Generate.Type.Annotate
import           FFICXX.Generate.Type.Class 
import           FFICXX.Generate.Type.Module  
import           FFICXX.Generate.Type.PackageInterface
import           FFICXX.Generate.Util
--

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
  \  Build-Depends:      base>4 && < 5, fficxx >= 0.2.999, fficxx-runtime >= 0.2, template-haskell$deps\n\
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
buildCabalFile :: (Cabal, CabalAttr)
            -> String
            -> PackageConfig -- (TopLevelImportHeader,[ClassModule],[TemplateClassModule])
            -> [String] -- ^ extra libs
            -> FilePath
            -> IO ()
buildCabalFile (cabal, cabalattr) summarymodule pkgconfig extralibs cabalfile = do
  let tih = pcfg_topLevelImportHeader pkgconfig
      classmodules = pcfg_classModules pkgconfig
      cih = pcfg_classImportHeaders pkgconfig
      tmods = pcfg_templateClassModules pkgconfig
      tcih = pcfg_templateClassImportHeaders pkgconfig
      txt = subst cabalTemplate
              (context ([ ("licenseField", "license: " <> license)
                          | Just license <- [cabalattr_license cabalattr] ] <>
                        [ ("licenseFileField", "license-file: " <> licensefile)
                          | Just licensefile <- [cabalattr_licensefile cabalattr] ] <>
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
                        , ("includeFiles", genIncludeFiles (cabal_pkgname cabal) (cih,tcih) )
                        , ("cppFiles", genCppFiles (tih,classmodules))
                        , ("exposedModules", genExposedModules summarymodule (classmodules,tmods))
                        , ("otherModules", genOtherModules classmodules)
                        , ("extralibdirs", intercalate ", " $ cabalattr_extralibdirs cabalattr)
                        , ("extraincludedirs", intercalate ", " $ cabalattr_extraincludedirs cabalattr)
                        , ("extraLibraries", concatMap (", " <>) extralibs)
                        , ("cabalIndentation", cabalIndentation)
                        ]))
  writeFile cabalfile txt


macrofy :: String -> String
macrofy = map ((\x->if x=='-' then '_' else x) . toUpper)

simpleBuilder :: String -> [(String,([Namespace],[HeaderName]))]
              -> (Cabal, CabalAttr, [Class], [TopLevelFunction], [(TemplateClass,HeaderName)])
              -> [String] -- ^ extra libs
              ->  IO ()
simpleBuilder summarymodule lst (cabal, cabalattr, classes, toplevelfunctions, templates) extralibs = do
  let pkgname = cabal_pkgname cabal
  putStrLn ("generating " <> pkgname)
  cwd <- getCurrentDirectory
  let cfg =  FFICXXConfig { fficxxconfig_scriptBaseDir = cwd
                          , fficxxconfig_workingDir = cwd </> "working"
                          , fficxxconfig_installBaseDir = cwd </> pkgname
                          }
      workingDir = fficxxconfig_workingDir cfg
      installDir = fficxxconfig_installBaseDir cfg

      pkgconfig@(PkgConfig mods cihs tih tcms _tcihs) =
        mkPackageConfig (pkgname, mkClassNSHeaderFromMap (HM.fromList lst)) (classes, toplevelfunctions,templates)
      hsbootlst = mkHSBOOTCandidateList mods
      cabalFileName = pkgname <.> "cabal" 
  --
  notExistThenCreate workingDir
  notExistThenCreate installDir
  notExistThenCreate (installDir </> "src")
  notExistThenCreate (installDir </> "csrc")
  --
  putStrLn "cabal file generation"
  buildCabalFile (cabal,cabalattr) summarymodule pkgconfig extralibs (workingDir</>cabalFileName)
  --
  putStrLn "header file generation"
  let typmacro = TypMcro ("__"  <> macrofy (cabal_pkgname cabal) <> "__")
      gen :: FilePath -> String -> IO ()
      gen file str =
        let path = workingDir </> file in withFile path WriteMode (flip hPutStrLn str)


  gen (pkgname <> "Type.h") (buildTypeDeclHeader typmacro (map cihClass cihs))
  mapM_ (\hdr -> gen (unHdrName (cihSelfHeader hdr)) (buildDeclHeader typmacro pkgname hdr)) cihs
  gen (tihHeaderFileName tih <.> "h") (buildTopLevelFunctionHeader typmacro pkgname tih)
  forM_ tcms $ \m ->
    let tcihs = tcmTCIH m
    in forM_ tcihs $ \tcih ->
         let t = tcihTClass tcih
             hdr = unHdrName (tcihSelfHeader tcih)
         in gen hdr (buildTemplateHeader typmacro t)
  --
  putStrLn "cpp file generation"
  mapM_ (\hdr -> gen (cihSelfCpp hdr) (buildDefMain hdr)) cihs
  gen (tihHeaderFileName tih <.> "cpp") (buildTopLevelFunctionCppDef tih)
  --
  putStrLn "RawType.hs file generation"
  mapM_ (\m -> gen (cmModule m <.> "RawType" <.> "hs") (prettyPrint (buildRawTypeHs m))) mods
  --
  putStrLn "FFI.hsc file generation"
  mapM_ (\m -> gen (cmModule m <.> "FFI" <.> "hsc") (prettyPrint (buildFFIHsc m))) mods
  --
  putStrLn "Interface.hs file generation"
  mapM_ (\m -> gen (cmModule m <.> "Interface" <.> "hs") (prettyPrint (buildInterfaceHs mempty m))) mods
  --
  putStrLn "Cast.hs file generation"
  mapM_ (\m -> gen (cmModule m <.> "Cast" <.> "hs") (prettyPrint (buildCastHs m))) mods
  --
  putStrLn "Implementation.hs file generation"
  mapM_ (\m -> gen (cmModule m <.> "Implementation" <.> "hs") (prettyPrint (buildImplementationHs mempty m))) mods
  --
  putStrLn "Template.hs file generation"
  mapM_ (\m -> gen (tcmModule m <.> "Template" <.> "hs") (prettyPrint (buildTemplateHs m))) tcms 
  -- 
  putStrLn "TH.hs file generation"
  mapM_ (\m -> gen (tcmModule m <.> "TH" <.> "hs") (prettyPrint (buildTHHs m))) tcms 


  -- 
  putStrLn "hs-boot file generation"
  mapM_ (\m -> gen (m <.> "Interface" <.> "hs-boot") (prettyPrint (buildInterfaceHSBOOT m))) hsbootlst
  --


  
  putStrLn "module file generation"
  mapM_ (\m -> gen (cmModule m <.> "hs") (prettyPrint (buildModuleHs m))) mods
  --
  putStrLn "summary module generation generation"
  gen (summarymodule <.> "hs") (buildPkgHs summarymodule mods tih)
  --
  putStrLn "copying"
  touch (workingDir </> "LICENSE")
  copyFileWithMD5Check (workingDir </> cabalFileName)  (installDir </> cabalFileName)
  copyFileWithMD5Check (workingDir </> "LICENSE") (installDir </> "LICENSE")

  copyCppFiles workingDir (csrcDir installDir) pkgname pkgconfig
  mapM_ (copyModule workingDir (srcDir installDir)) mods
  mapM_ (copyTemplateModule workingDir (srcDir installDir)) tcms  
  moduleFileCopy workingDir (srcDir installDir) $ summarymodule <.> "hs"


-- | some dirty hack. later, we will do it with more proper approcah.

touch :: FilePath -> IO ()
touch fp = void (readProcess "touch" [fp] "")


notExistThenCreate :: FilePath -> IO () 
notExistThenCreate dir = do 
    b <- doesDirectoryExist dir
    if b then return () else system ("mkdir -p " <> dir) >> return ()


copyFileWithMD5Check :: FilePath -> FilePath -> IO () 
copyFileWithMD5Check src tgt = do
  b <- doesFileExist tgt 
  if b 
    then do 
      srcmd5 <- md5 <$> L.readFile src  
      tgtmd5 <- md5 <$> L.readFile tgt 
      if srcmd5 == tgtmd5 then return () else copyFile src tgt 
    else copyFile src tgt  


copyCppFiles :: FilePath -> FilePath -> String -> PackageConfig -> IO ()
copyCppFiles wdir ddir cprefix (PkgConfig _ cihs tih _ tcihs) = do 
  let thfile = cprefix <> "Type.h"
      tlhfile = tihHeaderFileName tih <.> "h"
      tlcppfile = tihHeaderFileName tih <.> "cpp"
  copyFileWithMD5Check (wdir </> thfile) (ddir </> thfile) 
  doesFileExist (wdir </> tlhfile) 
    >>= flip when (copyFileWithMD5Check (wdir </> tlhfile) (ddir </> tlhfile))
  doesFileExist (wdir </> tlcppfile) 
    >>= flip when (copyFileWithMD5Check (wdir </> tlcppfile) (ddir </> tlcppfile))
  forM_ cihs $ \header-> do 
    let hfile = unHdrName (cihSelfHeader header)
        cppfile = cihSelfCpp header
    copyFileWithMD5Check (wdir </> hfile) (ddir </> hfile) 
    copyFileWithMD5Check (wdir </> cppfile) (ddir </> cppfile)

  forM_ tcihs $ \header-> do 
    let hfile = unHdrName (tcihSelfHeader header)
    copyFileWithMD5Check (wdir </> hfile) (ddir </> hfile) 


moduleFileCopy :: FilePath -> FilePath -> FilePath -> IO ()
moduleFileCopy wdir ddir fname = do 
  let (fnamebody,fnameext) = splitExtension fname
      (mdir,mfile) = moduleDirFile fnamebody
      origfpath = wdir </> fname
      (mfile',_mext') = splitExtension mfile
      newfpath = ddir </> mdir </> mfile' <> fnameext   
  b <- doesFileExist origfpath 
  when b $ do 
    notExistThenCreate (ddir </> mdir) 
    copyFileWithMD5Check origfpath newfpath 


copyModule :: FilePath -> FilePath -> ClassModule -> IO ()
copyModule wdir ddir m = do 
  let modbase = cmModule m 

  moduleFileCopy wdir ddir $ modbase <> ".hs"
  moduleFileCopy wdir ddir $ modbase <> ".RawType.hs"
  moduleFileCopy wdir ddir $ modbase <> ".FFI.hsc"
  moduleFileCopy wdir ddir $ modbase <> ".Interface.hs"
  moduleFileCopy wdir ddir $ modbase <> ".Cast.hs"
  moduleFileCopy wdir ddir $ modbase <> ".Implementation.hs"
  moduleFileCopy wdir ddir $ modbase <> ".Interface.hs-boot"
  return ()

copyTemplateModule :: FilePath -> FilePath -> TemplateClassModule -> IO ()
copyTemplateModule wdir ddir m = do 
  let modbase = tcmModule m 
  moduleFileCopy wdir ddir $ modbase <> ".Template.hs"
  moduleFileCopy wdir ddir $ modbase <> ".TH.hs"
  return ()

