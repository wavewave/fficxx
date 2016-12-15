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
import           Data.Monoid (mempty)
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
mkCabalFile :: (Cabal, CabalAttr)
            -> String
            -> (TopLevelImportHeader,[ClassModule])
            -> [String] -- ^ extra libs
            -> FilePath
            -> IO ()
mkCabalFile (cabal, cabalattr)
            summarymodule
            (tih,classmodules)
            extralibs
            cabalfile
            = do
  -- cpath <- getCurrentDirectory

  let txt = subst cabalTemplate
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
  writeFile cabalfile txt


macrofy :: String -> String
macrofy = map ((\x->if x=='-' then '_' else x) . toUpper)

simpleBuilder :: String -> [(String,([Namespace],[HeaderName]))]
              -> (Cabal, CabalAttr, [Class], [TopLevelFunction], [TemplateClass])
              -> [String] -- ^ extra libs
              ->  IO ()
simpleBuilder summarymodule lst (cabal, cabalattr, classes, toplevelfunctions, templates) extralibs = do
  let pkgname = cabal_pkgname cabal
  putStrLn ("generating " ++ pkgname)
  cwd <- getCurrentDirectory
  let cfg =  FFICXXConfig { fficxxconfig_scriptBaseDir = cwd
                          , fficxxconfig_workingDir = cwd </> "working"
                          , fficxxconfig_installBaseDir = cwd </> pkgname
                          }
      workingDir = fficxxconfig_workingDir cfg
      installDir = fficxxconfig_installBaseDir cfg

      (mods,cihs,tih,tcms) = mkAll_CM_CIH_TIH_TCM
                            (pkgname, mkClassNSHeaderFromMap (HM.fromList lst))
                            (classes, toplevelfunctions,templates)
      hsbootlst = mkHSBOOTCandidateList mods
      -- cglobal = mkGlobal classes
      cabalFileName = pkgname <.> "cabal" 
  --
  notExistThenCreate workingDir
  notExistThenCreate installDir
  notExistThenCreate (installDir </> "src")
  notExistThenCreate (installDir </> "csrc")
  --
  putStrLn "cabal file generation"
  mkCabalFile (cabal, cabalattr) summarymodule (tih,mods) extralibs (workingDir </> cabalFileName)
  --
  putStrLn "header file generation"
  let typmacro = TypMcro ("__"  ++ macrofy (cabal_pkgname cabal) ++ "__")
      gen :: FilePath -> String -> IO ()
      gen file str =
        let path = workingDir </> file in withFile path WriteMode (flip hPutStrLn str)


  gen (pkgname ++ "Type.h") (mkTypeDeclHeader typmacro (map cihClass cihs))
  mapM_ (\hdr -> gen (unHdrName (cihSelfHeader hdr)) (mkDeclHeader typmacro pkgname hdr)) cihs
  gen (tihHeaderFileName tih <.> "h") (mkTopLevelFunctionHeader typmacro pkgname tih)
  --
  putStrLn "cpp file generation"
  mapM_ (\hdr -> gen (cihSelfCpp hdr) (mkDefMain hdr)) cihs
  gen (tihHeaderFileName tih <.> "cpp") (mkTopLevelFunctionCppDef tih)
  --
  putStrLn "RawType.hs file generation"
  mapM_ (\m -> gen (cmModule m <.> "RawType" <.> "hs") (prettyPrint (mkRawTypeHs m))) mods
  --
  putStrLn "FFI.hsc file generation"
  mapM_ (\m -> gen (cmModule m <.> "FFI" <.> "hsc") (prettyPrint (mkFFIHsc m))) mods
  --
  putStrLn "Interface.hs file generation"
  mapM_ (\m -> gen (cmModule m <.> "Interface" <.> "hs") (prettyPrint (mkInterfaceHs mempty m))) mods
  --
  putStrLn "Cast.hs file generation"
  mapM_ (\m -> gen (cmModule m <.> "Cast" <.> "hs") (prettyPrint (mkCastHs m))) mods
  --
  putStrLn "Implementation.hs file generation"
  mapM_ (\m -> gen (cmModule m <.> "Implementation" <.> "hs") (prettyPrint (mkImplementationHs mempty m))) mods
  --
  putStrLn "Template.hs file generation"
  mapM_ (\m -> gen (tcmModule m <.> "Template" <.> "hs") (prettyPrint (mkTemplateHs m))) tcms 


  -- 
  putStrLn "hs-boot file generation"
  mapM_ (\m -> gen (m <.> "Interface" <.> "hs-boot") (prettyPrint (mkInterfaceHSBOOT m))) hsbootlst
  --


  
  putStrLn "module file generation"
  mapM_ (\m -> gen (cmModule m <.> "hs") (prettyPrint (mkModuleHs m))) mods
  --
  putStrLn "summary module generation generation"
  gen (summarymodule <.> "hs") (mkPkgHs summarymodule mods tih)
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
touch fp = void (readProcess "touch" [fp] "")


notExistThenCreate :: FilePath -> IO () 
notExistThenCreate dir = do 
    b <- doesDirectoryExist dir
    if b then return () else system ("mkdir -p " ++ dir) >> return ()


{- 
-- | now only create directory
copyPredefined :: FilePath -> FilePath -> String -> IO () 
copyPredefined _tdir _ddir _prefix = do 
    return () 
    -- notExistThenCreate (ddir </> prefix)
    -- copyFile (tdir </> "TypeCast.hs" ) (ddir </> prefix </> "TypeCast.hs") 
-}

copyFileWithMD5Check :: FilePath -> FilePath -> IO () 
copyFileWithMD5Check src tgt = do
  b <- doesFileExist tgt 
  if b 
    then do 
      srcmd5 <- md5 <$> L.readFile src  
      tgtmd5 <- md5 <$> L.readFile tgt 
      if srcmd5 == tgtmd5 then return () else copyFile src tgt 
    else copyFile src tgt  


copyCppFiles :: FilePath -> FilePath -> String -> (TopLevelImportHeader,[ClassImportHeader]) -> IO ()
copyCppFiles wdir ddir cprefix (tih,cihs) = do 
  let thfile = cprefix ++ "Type.h"
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

copyModule :: FilePath -> FilePath -> String -> ClassModule -> IO ()
copyModule wdir ddir summarymod m = do 
  let modbase = cmModule m 
  let onefilecopy fname = do 
        let (fnamebody,fnameext) = splitExtension fname
            (mdir,mfile) = moduleDirFile fnamebody
            origfpath = wdir </> fname
            (mfile',_mext') = splitExtension mfile
            newfpath = ddir </> mdir </> mfile' ++ fnameext   
        b <- doesFileExist origfpath 
        when b $ do 
          notExistThenCreate (ddir </> mdir) 
          copyFileWithMD5Check origfpath newfpath 

  onefilecopy $ modbase ++ ".hs"
  onefilecopy $ modbase ++ ".RawType.hs"
  onefilecopy $ modbase ++ ".FFI.hsc"
  onefilecopy $ modbase ++ ".Interface.hs"
  onefilecopy $ modbase ++ ".Cast.hs"
  onefilecopy $ modbase ++ ".Implementation.hs"
  onefilecopy $ modbase ++ ".Interface.hs-boot"
  onefilecopy $ summarymod <.> "hs"
  return ()
