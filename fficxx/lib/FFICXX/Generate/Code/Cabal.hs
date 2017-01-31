{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      : FFICXX.Generate.Code.Cabal
-- Copyright   : (c) 2011-2016 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module FFICXX.Generate.Code.Cabal where

import           Data.List                   ( intercalate, nub )
import           Data.Monoid                 ( (<>) )
import           Data.Text                   ( Text )
import           System.FilePath             ( (<.>), (</>) )
--
import           FFICXX.Generate.Type.Class
import           FFICXX.Generate.Type.Module
import           FFICXX.Generate.Type.PackageInterface
import           FFICXX.Generate.Util


cabalIndentation :: String 
cabalIndentation = replicate 23 ' ' 


genIncludeFiles :: String        -- ^ package name 
                -> ([ClassImportHeader],[TemplateClassImportHeader])
                -> String
genIncludeFiles pkgname (cih,tcih) =
  let indent = cabalIndentation 
      selfheaders = map cihSelfHeader cih <> map tcihSelfHeader tcih
      -- selfheaders = nub selfheaders'
      includeFileStrs = map ((indent<>).unHdrName) selfheaders
  in  unlines ((indent<>pkgname<>"Type.h") : includeFileStrs)

genCsrcFiles :: (TopLevelImportHeader,[ClassModule]) -> String
genCsrcFiles (tih,cmods) =
  let indent = cabalIndentation 
      selfheaders' = do 
        x <- cmods
        y <- cmCIH x
        return (cihSelfHeader y) 
      selfheaders = nub selfheaders'
      selfcpp' = do 
        x <- cmods
        y <- cmCIH x 
        return (cihSelfCpp y)
      selfcpp = nub selfcpp' 
      tlh = tihHeaderFileName tih <.> "h"
      tlcpp = tihHeaderFileName tih <.> "cpp"
      includeFileStrsWithCsrc = map (\x->indent<>"csrc"</> x) 
                                 (if (null.tihFuncs) tih then map unHdrName selfheaders else tlh:(map unHdrName selfheaders))
      cppFilesWithCsrc = map (\x->indent<>"csrc"</>x) 
                           (if (null.tihFuncs) tih then selfcpp else tlcpp:selfcpp)
      
  in  unlines (includeFileStrsWithCsrc <> cppFilesWithCsrc)

genCppFiles :: (TopLevelImportHeader,[ClassModule]) -> String 
genCppFiles (tih,cmods) = 
  let indent = cabalIndentation 
      selfcpp' = do 
        x <- cmods
        y <- cmCIH x
        return (cihSelfCpp y) 
      selfcpp = nub selfcpp'
      tlcpp = tihHeaderFileName tih <.> "cpp"
      cppFileStrs = map (\x->indent<> "csrc" </> x) 
                      (if (null.tihFuncs) tih then selfcpp else tlcpp:selfcpp)
  in  unlines cppFileStrs 



-- | generate exposed module list in cabal file 
genExposedModules :: String -> ([ClassModule],[TemplateClassModule]) -> String
genExposedModules summarymod (cmods,tmods) = 
    let indentspace = cabalIndentation
        summarystrs = indentspace <> summarymod 
        cmodstrs = map ((\x->indentspace<>x).cmModule) cmods 
        rawType = map ((\x->indentspace<>x<>".RawType").cmModule) cmods
        ffi = map ((\x->indentspace<>x<>".FFI").cmModule) cmods
        interface= map ((\x->indentspace<>x<>".Interface").cmModule) cmods
        cast = map ((\x->indentspace<>x<>".Cast").cmModule) cmods 
        implementation = map ((\x->indentspace<>x<>".Implementation").cmModule) cmods
        template = map ((\x->indentspace<>x<>".Template").tcmModule) tmods
        th = map ((\x->indentspace<>x<>".TH").tcmModule) tmods        
    in  unlines ([summarystrs]<>cmodstrs<>rawType<>ffi<>interface<>cast<>implementation<>template<>th)

-- | generate other modules in cabal file 
genOtherModules :: [ClassModule] -> String 
genOtherModules _cmods = "" 


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
  \  Build-Depends:      base>4 && < 5, fficxx >= 0.3, fficxx-runtime >= 0.3, template-haskell$deps\n\
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
                        , ("ccOptions","-std=c++14")
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

