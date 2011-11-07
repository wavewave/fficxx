module HROOT.Generate.Generator.Driver where

import Control.Applicative
import Control.Monad.Trans.Reader

import Text.StringTemplate hiding (render)
import Text.StringTemplate.Helpers

import qualified Data.Map as M

import HROOT.Generate.Util

import HROOT.Generate.Type.Annotate
import HROOT.Generate.Type.Class
import HROOT.Generate.Type.Module 
import HROOT.Generate.Type.Method

import HROOT.Generate.Code.Cpp
import HROOT.Generate.Code.HsFFI 
import HROOT.Generate.Code.HsFrontEnd

import System.FilePath 
import System.Directory 

import HROOT.Generate.Config

import HEP.Util.File

-- import HROOT.Generate.ROOT
-- import HROOT.Generate.ROOTModule

import Distribution.Package
import Distribution.PackageDescription hiding (exposedModules)
import Distribution.PackageDescription.Parse
import Distribution.Verbosity
import Distribution.Version 

import Data.List 
import System.IO
-----
 
getHROOTVersion :: HROOTConfig -> IO String 
getHROOTVersion conf = do 
  let hrootgeneratecabal = hrootConfig_scriptBaseDir conf </> "HROOT-generate.cabal"
  gdescs <- readPackageDescription normal hrootgeneratecabal
  
  let vnums = versionBranch . pkgVersion . package . packageDescription $ gdescs 
  return $ intercalate "." (map show vnums)
--  putStrLn $ "version = " ++ show vnum

----- 

srcDir :: FilePath -> FilePath
srcDir installbasedir = installbasedir </> "src" -- </> "HROOT" </> "Class"

csrcDir :: FilePath -> FilePath
csrcDir installbasedir = installbasedir </> "csrc" 

moduleTemplate :: String 
moduleTemplate = "module.hs"

cabalTemplate :: String 
cabalTemplate = "HROOT.cabal"

declarationTemplate :: String
declarationTemplate = "HROOT.h"

declbodyTemplate :: String
declbodyTemplate    = "declbody.h"

funcdeclTemplate :: String
funcdeclTemplate    = "funcdecl.h" 

definitionTemplate :: String
definitionTemplate = "HROOT.cpp"

classDefTemplate :: String
classDefTemplate   = "classdef.cpp"

functionTemplate :: String
functionTemplate   = "function.cpp" 

funcbodyTemplate :: String
funcbodyTemplate   = "functionbody.cpp"

headerFileName :: String
headerFileName = "HROOT.h"

cppFileName :: String
cppFileName = "HROOT.cpp" 

hscFileName :: String
hscFileName = "FFI.hsc"

hsFileName :: String
hsFileName  = "Implementation.hs"

typeHsFileName :: String
typeHsFileName = "Interface.hs"

existHsFileName :: String 
existHsFileName = "Existential.hs"

rawtypeHsFileName :: String
rawtypeHsFileName = "RawType.hs"

ffiHscFileName :: String 
ffiHscFileName = "FFI.hsc"

interfaceHsFileName :: String
interfaceHsFileName = "Interface.hs"

implementationHsFileName :: String 
implementationHsFileName = "Implementation.hs"

existentialHsFileName :: String 
existentialHsFileName = "Existential.hs"

cabalIndentation = replicate 23 ' ' 


---- common function for daughter

mkGlobal :: [Class] -> ClassGlobal
mkGlobal = ClassGlobal <$> mkDaughterSelfMap <*> mkDaughterMap 


mkDaughterDef :: ((Class,[Class]) -> String) -> DaughterMap -> String 
mkDaughterDef f m = 
  let lst = M.toList m 
      f' (x,xs) =  f (x,filter (not.isAbstractClass) xs) 
  in  concatMap f' lst 

mkParentDef :: ((Class,Class)->String) -> Class -> String
mkParentDef f c = g (class_allparents c,c)
  where g (ps,c) = concatMap (\p -> f (p,c)) ps


---- Header and Cpp file

genAllCppHeaderInclude :: ClassImportHeader -> String 
genAllCppHeaderInclude header = 
  let strlst = map (\x->"#include \""++x++"\"") (cihIncludedCHeaders header) 
  in  intercalate "\n" strlst 

genModuleImportRawType :: [String] -> String 
genModuleImportRawType modstrs =
  let strlst = map (\x->"import HROOT.Class."++x++".RawType") modstrs
  in  intercalate "\n" strlst 

genModuleIncludeHeader :: [ClassImportHeader] -> String 
genModuleIncludeHeader headers =
  let strlst = map ((\x->"#include \""++x++"\"") . cihSelfHeader) headers 
  in  intercalate "\n" strlst 

-----

mkDeclHeader :: STGroup String -> ClassGlobal 
             -> ClassImportHeader 
             -> String 
mkDeclHeader templates cglobal header =
  let classes = [cihClass header]
      declHeaderStr = genAllCppHeaderInclude header
      declDefStr    = genAllCppHeaderTmplVirtual classes 
                      `connRet2`
                      genAllCppHeaderTmplNonVirtual classes 
      typeDeclStr   = genAllCppHeaderTmplType classes 
      dsmap         = cgDaughterSelfMap cglobal
      classDeclsStr = mkParentDef genCppHeaderInstVirtual (cihClass header) 
                      `connRet2`
                      genCppHeaderInstVirtual (cihClass header, cihClass header)
                      `connRet2` 
                      genAllCppHeaderInstNonVirtual classes
      declBodyStr   = declDefStr 
                      `connRet2` 
                      typeDeclStr 
                      `connRet2` 
                      classDeclsStr 
  in  renderTemplateGroup 
        templates 
        [ ("declarationheader", declHeaderStr ) 
        , ("declarationbody", declBodyStr ) ] 
        declarationTemplate

mkDefMain :: STGroup String -> ClassImportHeader -> String 
mkDefMain templates header =
  let classes = [cihClass header]
      headerStr = genAllCppHeaderInclude header ++ "\n#include \"" ++ cihSelfHeader header ++ "\"\n"
      cppBody = genAllCppDefTmplVirtual classes
                `connRet2`
                genAllCppDefTmplNonVirtual classes
                `connRet2`
                -- mkDaughterDef genCppDefInstVirtual dsmap
                mkParentDef genCppDefInstVirtual (cihClass header)
                `connRet`
                genCppHeaderInstVirtual (cihClass header, cihClass header)
                `connRet2` 
                genAllCppDefInstNonVirtual classes
  in  renderTemplateGroup 
        templates 
        [ ("header" , headerStr ) 
        , ("cppbody", cppBody ) ] 
        definitionTemplate

mkFFIHsc :: STGroup String -> ClassModule -> String 
mkFFIHsc templates mod = 
    renderTemplateGroup templates 
                        [ ("hsInclude", hsIncludeStr) 
                        , ("cppInclude", cppIncludeStr)
                        , ("hsFunctionBody", genAllHsFFI headers) ]
                        ffiHscFileName
  where classes = cmClass mod
        headers = cmCIH mod
        hsIncludeStr = genModuleImportRawType (cmImportedModules mod)
        cppIncludeStr = genModuleIncludeHeader headers

mkRawTypeHs :: STGroup String -> ClassModule -> String
mkRawTypeHs templates mod = 
    renderTemplateGroup templates [ ("rawtypeHeader", rawtypeHeaderStr) 
                                  , ("rawtypeBody", rawtypeBodyStr)] rawtypeHsFileName
  where rawtypeHeaderStr = "module HROOT.Class." ++ cmModule mod ++ ".RawType where\n"
        classes = cmClass mod
        rawtypeBodyStr = 
          mkRawClasses (filter (not.isAbstractClass) classes)

                     
mkInterfaceHs :: AnnotateMap -> STGroup String -> ClassModule -> String    
mkInterfaceHs amap templates mod  = 
    renderTemplateGroup templates [ ("ifaceHeader", ifaceHeaderStr) 
                                  , ("ifaceImport", ifaceImportStr)
                                  , ("ifaceBody", ifaceBodyStr)]  "Interface.hs" 
  where ifaceHeaderStr = "module HROOT.Class." ++ cmModule mod ++ ".Interface where\n" 
        classes = cmClass mod
        ifaceImportStr = genModuleImportRawType (cmImportedModules mod)
        -- runReader (genModuleDecl mod) amap
        ifaceBodyStr = 
          runReader (genAllHsFrontDecl classes) amap 
          `connRet2`
          runReader (genAllHsFrontUpcastClass (filter (not.isAbstractClass) classes)) amap  
  
mkImplementationHs :: AnnotateMap -> STGroup String -> ClassModule -> String
mkImplementationHs amap templates mod = 
    renderTemplateGroup templates 
                        [ ("implHeader", implHeaderStr) 
                        , ("implBody", implBodyStr ) ]
                        "Implementation.hs"
  where -- dmap = mkDaughterMap classes
        classes = cmClass mod
        implHeaderStr = "module HROOT.Class." ++ cmModule mod ++ ".Implementation where\n" 

        implBodyStr =  genAllHsFrontInstCastable classes 
                       `connRet2`
                       genAllHsFrontInstExistCommon (filter (not.isAbstractClass) classes)
                       `connRet2`
                       -- genAllHsFrontInstExistVirtual (filter (not.isAbstractClass) classes) dmap
                       -- `connRet2`
                       -- genAllHsFrontInst classes dmap 
                       -- `connRet2`
                       runReader (genAllHsFrontInstNew classes) amap
                       `connRet2`
                       genAllHsFrontInstNonVirtual classes


-- Modules

genIncludeFiles :: [ClassModule] -> String
genIncludeFiles cmods =
  let indent = cabalIndentation 
      selfheaders' = do 
        x <- cmods
        y <- cmCIH x
        return (cihSelfHeader y) 
      selfheaders = nub selfheaders'
      includeFileStrs = map (\x->indent++x) selfheaders
  in  unlines includeFileStrs

genCsrcFiles :: [ClassModule] -> String
genCsrcFiles cmods =
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
      includeFileStrsWithCsrc = map (\x->indent++"csrc"</>x) selfheaders
      cppFilesWithCsrc = map (\x->indent++"csrc"</>x) selfcpp
  in  unlines (includeFileStrsWithCsrc ++ cppFilesWithCsrc)

genCppFiles :: [ClassModule] -> String 
genCppFiles cmods = 
  let indent = cabalIndentation 
      selfcpp' = do 
        x <- cmods
        y <- cmCIH x
        return (cihSelfCpp y) 
      selfcpp = nub selfcpp'
      cppFileStrs = map (\x->indent++ "csrc" </> x) selfcpp
  in  unlines cppFileStrs 


genExposedModules :: [ClassModule] -> String
genExposedModules cmods = 
  let indentspace = cabalIndentation
      cmodstrs = map ((\x->indentspace++"HROOT.Class."++x).cmModule) cmods 
  in  unlines cmodstrs  


genOtherModules cmods = 
  let indentspace = cabalIndentation 
      rawType = map ((\x->indentspace++"HROOT.Class."++x++".RawType").cmModule) cmods
      ffi = map ((\x->indentspace++"HROOT.Class."++x++".FFI").cmModule) cmods
      interface= map ((\x->indentspace++"HROOT.Class."++x++".Interface").cmModule) cmods
      implementation = map ((\x->indentspace++"HROOT.Class."++x++".Implementation").cmModule) cmods
      existential = map ((\x->indentspace++"HROOT.Class."++x++".Existential").cmModule) cmods 
  in  unlines (rawType++ffi++interface++implementation++existential)


genExport :: Class -> String 
genExport c =
    let methodstr = if null . (filter isVirtualFunc) $ (class_funcs c) 
                      then ""
                      else "(..)"
    in if isAbstractClass c 
         then "    " ++ ('I' : class_name c) ++ methodstr 
         else "    " ++ class_name c ++ "(..)\n  , " 
                     ++ ('I' : class_name c) ++ methodstr
                     ++ "\n  , upcast" ++ class_name c

genExportList :: [Class] -> String 
genExportList = concatMap genExport 

--  let cs = filter (\x->class_name x  == modname) all_classes
--  in  if null cs 
--        then error $ "no such class :" ++ modname 
--        else let c = head cs 

genImportInModule :: [Class] -> String 
genImportInModule cs = 
  let genImportOneClass c = 
        let cname = class_name c 
        in      "import HROOT.Class." ++ cname ++ ".RawType\n"
             ++ "import HROOT.Class." ++ cname ++ ".Interface\n"
             ++ "import HROOT.Class." ++ cname ++ ".Implementation ()\n"
             ++ "import HROOT.Class." ++ cname ++ ".Existential\n"
  in  intercalate "\n" (map genImportOneClass cs)

mkModuleHs :: STGroup String -> ClassModule -> String 
mkModuleHs templates mod = 
    let str = renderTemplateGroup 
                templates 
                [ ("moduleName", cmModule mod) 
                , ("exportList", genExportList (cmClass mod)) 
                , ("importList", genImportInModule (cmClass mod))
                ]
                moduleTemplate 
    in str
  

-- | Generate HROOT.cabal file 

mkCabalFile :: HROOTConfig -> STGroup String -> Handle -> [ClassModule] -> IO () 
mkCabalFile config templates h classmodules = do 
  version <- getHROOTVersion config

  let str = renderTemplateGroup 
              templates 
              [ ("version", version) 
              , ("csrcFiles", genCsrcFiles classmodules)
              , ("includeFiles", genIncludeFiles classmodules) 
              , ("cppFiles", genCppFiles classmodules)
              , ("exposedModules", genExposedModules classmodules) 
              , ("otherModules", genOtherModules classmodules)
              , ("cabalIndentation", cabalIndentation)
              ]
              cabalTemplate 
  hPutStrLn h str


-- | Generate Existential.hs file 

----------

mkExistentialEach :: STGroup String -> Class -> [Class] -> String 
mkExistentialEach templates mother daughters =   
  let makeOneDaughterGADTBody daughter = render hsExistentialGADTBodyTmpl 
                                                [ ( "mother", class_name mother ) 
                                                , ( "daughter", class_name daughter ) ] 
      makeOneDaughterCastBody daughter = render hsExistentialCastBodyTmpl
                                                [ ( "mother", class_name mother ) 
                                                , ( "daughter", class_name daughter) ] 
      gadtBody = intercalate "\n" (map makeOneDaughterGADTBody daughters)
      castBody = intercalate "\n" (map makeOneDaughterCastBody daughters)
      str = renderTemplateGroup 
              templates 
              [ ( "mother" , class_name mother ) 
              , ( "GADTbody" , gadtBody ) 
              , ( "castbody" , castBody ) ]
              "ExistentialEach.hs" 
  in  str

----

mkExistentialHs :: STGroup String -> ClassGlobal -> ClassModule -> String
mkExistentialHs templates cglobal mod = 
  let classes = cmClass mod
      dsmap = cgDaughterSelfMap cglobal
      makeOneMother :: Class -> String 
      makeOneMother mother = 
        let daughters = case M.lookup mother dsmap of 
                             Nothing -> error "error in mkExistential"
                             Just lst -> lst
            str = mkExistentialEach templates mother daughters
        in  str 
      existEachBody = intercalateWith connRet makeOneMother classes
      hsfilestr = renderTemplateGroup 
                    templates 
                    [ ( "existEachBody" , existEachBody) ]
                  "Existential.hs" 
  in  hsfilestr

----

writeDeclHeaders :: STGroup String -> ClassGlobal -> FilePath -> ClassImportHeader
                 -> IO () 
writeDeclHeaders templates cglobal wdir header = do 
  let fn = wdir </> cihSelfHeader header
  withFile fn WriteMode $ \h -> do 
    hPutStrLn h (mkDeclHeader templates cglobal header)

writeCppDef :: STGroup String -> FilePath -> ClassImportHeader -> IO () 
writeCppDef templates wdir header = do 
  let fn = wdir </> cihSelfCpp header
  withFile fn WriteMode $ \h -> do 
    hPutStrLn h (mkDefMain templates header)

writeRawTypeHs :: STGroup String -> FilePath -> ClassModule -> IO ()
writeRawTypeHs templates wdir mod = do
  let fn = wdir </> "HROOT.Class." ++ cmModule mod <.> rawtypeHsFileName
  withFile fn WriteMode $ \h -> do 
    hPutStrLn h (mkRawTypeHs templates mod) 

writeFFIHsc :: STGroup String -> FilePath -> ClassModule -> IO ()
writeFFIHsc templates wdir mod = do 
  let fn = wdir </> "HROOT.Class." ++ cmModule mod <.> ffiHscFileName
  withFile fn WriteMode $ \h -> do 
    hPutStrLn h (mkFFIHsc templates mod)

writeInterfaceHs :: AnnotateMap -> STGroup String -> FilePath -> ClassModule 
                 -> IO ()
writeInterfaceHs amap templates wdir mod = do 
  let fn = wdir </> "HROOT.Class." ++ cmModule mod <.> interfaceHsFileName
  withFile fn WriteMode $ \h -> do 
    hPutStrLn h (mkInterfaceHs amap templates mod)

writeImplementationHs :: AnnotateMap -> STGroup String -> FilePath -> ClassModule 
                      -> IO ()
writeImplementationHs amap templates wdir mod = do 
  let fn = wdir </> "HROOT.Class." ++ cmModule mod <.> implementationHsFileName
  withFile fn WriteMode $ \h -> do 
    hPutStrLn h (mkImplementationHs amap templates mod)

writeExistentialHs :: STGroup String -> ClassGlobal -> FilePath -> ClassModule 
                   -> IO ()
writeExistentialHs templates cglobal wdir mod = do 
  let fn = wdir </> "HROOT.Class." ++ cmModule mod <.> existentialHsFileName
  withFile fn WriteMode $ \h -> do 
    hPutStrLn h (mkExistentialHs templates cglobal mod)


writeModuleHs :: STGroup String -> FilePath -> ClassModule -> IO () 
writeModuleHs templates wdir mod = do 
  let fn = wdir </> "HROOT.Class." ++ cmModule mod <.> "hs"
  withFile fn WriteMode $ \h -> do 
    hPutStrLn h (mkModuleHs templates mod)

{-
copyHeaderFiles :: FilePath -> FilePath -> IO () 
copyHeaderFiles = 
-}

copyCppFiles :: FilePath -> FilePath -> ClassImportHeader -> IO ()
copyCppFiles wdir ddir header = do 
  let hfile = cihSelfHeader header
      cppfile = cihSelfCpp header
  copyFile (wdir </> hfile) (ddir </> hfile) 
  copyFile (wdir </> cppfile) (ddir </> cppfile)

copyModule :: FilePath -> FilePath -> ClassModule -> IO ()
copyModule wdir ddir mod = do 
  let modbase = cmModule mod 
  let onefilecopy fname = do 
        let (fnamebody,fnameext) = splitExtension fname
            (mdir,mfile) = moduleDirFile fnamebody
            origfpath = wdir </> fname
            newfpath = ddir </> mdir </> mfile  

        b <- doesDirectoryExist (ddir</>mdir)
        if b then return () else createDirectory (ddir</>mdir)     
        copyFile origfpath newfpath 

  onefilecopy $ "HROOT.Class." ++ modbase ++ ".hs"
  onefilecopy $ "HROOT.Class." ++ modbase ++ ".RawType.hs"
  onefilecopy $ "HROOT.Class." ++ modbase ++ ".FFI.hsc"
  onefilecopy $ "HROOT.Class." ++ modbase ++ ".Interface.hs"
  onefilecopy $ "HROOT.Class." ++ modbase ++ ".Implementation.hs"
  onefilecopy $ "HROOT.Class." ++ modbase ++ ".Existential.hs"

  return ()
