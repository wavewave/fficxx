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

import HROOT.Generate.Config

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
srcDir installbasedir = installbasedir </> "src" </> "HROOT" </> "Class"

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


cabalIndentation = replicate 23 ' ' 


---- common function for daughter

mkGlobal :: [Class] -> ClassGlobal
mkGlobal = ClassGlobal <$> mkDaughterSelfMap 


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
      dsmap         = cgDaughterMap cglobal
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

      -- classDeclsStr  = mkDaughterDef genCppHeaderInstVirtual dsmap
      --                 `connRet2` 


mkDefMain :: STGroup String -> ClassImportHeader -> String 
mkDefMain templates header =
  let -- dsmap    = mkDaughterSelfMap classes
      classes = [cihClass header]
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


{-  renderTemplateGroup templates
                      [ ("headerFileName", headerFileName)
                      , ("hsFunctionBody", genAllHsFFI headerFileName classes) ]  
                      "FFI.hsc" -} 

mkRawTypeHs :: STGroup String -> ClassModule -> String
mkRawTypeHs templates mod = 
  renderTemplateGroup templates [ ("rawtypeHeader", rawtypeHeaderStr) 
                                , ("rawtypeBody", rawtypeBodyStr)] rawtypeHsFileName
  where rawtypeHeaderStr = "module " ++ cmModule mod ++ " where\n"
        classes = cmClass mod
        rawtypeBodyStr = 
          mkRawClasses (filter (not.isAbstractClass) classes)
                     
mkInterfaceHs :: AnnotateMap -> STGroup String -> Module -> [Class] -> String    
mkInterfaceHs amap templates mod classes = 
  renderTemplateGroup templates [ ("ifaceHeader", ifaceHeaderStr) 
                                , ("ifaceBody", ifaceBodyStr)]  "Interface.hs" 
  where ifaceHeaderStr = runReader (genModuleDecl mod) amap
        ifaceBodyStr = 
          mkRawClasses (filter (not.isAbstractClass) classes)
          `connRet2`
          runReader (genAllHsFrontDecl classes) amap 
          `connRet2`
          runReader (genAllHsFrontUpcastClass (filter (not.isAbstractClass) classes)) amap  
  
mkImplementationHs :: AnnotateMap -> STGroup String -> [Class] -> String
mkImplementationHs amap templates classes = 
  renderTemplateGroup templates 
                      [ ("implBody", implBodyStr ) ]
                      "Implementation.hs"
  where dmap = mkDaughterMap classes
        implBodyStr =  genAllHsFrontInstCastable classes 
                       `connRet2`
                       genAllHsFrontInstExistCommon (filter (not.isAbstractClass) classes)
                       `connRet2`
                       genAllHsFrontInstExistVirtual (filter (not.isAbstractClass) classes) dmap
                       `connRet2`
                       genAllHsFrontInst classes dmap 
                       `connRet2`
                       runReader (genAllHsFrontInstNew classes) amap
                       `connRet2`
                       genAllHsFrontInstNonVirtual classes

-- Modules

genIncludeFiles :: [ClassModule] -> (String,String)
genIncludeFiles cmods =
  let indent = cabalIndentation 
      selfheaders' = do 
        x <- cmods
        y <- cmCIH x
        return (cihSelfHeader y) 
      selfheaders = nub selfheaders'
      includeFileStrs = map (\x->indent++x) selfheaders
      includeFileStrsWithCsrc = map (\x->indent++"csrc"</>x) selfheaders
  in  (unlines includeFileStrsWithCsrc, unlines includeFileStrs)

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
      cmodstrsInternal = map ((\x->indentspace++"HROOT.Class."++x++".Internal").cmModule) cmods
  in  unlines cmodstrsInternal


genExportList :: [Class] -> String -> String 
genExportList all_classes modname =
  let cs = filter (\x->class_name x  == modname) all_classes
  in  if null cs 
        then error $ "no such class :" ++ modname 
        else let c = head cs 
                 methodstr = if null . (filter isVirtualFunc) $ (class_funcs c) 
                               then ""
                               else "(..)"
             in if isAbstractClass c 
                  then "    " ++ ('I' : modname) ++ methodstr 
                  else "    " ++ modname ++ "(..)\n  , " 
                              ++ ('I' : modname) ++ methodstr
                              ++ "\n  , upcast" ++ modname

mkModuleFile :: HROOTConfig -> STGroup String -> [Class] -> String -> IO () 
mkModuleFile config templates all_classes modname = do 
  let modfilename = modname <.> "hs"
  withFile (hrootConfig_workingDir config </> modfilename) WriteMode $ 
    \h -> do 
      let str = renderTemplateGroup 
                  templates 
                  [ ("moduleName", modname) 
                  , ("exportList", genExportList all_classes modname) 
                  ]
                  moduleTemplate 
      hPutStrLn h str
  

-- | Generate HROOT.cabal file 

mkCabalFile :: HROOTConfig -> STGroup String -> Handle -> [ClassModule] -> IO () 
mkCabalFile config templates h classmodules = do 
  version <- getHROOTVersion config

  let str = renderTemplateGroup 
              templates 
              [ ("version", version) 
              , ("includeFiles", fst (genIncludeFiles classmodules))
              , ("includeFiles", snd (genIncludeFiles classmodules)) 
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

mkExistential :: STGroup String -> [Class] -> String
mkExistential templates classes = 
  let dsmap = mkDaughterSelfMap classes
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

writeDeclHeaders :: STGroup String -> ClassGlobal -> FilePath 
                 -> ClassImportHeader 
                 -> IO () 
writeDeclHeaders templates cglobal wdir header = do 
  let fn = wdir </> cihSelfHeader header
  withFile fn WriteMode $ \h -> do 
    hPutStrLn h (mkDeclHeader templates cglobal header)

writeCppDef :: STGroup String 
            -> FilePath 
            -> ClassImportHeader 
            -> IO () 
writeCppDef templates wdir header = do 
  let fn = wdir </> cihSelfCpp header
  withFile fn WriteMode $ \h -> do 
    hPutStrLn h (mkDefMain templates header)

writeRawTypeHs :: STGroup String
               -> FilePath 
               -> ClassModule
               -> IO ()
writeRawTypeHs templates wdir mod = do
  let fn = wdir </> cmModule mod <.> rawtypeHsFileName
  withFile fn WriteMode $ \h -> do 
    hPutStrLn h (mkRawTypeHs templates mod) 

writeFFIHsc :: STGroup String
            -> FilePath
            -> ClassModule
            -> IO ()
writeFFIHsc templates wdir mod = do 
  let fn = wdir </> cmModule mod <.> ffiHscFileName
  withFile fn WriteMode $ \h -> do 
    hPutStrLn h (mkFFIHsc templates mod)



{-
writeAllDeclHeaders :: STGroup String -> ClassGlobal -> FilePath 
                    -> [ClassImportHeader] 
                    -> IO () 
writeAllDeclHeaders templates cglobal wdir headers = 
  mapM_ (writeDeclHeaders templates cglobal wdir) headers -}

{-writeAllCppDef :: STGroup String
               -> FilePath 
               -> [ClassImportHeader] 
               -> IO () 
writeAllCppDef templates wdir headers = 
  mapM_ (writeCppDef templates wdir) headers -}
