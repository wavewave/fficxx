module HROOT.Generate.Generator.Driver where

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

import HROOT.Generate.ROOT
import HROOT.Generate.ROOTModule

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

---- common function for daughter

mkDaughterDef :: ((Class,[Class]) -> String) -> DaughterMap -> String 
mkDaughterDef f m = 
  let lst = M.toList m 
      f' (x,xs) =  f (x,filter (not.isAbstractClass) xs) 
  in  concatMap f' lst 

---- Header and Cpp file

mkDeclHeader :: STGroup String -> [Class] -> String 
mkDeclHeader templates classes = 
  let declDefStr     = genAllCppHeaderTmplVirtual classes 
                       `connRet2`
                       genAllCppHeaderTmplNonVirtual classes 
      typeDeclStr    = genAllCppHeaderTmplType classes 
      dsmap           = mkDaughterSelfMap classes
      classDeclsStr  = mkDaughterDef genCppHeaderInstVirtual dsmap
                       `connRet2` 
                       genAllCppHeaderInstNonVirtual classes
      declBodyStr    = declDefStr 
                       `connRet2` 
                       typeDeclStr 
                       `connRet2` 
                       classDeclsStr 
  in  renderTemplateGroup 
        templates 
        [ ("declarationbody", declBodyStr ) ] 
        declarationTemplate

mkDefMain :: STGroup String -> [Class] -> String 
mkDefMain templates classes =
  let dsmap    = mkDaughterSelfMap classes
      cppBody = genAllCppDefTmplVirtual classes
                `connRet2`
                genAllCppDefTmplNonVirtual classes
                `connRet2`
                mkDaughterDef genCppDefInstVirtual dsmap
                `connRet2` 
                genAllCppDefInstNonVirtual classes

  in  renderTemplateGroup 
        templates 
        [ ("headerfilename", headerFileName ) 
        , ("cppbody"       , cppBody ) ] 
        definitionTemplate

mkFFIHsc :: STGroup String -> [Class] -> String 
mkFFIHsc templates classes = 
  renderTemplateGroup templates
                      [ ("headerFileName", headerFileName)
                      , ("hsFunctionBody", genAllHsFFI headerFileName classes) ]  
                      "FFI.hsc" 
                     
mkInterfaceHs :: AnnotateMap -> STGroup String -> Module -> [Class] -> String                      
mkInterfaceHs amap templates mod classes = 
  renderTemplateGroup templates [ ("ifaceHeader", ifaceHeaderStr) 
                                , ("ifaceBody", ifaceBodyStr)]  "Interface.hs" 
  where ifaceHeaderStr = runReader (genModuleDecl mod) amap
        ifaceBodyStr = 
          mkRawClasses (filter (not.isAbstractClass) classes)
          `connRet2`
          runReader (genAllHsFrontDecl classes) amap 
  
  
mkImplementationHs :: AnnotateMap -> STGroup String -> [Class] -> String
mkImplementationHs amap templates classes = 
  renderTemplateGroup templates 
                      [ ("implBody", implBodyStr ) ]
                      "Implementation.hs"
  where dmap = mkDaughterMap classes
        implBodyStr =  genAllHsFrontInstCastable classes 
                       `connRet2`
                       genAllHsFrontInstExist (filter (not.isAbstractClass) classes)
                       `connRet2`
                       genAllHsFrontInst classes dmap 
                       `connRet2`
                       runReader (genAllHsFrontInstNew classes) amap
                       `connRet2`
                       genAllHsFrontInstNonVirtual classes

-- Modules

genExposedModules :: [String] -> [String] -> String
genExposedModules emods cmods = 
  let indentspace = replicate 23 ' ' 
      emodstrs = map (indentspace ++) emods
      cmodstrs = map (\x -> indentspace ++ "HROOT.Class." ++ x) cmods 
  in  unlines (emodstrs ++ cmodstrs) 

genExportList :: String -> String 
genExportList modname =
  let cs = filter (\x->class_name x  == modname) root_all_classes
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

mkModuleFile :: HROOTConfig -> STGroup String -> String -> IO () 
mkModuleFile config templates modname = do 
  let modfilename = modname <.> "hs"
  withFile (hrootConfig_workingDir config </> modfilename) WriteMode $ 
    \h -> do 
      let str = renderTemplateGroup 
                  templates 
                  [ ("moduleName", modname) 
                  , ("exportList", genExportList modname) 
                  ]
                  moduleTemplate 
      hPutStrLn h str
  

-- | Generate HROOT.cabal file 

mkCabalFile :: HROOTConfig -> STGroup String -> Handle -> IO () 
mkCabalFile config templates h = do 
  version <- getHROOTVersion config

  let str = renderTemplateGroup 
              templates 
              [ ("version", version) 
              , ("exposedModules", genExposedModules exposedModules classModules) 
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
