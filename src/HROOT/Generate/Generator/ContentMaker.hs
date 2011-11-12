module HROOT.Generate.Generator.ContentMaker where 

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
import System.IO

import HROOT.Generate.Config
import HROOT.Generate.Code.Cabal 

-- import HEP.Util.File

import Distribution.Package
import Distribution.PackageDescription hiding (exposedModules)
import Distribution.PackageDescription.Parse
import Distribution.Verbosity
import Distribution.Version 

import Data.List 
import Data.Maybe

 
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

typeDeclHeaderFileName :: String
typeDeclHeaderFileName = "HROOTType.h"

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

castHsFileName :: String
castHsFileName = "Cast.hs"

implementationHsFileName :: String 
implementationHsFileName = "Implementation.hs"

existentialHsFileName :: String 
existentialHsFileName = "Existential.hs"


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

mkTypeDeclHeader :: STGroup String -> ClassGlobal 
             -> [Class]
             -> String 
mkTypeDeclHeader templates cglobal classes =
  let typeDeclBodyStr   = genAllCppHeaderTmplType classes 
  in  renderTemplateGroup 
        templates 
        [ ("typeDeclBody", typeDeclBodyStr ) ] 
        typeDeclHeaderFileName

mkDeclHeader :: STGroup String -> ClassGlobal 
             -> ClassImportHeader 
             -> String 
mkDeclHeader templates cglobal header =
  let classes = [cihClass header]
      aclass = cihClass header
      declHeaderStr = intercalateWith connRet (\x->"#include \""++x++"\"") $
                        cihIncludedHROOTHeaders header
                      -- genAllCppHeaderInclude header
      declDefStr    = genAllCppHeaderTmplVirtual classes 
                      `connRet2`
                      genAllCppHeaderTmplNonVirtual classes 
                      `connRet2`   
                      genAllCppDefTmplVirtual classes
                      `connRet2`
                       genAllCppDefTmplNonVirtual classes
      -- typeDeclStr   = genAllCppHeaderTmplType classes 
      dsmap         = cgDaughterSelfMap cglobal
      classDeclsStr = if class_name aclass /= "Deletable"
                        then mkParentDef genCppHeaderInstVirtual aclass 
                             `connRet2`
                             genCppHeaderInstVirtual (aclass, aclass)
                             `connRet2` 
                             genAllCppHeaderInstNonVirtual classes
                        else "" 
      declBodyStr   = declDefStr 
                      `connRet2` 
                      -- typeDeclStr 
                      -- `connRet2` 
                      classDeclsStr 
  in  renderTemplateGroup 
        templates 
        [ ("declarationheader", declHeaderStr ) 
        , ("declarationbody", declBodyStr ) ] 
        declarationTemplate

mkDefMain :: STGroup String -> ClassImportHeader -> String 
mkDefMain templates header =
  let classes = [cihClass header]
      headerStr = genAllCppHeaderInclude header ++ "\n#include \"" ++ (cihSelfHeader header) ++ "\"" 
      aclass = cihClass header
      cppBody = -- mkDaughterDef genCppDefInstVirtual dsmap
                mkParentDef genCppDefInstVirtual (cihClass header)
                `connRet` 
                if isAbstractClass aclass 
                  then "" 
                  else genCppDefInstVirtual (aclass, aclass)
                `connRet`
                genAllCppDefInstNonVirtual classes
  in  renderTemplateGroup 
        templates 
        [ ("header" , headerStr ) 
        , ("cppbody", cppBody ) 
        , ("modname", class_name (cihClass header)) ] 
        definitionTemplate




mkFFIHsc :: STGroup String -> ClassModule -> String 
mkFFIHsc templates mod = 
    renderTemplateGroup templates 
                        [ ("ffiHeader", ffiHeaderStr)
                        , ("ffiImport", ffiImportStr)
                        -- , ("hsInclude", hsIncludeStr) 
                        , ("cppInclude", cppIncludeStr)
                        , ("hsFunctionBody", genAllHsFFI headers) ]
                        ffiHscFileName
  where mname = cmModule mod
        classes = cmClass mod
        headers = cmCIH mod
        ffiHeaderStr = "module HROOT.Class." ++ mname ++ ".FFI where\n"
        ffiImportStr = "import HROOT.Class." ++ mname ++ ".RawType\n"
                       ++ genImportInFFI mod
        --  hsIncludeStr = genModuleImportRawType (cmImportedModulesRaw mod)
        cppIncludeStr = genModuleIncludeHeader headers

                     


mkRawTypeHs :: STGroup String -> ClassModule -> String
mkRawTypeHs templates mod = 
    renderTemplateGroup templates [ ("rawtypeHeader", rawtypeHeaderStr) 
                                  , ("rawtypeBody", rawtypeBodyStr)] rawtypeHsFileName
  where rawtypeHeaderStr = "module HROOT.Class." ++ cmModule mod ++ ".RawType where\n"
        classes = cmClass mod
        rawtypeBodyStr = 
          intercalateWith connRet2 hsClassRawType (filter (not.isAbstractClass) classes)
          -- mkRawClasses (filter (not.isAbstractClass) classes)



mkInterfaceHs :: AnnotateMap -> STGroup String -> ClassModule -> String    
mkInterfaceHs amap templates mod  = 
    renderTemplateGroup templates [ ("ifaceHeader", ifaceHeaderStr) 
                                  , ("ifaceImport", ifaceImportStr)
                                  , ("ifaceBody", ifaceBodyStr)]  "Interface.hs" 
  where ifaceHeaderStr = "module HROOT.Class." ++ cmModule mod ++ ".Interface where\n" 
        classes = cmClass mod
        ifaceImportStr = genImportInInterface mod
        -- runReader (genModuleDecl mod) amap
        ifaceBodyStr = 
          runReader (genAllHsFrontDecl classes) amap 
          `connRet2`
          intercalateWith connRet hsClassExistType (filter (not.isAbstractClass) classes) 
          `connRet2`
          runReader (genAllHsFrontUpcastClass (filter (not.isAbstractClass) classes)) amap  



mkCastHs :: STGroup String -> ClassModule -> String    
mkCastHs templates mod  = 
    renderTemplateGroup templates [ ("castHeader", castHeaderStr) 
                                  , ("castImport", castImportStr)
                                  , ("castBody", castBodyStr) ]  
                                  castHsFileName
  where castHeaderStr = "module HROOT.Class." ++ cmModule mod ++ ".Cast where\n" 
        classes = cmClass mod
        castImportStr = genImportInCast mod
        castBodyStr = 
          genAllHsFrontInstCastable classes 
          `connRet2`
          intercalateWith connRet2 genHsFrontInstCastableSelf classes

mkImplementationHs :: AnnotateMap -> STGroup String -> ClassModule -> String
mkImplementationHs amap templates mod = 
    renderTemplateGroup templates 
                        [ ("implHeader", implHeaderStr) 
                        , ("implImport", implImportStr)
                        , ("implBody", implBodyStr ) ]
                        "Implementation.hs"
  where -- dmap = mkDaughterMap classes
        classes = cmClass mod
        implHeaderStr = "module HROOT.Class." ++ cmModule mod ++ ".Implementation where\n" 
        implImportStr = genImportInImplementation mod
        f y = intercalateWith connRet (flip genHsFrontInst y) (y:class_allparents y )
        g y = intercalateWith connRet (flip genHsFrontInstExistVirtual y) (y:class_allparents y )

        implBodyStr =  
          intercalateWith connRet2 f classes
          `connRet2` 
          intercalateWith connRet2 g (filter (not.isAbstractClass) classes)
          `connRet2`
          runReader (genAllHsFrontInstNew classes) amap
          `connRet2`
          genAllHsFrontInstNonVirtual classes
          `connRet2`
          intercalateWith connRet id (mapMaybe genHsFrontInstStatic classes)
          `connRet2`
          genAllHsFrontInstExistCommon (filter (not.isAbstractClass) classes)
        

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

mkExistentialHs :: STGroup String -> ClassGlobal -> ClassModule -> String
mkExistentialHs templates cglobal mod = 
  let classes = filter (not.isAbstractClass) (cmClass mod)
      dsmap = cgDaughterSelfMap cglobal
      makeOneMother :: Class -> String 
      makeOneMother mother = 
        let daughters = case M.lookup mother dsmap of 
                             Nothing -> error "error in mkExistential"
                             Just lst -> filter (not.isAbstractClass) lst
            str = mkExistentialEach templates mother daughters
        in  str 
      existEachBody = intercalateWith connRet makeOneMother classes
      existHeaderStr = "module HROOT.Class."++cmModule mod++".Existential where"
      existImportStr = genImportInExistential dsmap mod
      hsfilestr = renderTemplateGroup 
                    templates 
                    [ ("existHeader", existHeaderStr)
                    , ("existImport", existImportStr)
                    , ("modname", cmModule mod)
                    , ( "existEachBody" , existEachBody) ]
                  "Existential.hs" 
  in  hsfilestr


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
  
