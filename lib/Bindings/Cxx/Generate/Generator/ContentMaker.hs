module Bindings.Cxx.Generate.Generator.ContentMaker where 

import           Control.Applicative
import           Control.Lens (set,at)
import           Control.Monad.Trans.Reader
import qualified Data.Map as M
import           Data.List 
import           Data.Maybe
import           Distribution.Package
import           Distribution.PackageDescription hiding (exposedModules)
import           Distribution.PackageDescription.Parse
import           Distribution.Verbosity
import           Distribution.Version 
import           System.Directory 
import           System.FilePath 
import           System.IO
import           Text.StringTemplate hiding (render)
-- 
import           Bindings.Cxx.Generate.Code.Cpp
import           Bindings.Cxx.Generate.Code.HsFFI 
import           Bindings.Cxx.Generate.Code.HsFrontEnd
import           Bindings.Cxx.Generate.Code.Cabal 
import           Bindings.Cxx.Generate.Config
import           Bindings.Cxx.Generate.Type.Annotate
import           Bindings.Cxx.Generate.Type.Class
import           Bindings.Cxx.Generate.Type.Module 
-- import           Bindings.Cxx.Generate.Type.Method
import qualified Bindings.Cxx.Generate.Type.PackageInterface as T
import           Bindings.Cxx.Generate.Util
--
 

srcDir :: FilePath -> FilePath
srcDir installbasedir = installbasedir </> "src" 

csrcDir :: FilePath -> FilePath
csrcDir installbasedir = installbasedir </> "csrc" 

moduleTemplate :: String 
moduleTemplate = "module.hs"

-- cabalTemplate :: String 
-- cabalTemplate = "Pkg.cabal"

declarationTemplate :: String
declarationTemplate = "Pkg.h"

typeDeclHeaderFileName :: String
typeDeclHeaderFileName = "PkgType.h"

declbodyTemplate :: String
declbodyTemplate    = "declbody.h"

funcdeclTemplate :: String
funcdeclTemplate    = "funcdecl.h" 

definitionTemplate :: String
definitionTemplate = "Pkg.cpp"

classDefTemplate :: String
classDefTemplate   = "classdef.cpp"

functionTemplate :: String
functionTemplate   = "function.cpp" 

funcbodyTemplate :: String
funcbodyTemplate   = "functionbody.cpp"

headerFileName :: String
headerFileName = "Pkg.h"

cppFileName :: String
cppFileName = "Pkg.cpp" 

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


-- | 
mkGlobal :: [Class] -> ClassGlobal
mkGlobal = ClassGlobal <$> mkDaughterSelfMap <*> mkDaughterMap 


-- | 
mkDaughterDef :: ((String,[Class]) -> String) 
              -> DaughterMap 
              -> String 
mkDaughterDef f m =   
    let lst = M.toList m 
        f' (x,xs) =  f (x,filter (not.isAbstractClass) xs) 
    in (concatMap f' lst)

-- | 
mkParentDef :: ((Class,Class)->String) -> Class -> String
mkParentDef f c = g (class_allparents c,c)
  where g (ps,c) = concatMap (\p -> f (p,c)) ps


-- |
mkTypeDeclHeader :: STGroup String -> ClassGlobal 
             -> [Class]
             -> String 
mkTypeDeclHeader templates cglobal classes =
  let typeDeclBodyStr   = genAllCppHeaderTmplType classes 
  in  renderTemplateGroup 
        templates 
        [ ("typeDeclBody", typeDeclBodyStr ) ] 
        typeDeclHeaderFileName

-- | 
mkDeclHeader :: STGroup String 
             -> ClassGlobal 
             -> String  -- ^ C prefix 
             -> ClassImportHeader 
             -> String 
mkDeclHeader templates cglobal cprefix header =
  let classes = [cihClass header]
      aclass = cihClass header
      declHeaderStr = intercalateWith connRet (\x->"#include \""++x++"\"") $
                        cihIncludedHPkgHeaders header
      declDefStr    = genAllCppHeaderTmplVirtual classes 
                      `connRet2`
                      genAllCppHeaderTmplNonVirtual classes 
                      `connRet2`   
                      genAllCppDefTmplVirtual classes
                      `connRet2`
                       genAllCppDefTmplNonVirtual classes
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
                      classDeclsStr 
  in  renderTemplateGroup 
        templates 
        [ ("cprefix", cprefix)
        , ("declarationheader", declHeaderStr ) 
        , ("declarationbody", declBodyStr ) ] 
        declarationTemplate

-- | 
mkDefMain :: STGroup String 
          -> ClassImportHeader 
          -> String 
mkDefMain templates header =
  let classes = [cihClass header]
      headerStr = genAllCppHeaderInclude header ++ "\n#include \"" ++ (cihSelfHeader header) ++ "\"" 
      aclass = cihClass header
      cppBody = mkParentDef genCppDefInstVirtual (cihClass header)
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

-- | 
mkFFIHsc :: STGroup String 
         -> ClassModule 
         -> String 
mkFFIHsc templates mod = 
    renderTemplateGroup templates 
                        [ ("ffiHeader", ffiHeaderStr)
                        , ("ffiImport", ffiImportStr)
                        , ("cppInclude", cppIncludeStr)
                        , ("hsFunctionBody", genAllHsFFI headers) ]
                        ffiHscFileName
  where mname = cmModule mod
        classes = cmClass mod
        headers = cmCIH mod
        ffiHeaderStr = "module " ++ mname <.> "FFI where\n"
        ffiImportStr = "import " ++ mname <.> "RawType\n"
                       ++ genImportInFFI mod
        cppIncludeStr = genModuleIncludeHeader headers

-- |                      
mkRawTypeHs :: STGroup String 
            -> ClassModule 
            -> String
mkRawTypeHs templates mod = 
    renderTemplateGroup templates [ ("rawtypeHeader", rawtypeHeaderStr) 
                                  , ("rawtypeBody", rawtypeBodyStr)] rawtypeHsFileName
  where rawtypeHeaderStr = "module " ++ cmModule mod <.> "RawType where\n"
        classes = cmClass mod
        rawtypeBodyStr = 
          intercalateWith connRet2 hsClassRawType (filter (not.isAbstractClass) classes)

-- | 
mkInterfaceHs :: AnnotateMap 
              -> STGroup String 
              -> ClassModule 
              -> String    
mkInterfaceHs amap templates mod  = 
    renderTemplateGroup templates [ ("ifaceHeader", ifaceHeaderStr) 
                                  , ("ifaceImport", ifaceImportStr)
                                  , ("ifaceBody", ifaceBodyStr)]  "Interface.hs" 
  where ifaceHeaderStr = "module " ++ cmModule mod <.> "Interface where\n" 
        classes = cmClass mod
        ifaceImportStr = genImportInInterface mod
        ifaceBodyStr = 
          runReader (genAllHsFrontDecl classes) amap 
          `connRet2`
          intercalateWith connRet hsClassExistType (filter (not.isAbstractClass) classes) 
          `connRet2`
          runReader (genAllHsFrontUpcastClass (filter (not.isAbstractClass) classes)) amap  

-- | 
mkCastHs :: STGroup String -> ClassModule -> String    
mkCastHs templates mod  = 
    renderTemplateGroup templates [ ("castHeader", castHeaderStr) 
                                  , ("castImport", castImportStr)
                                  , ("castBody", castBodyStr) ]  
                                  castHsFileName
  where castHeaderStr = "module " ++ cmModule mod <.> "Cast where\n" 
        classes = cmClass mod
        castImportStr = genImportInCast mod
        castBodyStr = 
          genAllHsFrontInstCastable classes 
          `connRet2`
          intercalateWith connRet2 genHsFrontInstCastableSelf classes

-- | 
mkImplementationHs :: AnnotateMap 
                   -> STGroup String  -- ^ template 
                   -> ClassModule 
                   -> String
mkImplementationHs amap templates mod = 
    renderTemplateGroup templates 
                        [ ("implHeader", implHeaderStr) 
                        , ("implImport", implImportStr)
                        , ("implBody", implBodyStr ) ]
                        "Implementation.hs"
  where classes = cmClass mod
        implHeaderStr = "module " ++ cmModule mod <.> "Implementation where\n" 
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
        
-- | 
mkExistentialEach :: STGroup String 
                  -> Class 
                  -> [Class] 
                  -> String 
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

-- | 
mkExistentialHs :: STGroup String 
                -> ClassGlobal 
                -> ClassModule 
                -> String
mkExistentialHs templates cglobal mod = 
  let classes = filter (not.isAbstractClass) (cmClass mod)
      dsmap = cgDaughterSelfMap cglobal
      makeOneMother :: Class -> String 
      makeOneMother mother = 
        let daughters = case M.lookup (getClassModuleBase mother) dsmap of 
                             Nothing -> error "error in mkExistential"
                             Just lst -> filter (not.isAbstractClass) lst
            str = mkExistentialEach templates mother daughters
        in  str 
      existEachBody = intercalateWith connRet makeOneMother classes
      existHeaderStr = "module " ++ cmModule mod <.> "Existential where"
      existImportStr = genImportInExistential dsmap mod
      hsfilestr = renderTemplateGroup 
                    templates 
                    [ ("existHeader", existHeaderStr)
                    , ("existImport", existImportStr)
                    , ("modname", cmModule mod)
                    , ( "existEachBody" , existEachBody) ]
                  "Existential.hs" 
  in  hsfilestr

-- | 
mkModuleHs :: STGroup String 
           -> ClassModule 
           -> String 
mkModuleHs templates mod = 
    let str = renderTemplateGroup 
                templates 
                [ ("moduleName", cmModule mod) 
                , ("exportList", genExportList (cmClass mod)) 
                , ("importList", genImportInModule (cmClass mod))
                ]
                moduleTemplate 
    in str
  
-- |
mkPackageInterface :: T.PackageInterface 
                   -> T.PackageName 
                   -> [ClassImportHeader] 
                   -> T.PackageInterface
mkPackageInterface pinfc pkgname = foldr f pinfc 
  where f cih repo = 
          let name = (class_name . cihClass) cih 
              header = cihSelfHeader cih 
          in set (at (pkgname,T.ClsName name)) (Just (T.HdrName header)) repo

