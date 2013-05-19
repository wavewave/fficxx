module Bindings.Cxx.Generate.Generator.ContentMaker where 

import           Control.Applicative
import           Control.Lens (set,at)
import           Control.Monad.Trans.Reader
import qualified Data.Map as M
import           Data.List 
import           Data.Maybe
import           System.FilePath 
import           Text.StringTemplate hiding (render)
-- 
import           Bindings.Cxx.Generate.Code.Cpp
import           Bindings.Cxx.Generate.Code.HsFFI 
import           Bindings.Cxx.Generate.Code.HsFrontEnd
import           Bindings.Cxx.Generate.Type.Annotate
import           Bindings.Cxx.Generate.Type.Class
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
mkParentDef f cls = g (class_allparents cls,cls)
  where g (ps,c) = concatMap (\p -> f (p,c)) ps


-- | 
mkProtectedFunctionList :: Class -> String 
mkProtectedFunctionList c = 
    (unlines 
     . map (\x->"#define IS_" ++ class_name c ++ "_" ++ x ++ "_PROTECTED ()") 
     . unProtected . class_protected) c 




-- |
mkTypeDeclHeader :: STGroup String -- -> ClassGlobal 
                 -> String -- ^ typemacro 
                 -> [Class]
                 -> String 
mkTypeDeclHeader templates typemacro classes =
  let typeDeclBodyStr   = genAllCppHeaderTmplType classes 
  in  renderTemplateGroup 
        templates 
        [ ("typeDeclBody", typeDeclBodyStr ) 
        , ("typemacro", typemacro ) 

        ] 
        typeDeclHeaderFileName

-- | 
mkDeclHeader :: STGroup String 
             -> ClassGlobal 
             -> String  -- ^ C prefix 
             -> ClassImportHeader 
             -> String 
mkDeclHeader templates _cglobal cprefix header =
  let classes = [cihClass header]
      aclass = cihClass header
      declHeaderStr = intercalateWith connRet (\x->"#include \""++x++"\"") $
                        cihIncludedHPkgHeadersInH header
      declDefStr    = genAllCppHeaderTmplVirtual classes 
                      `connRet2`
                      genAllCppHeaderTmplNonVirtual classes 
                      `connRet2`   
                      genAllCppDefTmplVirtual classes
                      `connRet2`
                       genAllCppDefTmplNonVirtual classes
      -- dsmap         = cgDaughterSelfMap cglobal
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
      namespaceStr = (concatMap (\x->"using namespace " ++ unNamespace x ++ ";\n") . cihNamespace) header
      aclass = cihClass header
      cppBody = mkProtectedFunctionList (cihClass header) 
                `connRet`
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
        , ("namespace", namespaceStr ) 
        , ("cppbody", cppBody ) 
        , ("modname", class_name (cihClass header)) ] 
        definitionTemplate

-- | 
mkFFIHsc :: STGroup String 
         -> ClassModule 
         -> String 
mkFFIHsc templates m = 
    renderTemplateGroup templates 
                        [ ("ffiHeader", ffiHeaderStr)
                        , ("ffiImport", ffiImportStr)
                        , ("cppInclude", cppIncludeStr)
                        , ("hsFunctionBody", genAllHsFFI headers) ]
                        ffiHscFileName
  where mname = cmModule m
        -- classes = cmClass m
        headers = cmCIH m
        ffiHeaderStr = "module " ++ mname <.> "FFI where\n"
        ffiImportStr = "import " ++ mname <.> "RawType\n"
                       ++ genImportInFFI m
        cppIncludeStr = genModuleIncludeHeader headers

-- |                      
mkRawTypeHs :: STGroup String 
            -> ClassModule 
            -> String
mkRawTypeHs templates m = 
    renderTemplateGroup templates [ ("rawtypeHeader", rawtypeHeaderStr) 
                                  , ("rawtypeBody", rawtypeBodyStr)] rawtypeHsFileName
  where rawtypeHeaderStr = "module " ++ cmModule m <.> "RawType where\n"
        classes = cmClass m
        rawtypeBodyStr = 
          intercalateWith connRet2 hsClassRawType (filter (not.isAbstractClass) classes)

-- | 
mkInterfaceHs :: AnnotateMap 
              -> STGroup String 
              -> ClassModule 
              -> String    
mkInterfaceHs amap templates m = 
    renderTemplateGroup templates [ ("ifaceHeader", ifaceHeaderStr) 
                                  , ("ifaceImport", ifaceImportStr)
                                  , ("ifaceBody", ifaceBodyStr)]  "Interface.hs" 
  where ifaceHeaderStr = "module " ++ cmModule m <.> "Interface where\n" 
        classes = cmClass m
        ifaceImportStr = genImportInInterface m
        ifaceBodyStr = 
          runReader (genAllHsFrontDecl classes) amap 
          `connRet2`
          intercalateWith connRet hsClassExistType (filter (not.isAbstractClass) classes) 
          `connRet2`
          runReader (genAllHsFrontUpcastClass (filter (not.isAbstractClass) classes)) amap  
          `connRet2`
          runReader (genAllHsFrontDowncastClass (filter (not.isAbstractClass) classes)) amap

-- | 
mkCastHs :: STGroup String -> ClassModule -> String    
mkCastHs templates m  = 
    renderTemplateGroup templates [ ("castHeader", castHeaderStr) 
                                  , ("castImport", castImportStr)
                                  , ("castBody", castBodyStr) ]  
                                  castHsFileName
  where castHeaderStr = "module " ++ cmModule m <.> "Cast where\n" 
        classes = cmClass m
        castImportStr = genImportInCast m
        castBodyStr = 
          genAllHsFrontInstCastable classes 
          `connRet2`
          intercalateWith connRet2 genHsFrontInstCastableSelf classes

-- | 
mkImplementationHs :: AnnotateMap 
                   -> STGroup String  -- ^ template 
                   -> ClassModule 
                   -> String
mkImplementationHs amap templates m = 
    renderTemplateGroup templates 
                        [ ("implHeader", implHeaderStr) 
                        , ("implImport", implImportStr)
                        , ("implBody", implBodyStr ) ]
                        "Implementation.hs"
  where classes = cmClass m
        implHeaderStr = "module " ++ cmModule m <.> "Implementation where\n" 
        implImportStr = genImportInImplementation m
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
mkExistentialHs templates cglobal m = 
  let classes = filter (not.isAbstractClass) (cmClass m)
      dsmap = cgDaughterSelfMap cglobal
      makeOneMother :: Class -> String 
      makeOneMother mother = 
        let daughters = case M.lookup (getClassModuleBase mother) dsmap of 
                             Nothing -> error "error in mkExistential"
                             Just lst -> filter (not.isAbstractClass) lst
            str = mkExistentialEach templates mother daughters
        in  str 
      existEachBody = intercalateWith connRet makeOneMother classes
      existHeaderStr = "module " ++ cmModule m <.> "Existential where"
      existImportStr = genImportInExistential dsmap m
      hsfilestr = renderTemplateGroup 
                    templates 
                    [ ("existHeader", existHeaderStr)
                    , ("existImport", existImportStr)
                    , ("modname", cmModule m)
                    , ( "existEachBody" , existEachBody) ]
                  "Existential.hs" 
  in  hsfilestr

-- | 
mkModuleHs :: STGroup String 
           -> ClassModule 
           -> String 
mkModuleHs templates m = 
    let str = renderTemplateGroup 
                templates 
                [ ("moduleName", cmModule m) 
                , ("exportList", genExportList (cmClass m)) 
                , ("importList", genImportInModule (cmClass m))
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

