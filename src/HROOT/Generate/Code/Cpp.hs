module HROOT.Generate.Code.Cpp where

import Data.Char 
import Data.List
import System.FilePath

import HROOT.Generate.Util
import HROOT.Generate.Type.Method
import HROOT.Generate.Type.Class
import HROOT.Generate.Code.MethodDef
import HROOT.Generate.Code.Cabal

-- Class Declaration and Definition

----
---- Declaration
----

---- "Class Type Declaration" Instances

genCppHeaderTmplType :: Class -> String 
genCppHeaderTmplType c = let tmpl = "ROOT_TYPE_DECLARATION($classname$);" 
                     in  render tmpl [ ("classname", class_name c) ] 

genAllCppHeaderTmplType :: [Class] -> String
genAllCppHeaderTmplType = intercalateWith connRet (genCppHeaderTmplType) 

---- "Class Declaration Virtual" Declaration 

genCppHeaderTmplVirtual :: Class -> String 
genCppHeaderTmplVirtual aclass =  
  let tmpl = "#undef ROOT_$classname$_DECLARATIONVIRT\\\n#define ROOT_$classname$_DECLARATIONVIRT(Type) \\\\\\\n$funcdecl$" 
      declBodyStr = render tmpl [ ("classname", map toUpper (class_name aclass) ) 
                                 , ("funcdecl" , funcDeclStr ) ] 
      funcDeclStr = (funcsToDecls aclass) . virtualFuncs . class_funcs $ aclass
  in  declBodyStr 
      
genAllCppHeaderTmplVirtual :: [Class] -> String 
genAllCppHeaderTmplVirtual = intercalateWith connRet2 genCppHeaderTmplVirtual

---- "Class Declaration Non-Virtual" Declaration

genCppHeaderTmplNonVirtual :: Class -> String
genCppHeaderTmplNonVirtual c = 
  let tmpl = "#undef ROOT_$classname$_DECLARATIONNONVIRT\\\n#define ROOT_$classname$_DECLARATIONNONVIRT(Type) \\\\\\\n$funcdecl$" 
      declBodyStr = render tmpl [ ("classname", map toUpper (class_name c) ) 
                                 , ("funcdecl" , funcDeclStr ) ] 
      funcDeclStr = (funcsToDecls c) . filter (not.isVirtualFunc) 
                                     . class_funcs $ c
  in  declBodyStr 

genAllCppHeaderTmplNonVirtual :: [Class] -> String 
genAllCppHeaderTmplNonVirtual = intercalateWith connRet genCppHeaderTmplNonVirtual

---- "Class Declaration Virtual/NonVirtual" Instances

genCppHeaderInstVirtual :: (Class,Class) -> String 
genCppHeaderInstVirtual (p,c) = 
  let strc = map toUpper (class_name p) 
  in  "ROOT_"++strc++"_DECLARATIONVIRT(" ++ class_name c ++ ");\n"

genCppHeaderInstNonVirtual :: Class -> String 
genCppHeaderInstNonVirtual c = 
  let strx = map toUpper (class_name c) 
  in  "ROOT_"++strx++"_DECLARATIONNONVIRT(" ++ class_name c ++ ");\n" 

genAllCppHeaderInstNonVirtual :: [Class] -> String 
genAllCppHeaderInstNonVirtual = 
  intercalateWith connRet genCppHeaderInstNonVirtual


----
---- Definition
----

---- "Class Definition Virtual" Declaration

genCppDefTmplVirtual :: Class -> String 
genCppDefTmplVirtual aclass =  
  let tmpl = "#undef ROOT_$classname$_DEFINITIONVIRT\\\n#define ROOT_$classname$_DEFINITIONVIRT(Type)\\\\\\\n$funcdef$" 
      defBodyStr = render tmpl [ ("classname", map toUpper (class_name aclass) ) 
                               , ("funcdef" , funcDefStr ) ] 
      funcDefStr = (funcsToDefs aclass) . virtualFuncs . class_funcs $ aclass
  in  defBodyStr 
      
genAllCppDefTmplVirtual :: [Class] -> String
genAllCppDefTmplVirtual = intercalateWith connRet2 genCppDefTmplVirtual

---- "Class Definition NonVirtual" Declaration

genCppDefTmplNonVirtual :: Class -> String 
genCppDefTmplNonVirtual aclass =  
  let tmpl = "#undef ROOT_$classname$_DEFINITIONNONVIRT\\\n#define ROOT_$classname$_DEFINITIONNONVIRT(Type)\\\\\\\n$funcdef$" 
      defBodyStr = render tmpl [ ("classname", map toUpper (class_name aclass) ) 
                               , ("funcdef" , funcDefStr ) ] 
      funcDefStr = (funcsToDefs aclass) . filter (not.isVirtualFunc) 
                                        . class_funcs $ aclass
  in  defBodyStr 
      
genAllCppDefTmplNonVirtual :: [Class] -> String
genAllCppDefTmplNonVirtual = intercalateWith connRet2 genCppDefTmplNonVirtual

---- "Class Definition Virtual/NonVirtual" Instances

genCppDefInstVirtual :: (Class,Class) -> String 
genCppDefInstVirtual (p,c) = 
  let strc = map toUpper (class_name p) 
  in  "ROOT_"++strc++"_DEFINITIONVIRT(" ++ class_name c ++ ")\n"

genCppDefInstNonVirtual :: Class -> String
genCppDefInstNonVirtual c = 
  let tmpl = "ROOT_$capitalclassname$_DEFINITIONNONVIRT($classname$)" 
  in  render tmpl [ ("capitalclassname", toUppers (class_name c))
                  , ("classname", class_name c) ] 

genAllCppDefInstNonVirtual :: [Class] -> String 
genAllCppDefInstNonVirtual = 
  intercalateWith connRet genCppDefInstNonVirtual

-----


-----------------

genAllCppHeaderInclude :: ClassImportHeader -> String 
genAllCppHeaderInclude header = 
    intercalateWith connRet (\x->"#include \""++x++"\"") $
      cihIncludedHROOTHeaders header
        ++ cihIncludedCROOTHeaders header

genModuleIncludeHeader :: [ClassImportHeader] -> String 
genModuleIncludeHeader headers =
  let strlst = map ((\x->"#include \""++x++"\"") . cihSelfHeader) headers 
  in  intercalate "\n" strlst 

-----

  
----

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


