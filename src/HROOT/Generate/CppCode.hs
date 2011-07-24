module HROOT.Generate.CppCode where

import Data.Char 

import qualified Data.Map as M

import HROOT.Generate.Util
import HROOT.Generate.Function
import HROOT.Generate.Class

-- Class Declaration and Definition

----
---- Declaration
----

---- "Class Type Declaration" Instances

classCppTypeDecl :: Class -> String 
classCppTypeDecl c = let tmpl = "ROOT_TYPE_DECLARATION($classname$);" 
                    in  render tmpl [ ("classname", class_name c) ] 

classesCppTypeDecls :: [Class] -> String
classesCppTypeDecls = intercalateWith connRet (classCppTypeDecl ) 

---- "Class Declaration Virtual" Declaration 

classCppDeclVirtual :: Class -> String 
classCppDeclVirtual aclass =  
  let tmpl = "#undef ROOT_$classname$_DECLARATIONVIRT\\\n#define ROOT_$classname$_DECLARATIONVIRT(Type) \\\\\\\n$funcdecl$" 
      declBodyStr = render tmpl [ ("classname", map toUpper (class_name aclass) ) 
                                 , ("funcdecl" , funcDeclStr ) ] 
      funcDeclStr = funcsToDecls . virtualFuncs . class_funcs $ aclass
  in  declBodyStr 
      
classesCppDeclsVirtual :: [Class] -> String 
classesCppDeclsVirtual = intercalateWith connRet2 classCppDeclVirtual

---- "Class Declaration Non-Virtual" Declaration

classCppDeclNonVirtual :: Class -> String
classCppDeclNonVirtual c = 
  let tmpl = "#undef ROOT_$classname$_DECLARATIONNONVIRT\\\n#define ROOT_$classname$_DECLARATIONNONVIRT(Type) \\\\\\\n$funcdecl$" 
      declBodyStr = render tmpl [ ("classname", map toUpper (class_name c) ) 
                                 , ("funcdecl" , funcDeclStr ) ] 
      funcDeclStr = funcsToDecls . filter (not.isVirtualFunc) 
                                 . class_funcs $ c
  in  declBodyStr 


classesCppDeclsNonVirtual :: [Class] -> String 
classesCppDeclsNonVirtual = intercalateWith connRet classCppDeclNonVirtual

---- "Class Declaration Virtual/NonVirtual" Instances

classCppDeclsInstancesVirtual :: (Class,[Class]) -> String 
classCppDeclsInstancesVirtual (c,cs) = 
  let strc = map toUpper (class_name c) 
  in  concatMap (\y ->"ROOT_"++strc++"_DECLARATIONVIRT(" ++ class_name y ++ ");\n") cs

classCppDeclInstanceNonVirtual :: Class -> String 
classCppDeclInstanceNonVirtual c = 
  let strx = map toUpper (class_name c) 
  in  "ROOT_"++strx++"_DECLARATIONNONVIRT(" ++ class_name c ++ ");\n" 

classesCppDeclsInstancesNonVirtual :: [Class] -> String 
classesCppDeclsInstancesNonVirtual = 
  intercalateWith connRet classCppDeclInstanceNonVirtual


----
---- Definition
----

---- "Class Definition Virtual" Declaration

classCppDefVirtual :: Class -> String 
classCppDefVirtual aclass =  
  let tmpl = "#undef ROOT_$classname$_DEFINITIONVIRT\\\n#define ROOT_$classname$_DEFINITIONVIRT(Type)\\\\\\\n$funcdef$" 
      defBodyStr = render tmpl [ ("classname", map toUpper (class_name aclass) ) 
                               , ("funcdef" , funcDefStr ) ] 
      funcDefStr = funcsToDefs . virtualFuncs . class_funcs $ aclass
  in  defBodyStr 
      
classesCppDefsVirtual :: [Class] -> String
classesCppDefsVirtual = intercalateWith connRet2 classCppDefVirtual

---- "Class Definition NonVirtual" Declaration

classCppDefNonVirtual :: Class -> String 
classCppDefNonVirtual aclass =  
  let tmpl = "#undef ROOT_$classname$_DEFINITIONNONVIRT\\\n#define ROOT_$classname$_DEFINITIONNONVIRT(Type)\\\\\\\n$funcdef$" 
      defBodyStr = render tmpl [ ("classname", map toUpper (class_name aclass) ) 
                               , ("funcdef" , funcDefStr ) ] 
      funcDefStr = funcsToDefs . filter (not.isVirtualFunc) 
                               . class_funcs $ aclass
  in  defBodyStr 
      
classesCppDefsNonVirtual :: [Class] -> String
classesCppDefsNonVirtual = intercalateWith connRet2 classCppDefNonVirtual

---- "Class Definition Virtual/NonVirtual" Instances

classCppDefInstancesVirtual :: (Class,[Class]) -> String 
classCppDefInstancesVirtual (c,cs) = 
  let strc = map toUpper (class_name c) 
  in  concatMap (\y ->"ROOT_"++strc++"_DEFINITIONVIRT(" ++ class_name y ++ ")\n") cs

classCppDefInstanceNonVirtual :: Class -> String
classCppDefInstanceNonVirtual c = 
  let tmpl = "ROOT_$capitalclassname$_DEFINITIONNONVIRT($classname$)" 
  in  render tmpl [ ("capitalclassname", toUppers (class_name c))
                  , ("classname", class_name c) ] 

classesCppDefsInstancesNonVirtual :: [Class] -> String 
classesCppDefsInstancesNonVirtual = 
  intercalateWith connRet classCppDefInstanceNonVirtual

-----


-----------------


  
----

