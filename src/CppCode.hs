module CppCode where

import Data.Char 
import Text.StringTemplate hiding (render)

import qualified Data.Map as M

import CType
import Util
import Function
import Class
import Templates

-- Class Declaration and Definition


classToDeclDef :: Class -> String 
classToDeclDef aclass =  
  let tmpl = "#undef ROOT_$classname$_DECLARATION\\\n#define ROOT_$classname$_DECLARATION(Type) \\\\\\\n$funcdecl$" 
      declBodyStr = render tmpl [ ("classname", map toUpper (class_name aclass) ) 
                                 , ("funcdecl" , funcDeclStr ) ] 
      funcDeclStr = funcsToDecls (class_funcs aclass)
  in  declBodyStr 
      

classesToDeclsDef :: [Class] -> String 
classesToDeclsDef = intercalateWith connRet2 classToDeclDef

-----

classToTypeDecl :: Class -> String 
classToTypeDecl c = let tmpl = "ROOT_TYPE_DECLARATION($classname$);" 
                    in  render tmpl [ ("classname", class_name c) ] 

classesToTypeDecls = intercalateWith connRet (classToTypeDecl ) 

-----

classesToClassDecls :: DaughterMap -> String 
classesToClassDecls  m = 
  let lst = M.toList m 
      f (x,ys) = let strx = map toUpper (class_name x) 
                 in  concatMap (\y ->"ROOT_"++strx++"_DECLARATION(" ++ class_name y ++ ");\n") ys
  in  concatMap f lst


classSelfDecl :: Class -> String
classSelfDecl c = let tmpl = "ROOT_$capitalclassname$_DECLARATION($classname$);" 
                  in  render tmpl [ ("capitalclassname", toUppers (class_name c))
                                  , ("classname", class_name c) 
                                  ] 

classesSelfDecls :: [Class] -> String 
classesSelfDecls = intercalateWith connRet classSelfDecl 


classSelfDef :: Class -> String
classSelfDef c = let tmpl = "ROOT_$capitalclassname$_DEFINITION($classname$)" 
                  in  render tmpl [ ("capitalclassname", toUppers (class_name c))
                                  , ("classname", class_name c) 
                                  ] 

classesSelfDefs :: [Class] -> String 
classesSelfDefs = intercalateWith connRet classSelfDef 

-----

classToDef :: Class -> String 
classToDef aclass =  
  let tmpl = "#undef ROOT_$classname$_DEFINITION\\\n#define ROOT_$classname$_DEFINITION(Type)\\\\\\\n$funcdef$" 
      defBodyStr = render tmpl [ ("classname", map toUpper (class_name aclass) ) 
                               , ("funcdef" , funcDefStr ) ] 
      funcDefStr = funcsToDefs (class_funcs aclass)
  in  defBodyStr 
      
classesToDefs = intercalateWith connRet2 classToDef

  
----

