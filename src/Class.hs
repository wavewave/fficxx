module Class where

import Data.Char

import Text.StringTemplate
import Text.StringTemplate.Helpers


import Util
import Function
import Templates

data Class = Class { 
    class_name :: String, 
    class_funcs :: [Function]
  }


-- Class Declaration and Definition


classToDecl :: STGroup String -> Class -> String 
classToDecl templates aclass =  
  let declBodyStr = 
        renderTemplateGroup templates 
                            [ ("classname", map toUpper (class_name aclass) ) 
                            , ("funcdecl" , funcDeclStr ) ] 
                            declbodyTemplate 
      funcDeclStr = funcsToDecls templates (class_funcs aclass)
        
  in  declBodyStr 
      

classesToDecls :: STGroup String -> [Class] -> String 
classesToDecls templates = intercalateWith connRet2 (classToDecl templates)



classToDef :: STGroup String -> Class -> String 
classToDef templates aclass =  
  let defBodyStr = 
        renderTemplateGroup templates 
                            [ ("classname", map toUpper (class_name aclass) ) 
                            , ("funcdef" , funcDefStr ) ] 
                            classDefTemplate 
      funcDefStr = funcsToDefs templates (class_funcs aclass)
        
  in  defBodyStr 
      
classesToDefs templates = intercalateWith connRet2 (classToDecl templates)

  
---- Header and Cpp file

mkDeclHeader :: STGroup String -> [Class] -> String 
mkDeclHeader templates classes = 
  let decl        = renderTemplateGroup templates 
                                        [ ("declarationbody", declBodyStr ) ] 
                                        declarationTemplate
      declBodyStr = classesToDecls templates classes
  in  decl
      
mkDefMain :: STGroup String -> [Class] -> String 
mkDefMain templates classes =
  let def        = renderTemplateGroup templates 
                                        [ ("headerfilename", headerFileName ) 
                                        , ("cppbody"       , cppBody ) ] 
                                        definitionTemplate
      cppBody = classesToDefs templates classes
  in  def

