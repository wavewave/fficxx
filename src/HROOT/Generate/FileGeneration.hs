module HROOT.Generate.FileGeneration where

import Text.StringTemplate hiding (render)
import Text.StringTemplate.Helpers

import qualified Data.Map as M

import HROOT.Generate.Util
import HROOT.Generate.Templates
import HROOT.Generate.Class
import HROOT.Generate.CppCode
import HROOT.Generate.FFI 
import HROOT.Generate.HsCode

---- common function for daughter

mkDaughterDef :: ((Class,[Class]) -> String) -> DaughterMap -> String 
mkDaughterDef f m = 
  let lst = M.toList m 
      f' (x,xs) =  f (x,filter (not.isAbstractClass) xs) 
  in  concatMap f' lst 

---- Header and Cpp file

mkDeclHeader :: STGroup String -> [Class] -> String 
mkDeclHeader templates classes = 
  let declDefStr     = classesCppDeclsVirtual classes 
                       `connRet2`
                       classesCppDeclsNonVirtual classes 
      typeDeclStr    = classesCppTypeDecls classes 
      dsmap           = mkDaughterSelfMap classes
      classDeclsStr  = mkDaughterDef classCppDeclsInstancesVirtual dsmap
                       `connRet2` 
                       classesCppDeclsInstancesNonVirtual classes
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
      cppBody = classesCppDefsVirtual classes
                `connRet2`
                classesCppDefsNonVirtual classes
                `connRet2`
                mkDaughterDef classCppDefInstancesVirtual dsmap
                `connRet2` 
                classesCppDefsInstancesNonVirtual classes

  in  renderTemplateGroup 
        templates 
        [ ("headerfilename", headerFileName ) 
        , ("cppbody"       , cppBody ) ] 
        definitionTemplate

mkFunctionHsc :: STGroup String -> [Class] -> String 
mkFunctionHsc templates classes = 
  renderTemplateGroup templates
                      [ ("headerFileName", headerFileName)
                      , ("hsFunctionBody", mkFFIClasses classes) ]  
                      "Function.hsc" 
                     
mkTypeHs :: STGroup String -> [Class] -> String                      
mkTypeHs templates classes = 
  renderTemplateGroup templates [ ("typeBody", typebody) ]  "Type.hs" 
  where typebody = mkRawClasses (filter (not.isAbstractClass) classes)
  
  
mkClassHs :: STGroup String -> [Class] -> String
mkClassHs templates classes = 
  renderTemplateGroup templates 
                      [ ("classBody", classBodyStr ) ]
                      "Class.hs"
  where dmap = mkDaughterMap classes
        classBodyStr = classesToHsDecls classes 
                       `connRet2`
                       mkInterfaceCastableInstance classes 
                       `connRet2`
                       mkClassInstances classes dmap 
                       `connRet2`
                       classesToHsDefNews classes 
                       `connRet2`
                       intercalateWith connRet hsClassMethodNonVirtual classes 
                       
                       
                       
                       