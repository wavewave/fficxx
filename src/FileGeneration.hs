module FileGeneration where

import Text.StringTemplate hiding (render)
import Text.StringTemplate.Helpers

import Data.Char
import qualified Data.Map as M

import Util
import Templates
import Class
import CppCode
import FFI 
import HsCode

---- Header and Cpp file

mkDeclHeader :: STGroup String -> [Class] -> String 
mkDeclHeader templates classes = 
  let declDefStr    = classesToDeclsDef  classes 
      typeDeclStr    = classesToTypeDecls classes 
      dmap = mkDaughterMap classes
      classDeclsStr  = classesToClassDecls dmap 
                       `connRet2` classesSelfDecls classes
      declBodyStr = declDefStr 
                    `connRet2` typeDeclStr 
                    `connRet2` classDeclsStr 
  in  renderTemplateGroup 
        templates 
        [ ("declarationbody", declBodyStr ) ] 
        declarationTemplate

mkDefMain :: STGroup String -> [Class] -> String 
mkDefMain templates classes =
  let cppBody = classesToDefs classes
  in  renderTemplateGroup 
        templates 
        [ ("headerfilename", headerFileName ) 
        , ("cppbody"       , cppBody ) ] 
        definitionTemplate

mkDaughterDef :: DaughterMap -> String 
mkDaughterDef m = 
  let lst = M.toList m 
      f (x,ys) = 
        let strx = map toUpper (class_name x) 
        in  (flip concatMap) ys $ \y ->"ROOT_"++strx++"_DEFINITION(" 
                                       ++ class_name y ++ ")\n" 
  in  concatMap f lst



mkFunctionHsc :: STGroup String -> [Class] -> String 
mkFunctionHsc templates classes = 
  renderTemplateGroup templates
                      [ ("headerFileName", headerFileName)
                      , ("hsFunctionBody", mkFFIClasses classes) ]  
                      "Function.hsc" 
                     
mkTypeHs :: STGroup String -> [Class] -> String                      
mkTypeHs templates classes = 
  renderTemplateGroup templates [ ("typeBody", typebody) ]  "Type.hs" 
  where typebody = mkRawClasses classes 
  
  
mkClassHs :: STGroup String -> [Class] -> String
mkClassHs templates classes = 
  renderTemplateGroup templates 
                      [ ("classBody", classBodyStr ) ]
                      "Class.hs"
  where dmap = mkDaughterMap classes
        classBodyStr = classesToHsDecls classes `connRet2`
                       mkInterfaceCastableInstance classes `connRet2`
                       mkClassInstances dmap `connRet2`
                       classesToHsDefNews classes `connRet2`
                       intercalateWith connRet hsClassMethodExport classes 
                       
                       
                       
                       