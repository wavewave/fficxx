module FileGeneration where

import System.IO
import System.Directory
import System.Process
import System.FilePath ((</>))

import Text.StringTemplate hiding (render)
import Text.StringTemplate.Helpers

import CType
import Util
import Templates
import Function
import Class
import ROOT
import FFI 

mkFunctionHsc :: STGroup String -> [Class] -> String 
mkFunctionHsc templates classes = 
  renderTemplateGroup templates
                      [ ("headerFileName", headerFileName)
                      , ("hsFunctionBody", mkFFIClasses classes) ]  
                      "Function.hsc" 
                     
mkTypeHs :: STGroup String -> [Class] -> String                      
mkTypeHs templates cclass = 
  renderTemplateGroup templates [ ("typeBody", typebody) ]  "Type.hs" 
  where typebody = mkRawClasses cclass 
  
  
mkClassHs :: STGroup String -> [Class] -> [Class] -> String
mkClassHs templates aclass cclass = 
  renderTemplateGroup templates 
                      [ ("classBody", classBodyStr ) ]
                      "Class.hs"
  where dmap = mkDaughterMap cclass
        classBodyStr = classesToHsDecls aclass `connRet2`
                       mkClassInstances dmap
                       
                       
                       