module HROOT.Generate.Generator.Driver where

import Text.StringTemplate hiding (render)
import Text.StringTemplate.Helpers

import qualified Data.Map as M

import HROOT.Generate.Util
import HROOT.Generate.Type.Class
import HROOT.Generate.Code.Cpp
import HROOT.Generate.Code.HsFFI 
import HROOT.Generate.Code.HsFrontEnd

import System.FilePath 

----- 

srcDir :: FilePath -> FilePath
srcDir installbasedir = installbasedir </> "src" </> "HROOT"

csrcDir :: FilePath -> FilePath
csrcDir installbasedir = installbasedir </> "csrc" 

declarationTemplate :: String
declarationTemplate = "HROOT.h"

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
hscFileName = "Function.hsc"

hsFileName :: String
hsFileName  = "Class.hs"

typeHsFileName :: String
typeHsFileName = "Type.hs"


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
                      , ("hsFunctionBody", mkFFIClasses headerFileName classes) ]  
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
                       
                       
                       
                       