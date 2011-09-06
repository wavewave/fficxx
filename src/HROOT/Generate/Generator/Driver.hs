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
srcDir installbasedir = installbasedir </> "src" </> "HROOT" </> "Class"

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
hscFileName = "FFI.hsc"

hsFileName :: String
hsFileName  = "Implementation.hs"

typeHsFileName :: String
typeHsFileName = "Interface.hs"


---- common function for daughter

mkDaughterDef :: ((Class,[Class]) -> String) -> DaughterMap -> String 
mkDaughterDef f m = 
  let lst = M.toList m 
      f' (x,xs) =  f (x,filter (not.isAbstractClass) xs) 
  in  concatMap f' lst 

---- Header and Cpp file

mkDeclHeader :: STGroup String -> [Class] -> String 
mkDeclHeader templates classes = 
  let declDefStr     = genAllCppHeaderTmplVirtual classes 
                       `connRet2`
                       genAllCppHeaderTmplNonVirtual classes 
      typeDeclStr    = genAllCppHeaderTmplType classes 
      dsmap           = mkDaughterSelfMap classes
      classDeclsStr  = mkDaughterDef genCppHeaderInstVirtual dsmap
                       `connRet2` 
                       genAllCppHeaderInstNonVirtual classes
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
      cppBody = genAllCppDefTmplVirtual classes
                `connRet2`
                genAllCppDefTmplNonVirtual classes
                `connRet2`
                mkDaughterDef genCppDefInstVirtual dsmap
                `connRet2` 
                genAllCppDefInstNonVirtual classes

  in  renderTemplateGroup 
        templates 
        [ ("headerfilename", headerFileName ) 
        , ("cppbody"       , cppBody ) ] 
        definitionTemplate

mkFFIHsc :: STGroup String -> [Class] -> String 
mkFFIHsc templates classes = 
  renderTemplateGroup templates
                      [ ("headerFileName", headerFileName)
                      , ("hsFunctionBody", genAllHsFFI headerFileName classes) ]  
                      "FFI.hsc" 
                     
mkInterfaceHs :: STGroup String -> [Class] -> String                      
mkInterfaceHs templates classes = 
  renderTemplateGroup templates [("ifaceBody", ifaceBodyStr)]  "Interface.hs" 
  where ifaceBodyStr = 
          mkRawClasses (filter (not.isAbstractClass) classes)
          `connRet2`
          genAllHsFrontDecl classes
  
  
mkImplementationHs :: STGroup String -> [Class] -> String
mkImplementationHs templates classes = 
  renderTemplateGroup templates 
                      [ ("implBody", implBodyStr ) ]
                      "Implementation.hs"
  where dmap = mkDaughterMap classes
        implBodyStr =  genAllHsFrontInstCastable classes 
                       `connRet2`
                       genAllHsFrontInstExist (filter (not.isAbstractClass) classes)
                       `connRet2`
                       genAllHsFrontInst classes dmap 
                       `connRet2`
                       genAllHsFrontInstNew classes 
                       `connRet2`
                       genAllHsFrontInstNonVirtual classes

                       
                       
                       
                       