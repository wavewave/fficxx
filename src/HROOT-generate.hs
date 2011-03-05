module Main where

import System.IO
import System.Directory
import System.Process
import System.FilePath ((</>))

import Text.StringTemplate
import Text.StringTemplate.Helpers

import CType
import Util
import Templates
import Function


sampleargs = [ (CT CTString Const, "a") 
             , (CT CTInt NoConst, "b") ] 
sampleargsStr = argsToString sampleargs


void = Void 

sampleclass1 = Class "TNamed" [ Function Void    "SetTitle"        [cstring "name"] 
                              , Function Void    "SaveAs"          [cstring "filename", cstring "option"] 
                              , Function double_ "GetParameter"    [int     "idx" ] 
                              ]

sampleclass2 = Class "TObject" [ Function cstring_ "GetName" [] 
                               , Function void     "Draw"    [cstring "option"] ]


classes = [ sampleclass1, sampleclass2 ]


main :: IO () 
main = do 
  putStrLn "Automatic HROOT binding generation" 
  setCurrentDirectory scriptBaseDir
  
  templates <- directoryGroup templateDir 
  
  let str1 = renderTemplateGroup templates [ ("classname", "TTest") ] 
                                           definitionTemplate
      str2 = renderTemplateGroup templates [ ("returntype" , "rettype")
                                           , ("funcname" , "tester")
                                           , ("args" , sampleargsStr ) 
                                           , ("funcbody" , str3 ) ] 
                                           functionTemplate  
      str3 = renderTemplateGroup templates [ ("funcname", "tester" ) 
                                           , ("args", sampleargsStr) ] 
                                           funcbodyTemplate
             

  putStrLn $ mkDeclHeader templates classes

  