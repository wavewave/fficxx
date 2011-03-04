module Main where

import System.IO
import System.Directory
import System.Process
import System.FilePath ((</>))

import Text.StringTemplate
import Text.StringTemplate.Helpers


scriptBaseDir = "/home/wavewave/nfs/workspace/HROOT-generate" 
templateDir   = scriptBaseDir </> "template"
workingDir    = scriptBaseDir </> "working"

definition = "definition.cpp"
function   = "function.cpp" 
funcbody   = "functionbody.cpp"


main :: IO () 
main = do 
  putStrLn "Automatic HROOT binding generation" 
  setCurrentDirectory scriptBaseDir
  
  templates <- directoryGroup templateDir 
  
  let str1 = renderTemplateGroup templates [ ("classname", "TTest") ] 
                                           definition 
      str3 = renderTemplateGroup templates [ ("funcname", "tester" ) 
                                           , ("args", "merong") ] 
                                           funcbody
      str2 = renderTemplateGroup templates [ ("returntype" , "rettype")
                                           , ("funcname" , "tester")
                                           , ("argtypes" , "merong" ) 
                                           , ("funcbody" , str3 ) ] 
                                           function  
    
  putStrLn str1 
  putStrLn str2 
  
  