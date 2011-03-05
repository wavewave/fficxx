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
import Class


sampleargs = [ (CT CTString Const, "a") 
             , (CT CTInt NoConst, "b") ] 
sampleargsStr = argsToString sampleargs


void = Void 

tNamed = Class "TNamed" [tObject] 
                [ Function Void    "SetTitle"        [cstring "name"] 
                , Function Void    "SaveAs"          [cstring "filename", cstring "option"] 
                , Function double_ "GetParameter"    [int     "idx" ] 
                ]

tObject = Class "TObject" [] 
                 [ Function cstring_ "GetName" [] 
                 , Function void     "Draw"    [cstring "option"] ]


classes = [ tObject, tNamed ]


main :: IO () 
main = do 
  putStrLn "Automatic HROOT binding generation" 
  setCurrentDirectory scriptBaseDir
  
  templates <- directoryGroup templateDir 
  
  putStrLn $ mkDeclHeader templates classes

  putStrLn $ "--------"
  
  putStrLn $ mkDefMain templates classes
    
  putStrLn $ mkDaugtherMap classes 
                              
  
  