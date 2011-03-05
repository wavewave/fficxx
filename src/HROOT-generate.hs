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


void_ = Void 

tNamed = Class "TNamed" [tObject] 
                [ Function void_   "SetTitle"        [cstring "name"] 
                , Function void_   "SaveAs"          [cstring "filename", cstring "option"] 
                ]

tObject = Class "TObject" [] 
                 [ Function cstring_ "GetName" [] 
                 , Function void_    "Draw"    [cstring "option"] ]


tFormula = Class "TFormula" [] 
                 [ Function double_ "GetParameter"    [int "idx" ] 
                 , Function void_   "SetParameter"    [int "idx" , double "value"]    
                 ]

tAttLine = Class "TAttLine" [] 
                 [ Function void_   "SetLineColor"    [int "color" ]
                 ] 

tAttFill = Class "TAttFill" [] 
                 [ Function void_   "SetFillColor"    [int "color" ]
                 ]

tWBox    = Class "TWBox" [] 
                 [ Function void_   "SetBorderMode"   [int "bordermode" ]
                 ] 

tAttAxis = Class "TAttAxis" [] 
                 [ Function void_   "SetLabelColor"   [int    "color" ]
                 , Function void_   "SetLabelSize"    [double "size"  ]
                 , Function void_   "SetTickLength"   [double "length" ]
                 , Function void_   "SetTitleOffset"  [double "offset" ]
                 , Function void_   "SetNdivisions"   [int "n", bool "optim" ]
                 ] 
 
tAttHaveAxis = Class "TAttHaveAxis" [] 
                 [ Function (cppclass "TAxis") "GetXaxis" [] 
                 , Function (cppclass "TAxis") "GetYaxis" []
                 , Function (cppclass "TAxis") "GetZaxis" [] 
                 ] 


tH1F     = Class "TH1F" [tObject, tNamed, tAttLine, tAttFill, tAttHaveAxis] [] 

tH2F     = Class "TH2F" [tObject, tNamed ] []

tHStack  = Class "THStack" [tObject, tNamed ] [] 

tCanvas  = Class "TCanvas" [tObject, tNamed ] [] 

tF1      = Class "TF1" [tObject, tNamed, tFormula] []

tGraph   = Class "TGraph" [tObject, tNamed, tAttLine, tAttFill ] []

tAxis    = Class "TAxis" [tObject, tNamed, tAttAxis ] [] 

classes = [ tObject, tNamed, tFormula, tAttLine, tAttFill, tWBox, tAttAxis, tAttHaveAxis 
          , tH1F, tH2F, tHStack, tCanvas, tF1, tGraph, tAxis ]


main :: IO () 
main = do 
  putStrLn "Automatic HROOT binding generation" 
  setCurrentDirectory scriptBaseDir
  
  templates <- directoryGroup templateDir 
  
  putStrLn $ mkDeclHeader templates classes

  putStrLn $ "--------"
  
  putStrLn $ mkDefMain templates classes
    
  putStrLn $ show $ mkDaughterMap classes 
                              
  
  