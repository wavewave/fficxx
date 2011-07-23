module HROOT.Generate.ROOT where

import HROOT.Generate.CType
import HROOT.Generate.Function
import HROOT.Generate.Class

tObject :: Class
tObject = Class "TObject" [] 
                 [ Function cstring_ "GetName" [] Ordinary
                 , Function void_    "Draw"    [cstring "option"] Ordinary
                 , Function int_     "Write"   [cstring "name", int "option", int "bufsize" ] Ordinary
                 ]


tNamed :: Class
tNamed = Class "TNamed" [tObject] 
                [ Function void_   "SetTitle"        [cstring "name"] Ordinary 
                , Function void_   "SaveAs"          [cstring "filename", cstring "option"] Ordinary
                ]


tFormula :: Class
tFormula = Class "TFormula" [] 
                 [ Function double_ "GetParameter"    [int "idx" ] Ordinary
                 , Function void_   "SetParameter"    [int "idx" , double "value"] Ordinary
                 ]

tAttLine :: Class
tAttLine = Class "TAttLine" [] 
                 [ Function void_   "SetLineColor"    [int "color" ] Ordinary
                 ] 

tAttFill :: Class
tAttFill = Class "TAttFill" [] 
                 [ Function void_   "SetFillColor"    [int "color" ] Ordinary
                 , Function void_   "SetFillStyle"    [int "style" ] Ordinary 
                 ]

tWbox :: Class
tWbox    = Class "TWbox" [] 
                 [ Function void_   "SetBorderMode"   [int "bordermode" ] Ordinary
                 ] 

tAttAxis :: Class
tAttAxis = Class "TAttAxis" [] 
                 [ Function void_   "SetLabelColor"   [int    "color" ] Ordinary
                 , Function void_   "SetLabelSize"    [double "size"  ] Ordinary
                 , Function void_   "SetTickLength"   [double "length" ] Ordinary
                 , Function void_   "SetTitleOffset"  [double "offset" ] Ordinary
                 , Function void_   "SetNdivisions"   [int "n", bool "optim" ] Ordinary
                 ] 
 
tH1 :: Class
tH1 = Class "TH1" [tNamed, tAttLine, tAttFill] 
      [ Function (cppclass "TAxis") "GetXaxis" [] Ordinary
      , Function (cppclass "TAxis") "GetYaxis" [] Ordinary
      , Function (cppclass "TAxis") "GetZaxis" [] Ordinary
      , Function void_ "Add" [ (CPT (CPTClass "TH1") NoConst, "h1"), double "c1" ] Ordinary 
      , Function int_  "Fill" [double "x"] (Alias "fill1")
      ] 

tH1F :: Class
tH1F = Class "TH1F" [tH1] 
       [ Function self_ "New" [cstring "name",cstring "title",int "nbinsx",double "xlow",double "xup"] Constructor
       ] 
           
tH2 :: Class 
tH2 = Class "TH2" [tH1] 
      [ Function int_ "Fill" [double "x", double "y"] (Alias "fill2") 
      ]

tH2F :: Class
tH2F = Class "TH2F" [tH1] 
       [ Function self_ "New" [cstring "name",cstring "title",int "nbinsx",double "xlow",double "xup"
                              ,int "nbinsy", double "ylow", double "yup"] Constructor
       ]

tHStack :: Class
tHStack = Class "THStack" [tNamed] 
          [ Function self_ "New" [cstring "name",cstring "title"]  Constructor
          ] 

tCanvas :: Class
tCanvas = Class "TCanvas" [tObject, tNamed, tAttFill, tWbox  ] 
          [ Function self_ "New" [cstring "name",cstring "title",int "ww",int "wh"] Constructor
          ] 

tF1 :: Class
tF1 = Class "TF1" [tFormula, tAttLine, tAttFill] 
      [ Function self_ "New" [cstring "name",cstring "formula",double "xmin",double "xmax"] Constructor
      ]

tGraph :: Class
tGraph = Class "TGraph" [tNamed, tAttLine, tAttFill] 
         [ Function self_ "New" [int "n", doublep "x", doublep "y"] Constructor
         ]

tAxis :: Class
tAxis = Class "TAxis" [tNamed, tAttAxis] 
        []
 
tLine :: Class
tLine = Class "TLine" [tObject, tAttLine] 
        [ Function self_ "New" [double "x1", double "y1", double "x2", double "y2" ] Ordinary
        ]            
           
tText :: Class
tText = Class "TText" [tNamed, tAttText]
        [] 

tAttText :: Class
tAttText = Class "TAttText" [] 
           [ Function void_ "SetTextColor" [int "tcolor"] Ordinary 
           , Function void_ "SetTextAlign" [int "align"] Ordinary 
           , Function void_ "SetTextSize"  [double "tsize"] Ordinary 
           ]  

tLatex :: Class
tLatex = Class "TLatex" [tText, tAttLine] 
         [ Function self_ "New"       [double "x", double "y", cstring "text"] Constructor
         , Function self_ "DrawLatex" [double "x", double "y", cstring "text"] (NonVirtual "drawLatex")
         ]

tApplication :: Class
tApplication = Class "TApplication" [tObject] 
               [ Function self_ "New"    [ cstring "appClassName", intp "argc", charpp "argv"  ] Constructor
               , Function void_ "Run"    [] Ordinary  
               ]

tDirectory :: Class
tDirectory = Class "TDirectory" [tNamed] 
             [ Function void_ "Close"    [ cstring "option" ] Ordinary ]

tDirectoryFile :: Class
tDirectoryFile = Class "TDirectoryFile" [tDirectory] 
                 []

tFile :: Class
tFile = Class "TFile" [tDirectoryFile] 
        [ Function self_ "New" [cstring "fname", cstring "option", cstring "ftitle", int "compress" ] Constructor
        ]

root_all_classes :: [Class]
root_all_classes = [ tObject, tNamed, tFormula, tAttLine, tAttFill, tWbox, tAttAxis
                   , tAttText, tH1F, tH2F, tHStack, tCanvas, tF1, tGraph
                   , tAxis, tLine, tLatex, tH1, tH2, tApplication, tText
                   , tDirectory, tDirectoryFile, tFile ]
