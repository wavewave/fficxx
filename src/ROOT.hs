module ROOT where

import CType
import Function
import Class

tNamed = Class "TNamed" [tObject] 
                [ Function void_   "SetTitle"        [cstring "name"] NoExport 
                , Function void_   "SaveAs"          [cstring "filename", cstring "option"] NoExport
                ]

tObject = Class "TObject" [] 
                 [ Function cstring_ "GetName" [] NoExport
                 , Function void_    "Draw"    [cstring "option"] NoExport
                 ]


tFormula = Class "TFormula" [] 
                 [ Function double_ "GetParameter"    [int "idx" ] NoExport
                 , Function void_   "SetParameter"    [int "idx" , double "value"] NoExport
                 ]

tAttLine = Class "TAttLine" [] 
                 [ Function void_   "SetLineColor"    [int "color" ] NoExport
                 ] 

tAttFill = Class "TAttFill" [] 
                 [ Function void_   "SetFillColor"    [int "color" ] NoExport
                 ]

tWBox    = Class "TWBox" [] 
                 [ Function void_   "SetBorderMode"   [int "bordermode" ] NoExport
                 ] 

tAttAxis = Class "TAttAxis" [] 
                 [ Function void_   "SetLabelColor"   [int    "color" ] NoExport
                 , Function void_   "SetLabelSize"    [double "size"  ] NoExport
                 , Function void_   "SetTickLength"   [double "length" ] NoExport
                 , Function void_   "SetTitleOffset"  [double "offset" ] NoExport
                 , Function void_   "SetNdivisions"   [int "n", bool "optim" ] NoExport
                 ] 
 
tAttHaveAxis = Class "TAttHaveAxis" [] 
                 [ Function (cppclass "TAxis") "GetXaxis" [] NoExport
                 , Function (cppclass "TAxis") "GetYaxis" [] NoExport
                 , Function (cppclass "TAxis") "GetZaxis" [] NoExport
                 ] 

self_ = SelfType

tH1F     = Class "TH1F" [tObject, tNamed, tAttLine, tAttFill, tAttHaveAxis] 
           [ Function self_ "New" [cstring "name",cstring "title",int "nbinsx",double "xlow",double "xup"] NoExport
           , Function int_  "Fill" [double "x"] (Alias "fill1")
           ] 

tH2F     = Class "TH2F" [tObject, tNamed ] 
           [ Function self_ "New" [cstring "name",cstring "title",int "nbinsx",double "xlow",double "xup"
                                  ,int "nbinsy", double "ylow", double "yup"] NoExport
           , Function int_  "Fill" [double "x", double "y"] (Alias "fill2") 
           ]

tHStack  = Class "THStack" [tObject, tNamed ] 
           [ Function self_ "New" [cstring "name",cstring "title"]  NoExport
           ] 

tCanvas  = Class "TCanvas" [tObject, tNamed ] 
           [ Function self_ "New" [cstring "name",cstring "title",int "ww",int "wh"] NoExport
           ] 

tF1      = Class "TF1" [tObject, tNamed, tFormula] 
           [ Function self_ "New" [cstring "name",cstring "formula",double "xmin",double "xmax"] NoExport
           ]

tGraph   = Class "TGraph" [tObject, tNamed, tAttLine, tAttFill ] 
           [ Function self_ "New" [int "n", doublep "x", doublep "y"] NoExport
           ]

tAxis    = Class "TAxis" [tObject, tNamed, tAttAxis ] []
 
tLine    = Class "TLine" [tObject, tAttLine] 
           [ Function self_ "New" [double "x1", double "y1", double "x2", double "y2" ] NoExport
           ]            
           

tText    = Class "TText" [tObject, tNamed, tAttText] [] 

tAttText = Class "TAttText" [] 
           [ Function void_ "SetTextColor" [int "tcolor"] NoExport 
           , Function void_ "SetTextAlign" [int "align"] NoExport 
           , Function void_ "SetTextSize"  [double "tsize"] NoExport 
           ]  

tLatex   = Class "TLatex" [tObject, tNamed, tText, tAttText] 
           [ Function self_ "DrawLatex" [double "x", double "y", cstring "text"] (Alias "drawLatex")
           ]


root_abstract_classes = [ tObject, tNamed, tFormula, tAttLine, tAttFill, tWBox, tAttAxis, tAttHaveAxis ] 

root_concrete_classes = [ tH1F, tH2F, tHStack, tCanvas, tF1, tGraph, tAxis ]

root_all_classes = root_abstract_classes ++ root_concrete_classes 