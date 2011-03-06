module ROOT where

import CType
import Function
import Class

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

self_ = SelfType

tH1F     = Class "TH1F" [tObject, tNamed, tAttLine, tAttFill, tAttHaveAxis] 
           [ Function self_ "New" [cstring "name",cstring "title",int "nbinsx",double "xlow",double "xup"] 
           ] 

tH2F     = Class "TH2F" [tObject, tNamed ] 
           [ Function self_ "New" [cstring "name",cstring "title",int "nbinsx",double "xlow",double "xup"
                                  ,int "nbinsy", double "ylow", double "yup"]
           ]

tHStack  = Class "THStack" [tObject, tNamed ] 
           [ Function self_ "New" [cstring "name",cstring "title"] 
           ] 

tCanvas  = Class "TCanvas" [tObject, tNamed ] 
           [ Function self_ "New" [cstring "name",cstring "title",int "ww",int "wh"]
           ] 

tF1      = Class "TF1" [tObject, tNamed, tFormula] 
           [ Function self_ "New" [cstring "name",cstring "formula",double "xmin",double "xmax"]
           ]

tGraph   = Class "TGraph" [tObject, tNamed, tAttLine, tAttFill ] 
           [ Function self_ "New" [int "n", double "x", double "y"]
           ]

tAxis    = Class "TAxis" [tObject, tNamed, tAttAxis ] []
 

root_abstract_classes = [ tObject, tNamed, tFormula, tAttLine, tAttFill, tWBox, tAttAxis, tAttHaveAxis ] 

root_concrete_classes = [ tH1F, tH2F, tHStack, tCanvas, tF1, tGraph, tAxis ]

root_all_classes = root_abstract_classes ++ root_concrete_classes 