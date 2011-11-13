-- |
-- Module      : HROOT.Generate.ROOT
-- Copyright   : (c) 2011 Ian-Woo Kim
-- 
-- License     : GPL-3
-- Maintainer  : ianwookim@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- conversion data for ROOT classes 
--

module Bindings.Cxx.Generate.ROOT where

import Bindings.Cxx.Generate.Type.CType
import Bindings.Cxx.Generate.Type.Method
import Bindings.Cxx.Generate.Type.Class
import Bindings.Cxx.Generate.Type.Module


moduleInterface :: Module
moduleInterface = Module { module_name = "HROOT.Class.Interface"
                         , module_exports = [ "IDeletable" 
                                            , "TObject"
                                            , "ITObject"
                                            , "ITNamed" 
                                            , "TNamed" ]
                         } 

deletable :: Class 
deletable = AbstractClass "Deletable" [] 
          [ Destructor ]

tObject :: Class
tObject = 
  Class "TObject" [deletable] 
  [ Constructor [] 
  -- Browse
  -- , Virtual int_ "DistancetoPrimitive" [int "px", int "py"]
  , Virtual void_    "Draw"    [cstring "option"] 
  -- , Virtual void_ "ExecuteEvent" [int "event", int "px", int "py"]
  , Virtual (cppclass_ "TObject") "FindObject" [cstring "name"]
  , Virtual  cstring_ "GetName" [] 
  , Virtual (cppclass_ "TClass") "IsA" [] 
  , Virtual bool_ "IsFolder" [] 
  , Virtual bool_ "IsEqual" [cppclass "TObject" "obj"] 
  , Virtual bool_ "IsSortable" [] 
  , NonVirtual bool_ "IsOnHeap" [] 
  , NonVirtual bool_ "IsZombie" [] 
  -- omit.. 
  , Virtual void_ "Paint" [cstring "option"] 
  , AliasVirtual void_ "Print" [cstring "option"] "printObj"
  , Virtual void_ "RecursiveRemove" [cppclass "TObject" "obj"] 
  , Virtual void_    "SaveAs"  [cstring "filename", cstring "option"] 
  -- SavePrimitive
  , Virtual void_    "UseCurrentStyle" [] 
  , Virtual int_     "Write"   [cstring "name", int "option", int "bufsize" ]
  ]


tNamed :: Class
tNamed = 
  Class "TNamed" [tObject] 
  [ Constructor [cstring "name", cstring "title"] 
  , Virtual void_  "SetName"      [cstring "name"]
  , Virtual void_  "SetNameTitle" [cstring "name", cstring "title"]
  , Virtual void_  "SetTitle"     [cstring "name"]  
  ]

tDictionary :: Class
tDictionary = AbstractClass "TDictionary" [tNamed]
              [
              ]

tClass :: Class
tClass = Class "TClass" [tDictionary]
         [
         ]


tFormula :: Class
tFormula = Class "TFormula" [tNamed] 
           [ Constructor [cstring "name", cstring "formula"] 
           , NonVirtual void_ "Optimize" [] 
           -- Analyze
           -- AnalyzeFunction 
           , Virtual int_ "Compile" [cstring "expression"]
           , Virtual void_ "Clear" [cstring "option"]
           -- DefinedString
           , Virtual double_ "DefinedValue" [int "code"]
           -- DefinedVariable
           , Virtual double_ "Eval" [double "x", double "y", double "z", double "t"]
           , Virtual double_ "EvalParOld" [doublep "x", doublep "params"]
           , Virtual double_ "EvalPar" [doublep "x", doublep "params"]
           -- GetLinearPart
           , Virtual int_ "GetNdim" [] 
           , Virtual int_ "GetNpar" [] 
           , Virtual int_ "GetNumber" [] 
           -- GetExpFormula
           , NonVirtual double_ "GetParameter"    [cstring "name" ] 
           -- GetParameters
           -- GetParName
           , Virtual int_   "GetParNumber" [cstring "name"]
           , Virtual bool_  "IsLinear" [] 
           , Virtual bool_  "IsNormalized" [] 
           -- ProcessLinear
           , Virtual void_  "SetNumber" [int "number"]
           , Virtual void_  "SetParameter" [cstring "name", double "parvalue"]
           , Virtual void_  "SetParameters" [doublep "params"]
           , Virtual void_  "SetParName"  [int "ipar", cstring "name"] 
           , Virtual void_  "SetParNames" [cstring "name0", cstring "name1", cstring "name2"
                                          ,cstring "name3", cstring "name4", cstring "name5"
                                          ,cstring "name6", cstring "name7", cstring "name8"
                                          ,cstring "name9", cstring "name10" ]
           , Virtual void_  "Update" [] 
           -- SetMaxima
           ]

tAtt3D :: Class
tAtt3D = Class "TAtt3D" [deletable]
         []

tAttAxis :: Class
tAttAxis = 
  Class "TAttAxis" [deletable] 
  [ Constructor [] 
  , Virtual int_ "GetNdivisions" [] 
  , Virtual short_ "GetAxisColor" [] 
  , Virtual short_ "GetLabelColor" []
  , Virtual short_ "GetLabelFont" [] 
  , Virtual float_ "GetLabelOffset" [] 
  , Virtual float_ "GetLabelSize" [] 
  , Virtual float_ "GetTitleOffset" [] 
  , Virtual float_ "GetTitleSize" [] 
  , Virtual float_ "GetTickLength" []
  , Virtual short_ "GetTitleFont" [] 
  -- omit..
  , Virtual void_   "SetNdivisions"   [int "n", bool "optim" ]
  , Virtual void_   "SetAxisColor"    [short  "color"]
  , Virtual void_   "SetLabelColor"   [short  "color" ] 
  , Virtual void_   "SetLabelFont"    [short "font"] 
  , Virtual void_   "SetLabelOffset"  [float "offset"] 
  , Virtual void_   "SetLabelSize"    [float  "size"  ] 
  , Virtual void_   "SetTickLength"   [float  "length" ] 
  , Virtual void_   "SetTitleOffset"  [float  "offset" ] 
  , Virtual void_   "SetTitleSize"    [float  "size"]
  , Virtual void_   "SetTitleColor"   [short  "color"]
  , Virtual void_   "SetTitleFont"    [short  "font"]

  ] 

tAttBBox :: Class 
tAttBBox = Class "TAttBBox" [deletable] 
           []

tAttCanvas :: Class
tAttCanvas = Class "TAttCanvas" [deletable] 
             [ Constructor []  
             ]

tAttFill :: Class
tAttFill = Class "TAttFill" [deletable] 
           [ Constructor [short "fcolor", short "fstyle"] 
           , Virtual void_   "SetFillColor"    [int "color" ] 
           , Virtual void_   "SetFillStyle"    [int "style" ]  
           ]
tAttImage :: Class
tAttImage = Class "TAttImage" [deletable] 
            []


tAttLine :: Class
tAttLine = 
  Class "TAttLine" [deletable] 
  [ Constructor [short "lcolor", short "lstyle", short "lwidth"] 
  , NonVirtual int_ "DistancetoLine" [int "px", int "py", double "xp1", double "yp1", double "xp2", double "yp2"]
  , Virtual short_ "GetLineColor" [] 
  , Virtual short_ "GetLineStyle" [] 
  , Virtual short_ "GetLineWidth" [] 
  --   , Virtual void_ "Modify" [] 
  , Virtual void_ "ResetAttLine" [cstring "option"]
  -- SaveLineAttributes
  , Virtual void_ "SetLineAttributes" [] 
  , Virtual void_ "SetLineColor" [short "lcolor" ] 
  , Virtual void_ "SetLineStyle" [short "lstyle" ]
  , Virtual void_ "SetLineWidth" [short "lwidth" ]
  ]


tAttMarker :: Class
tAttMarker = 
  Class "TAttMarker" [deletable] 
  [ Constructor [short "color", short "style", short "msize"] 
  , Virtual short_ "GetMarkerColor" [] 
  , Virtual short_ "GetMarkerStyle" [] 
  , Virtual float_ "GetMarkerSize" []
  -- Modify
  , Virtual void_ "ResetAttMarker" [cstring "option"]
  , Virtual void_ "SetMarkerAttributes" [] 
  , Virtual void_ "SetMarkerColor" [short "tcolor"] 
  , Virtual void_ "SetMarkerStyle" [short "mstyle"]
  , Virtual void_ "SetMarkerSize" [short "msize"] 
  ]  
  

tAttPad :: Class
tAttPad = 
  Class "TAttPad" [deletable]
  [ Constructor [] 
  , NonVirtual float_ "GetBottomMargin" [] 
  , NonVirtual float_ "GetLeftMargin" [] 
  , NonVirtual float_ "GetRightMargin" [] 
  , NonVirtual float_ "GetTopMargin" [] 
  , NonVirtual float_ "GetAfile" [] 
  , NonVirtual float_ "GetXfile" [] 
  , NonVirtual float_ "GetYfile" [] 
  , NonVirtual float_ "GetAstat" [] 
  , NonVirtual float_ "GetXstat" [] 
  , NonVirtual float_ "GetYstat" [] 
  , NonVirtual short_ "GetFrameFillColor" [] 
  , NonVirtual short_ "GetFrameLineColor" [] 
  , NonVirtual short_ "GetFrameFillStyle" [] 
  , NonVirtual short_ "GetFrameLineStyle" [] 
  , NonVirtual short_ "GetFrameLineWidth" [] 
  , NonVirtual short_ "GetFrameBorderSize" [] 
  , NonVirtual short_ "GetFrameBorderMode" [] 
  , Virtual void_ "ResetAttPad" [cstring "option"] 
  , Virtual void_ "SetBottomMargin" [float "bottommargin"] 
  , Virtual void_ "SetLeftMargin" [float "leftmargin"]
  , Virtual void_ "SetRightMargin" [float "rightmargin"]
  , Virtual void_ "SetTopMargin" [float "topmargin"]
  , Virtual void_ "SetMargin" [float "left", float "right", float "bottom", float "top"]
  , Virtual void_ "SetAfile" [float "afile"] 
  , Virtual void_ "SetXfile" [float "xfile"]
  , Virtual void_ "SetYfile" [float "yfile"]
  , Virtual void_ "SetAstat" [float "astat"]
  , Virtual void_ "SetXstat" [float "xstat"]
  , Virtual void_ "SetYstat" [float "ystat"]
  , NonVirtual void_ "SetFrameFillColor" [short "color"]
  , NonVirtual void_ "SetFrameLineColor" [short "color"]
  , NonVirtual void_ "SetFrameFillStyle" [short "styl"]
  , NonVirtual void_ "SetFrameLineStyle" [short "styl"]
  , NonVirtual void_ "SetFrameLineWidth" [short "width"]
  , NonVirtual void_ "SetFrameBorderSize" [short "size"]
  , NonVirtual void_ "SetFrameBorderMode" [int "mode"]

  ]

tAttParticle :: Class
tAttParticle = Class "TAttParticle" [tNamed]
               [] 

tAttText :: Class
tAttText = 
  Class "TAttText" [deletable] 
  [ Constructor [int "align", float "angle", short "color", short "font", float "tsize" ] 
  , Virtual short_ "GetTextAlign" [] 
  , Virtual float_ "GetTextAngle" [] 
  , Virtual short_ "GetTextColor" [] 
  , Virtual short_ "GetTextFont" [] 
  , Virtual float_ "GetTextSize" [] 
  , Virtual void_ "ResetAttText" [cstring "toption"] 
  -- SaveTextAttributes
  , Virtual void_ "SetTextAttributes" [] 
  , Virtual void_ "SetTextAlign" [short "align"]
  , Virtual void_ "SetTextAngle" [float "tangle"]
  , Virtual void_ "SetTextColor" [int "tcolor"]  
  , Virtual void_ "SetTextFont" [short "tfont"]
  , Virtual void_ "SetTextSize"  [float "tsize"]  
  , Virtual void_ "SetTextSizePixels" [int "npixels"]
  ]  


tHStack :: Class
tHStack = Class "THStack" [tNamed] 
          [ Constructor [cstring "name",cstring "title"]  
          ] 



tF1 :: Class
tF1 = 
  Class "TF1" [tFormula, tAttLine, tAttFill, tAttMarker] 
  [ Constructor [cstring "name",cstring "formula",double "xmin",double "xmax"] 
  -- Browse
  , Virtual double_ "Derivative" [double "x", doublep "params", double "epsilon"] 
  , Virtual double_ "Derivative2" [double "x", doublep "params", double "epsilon"] 
  , Virtual double_ "Derivative3" [double "x", doublep "params", double "epsilon"]
  -- DerivativeError
  , AliasVirtual self_ "DrawCopy" [cstring "option"] "drawCopyTF1"
  , Virtual (cppclass_ "TObject") "DrawDerivative" [cstring "option"]
  , Virtual (cppclass_ "TObject") "DrawIntegral" [cstring "option"]
  , Virtual void_ "DrawF1" [cstring "formula", double "xmin", double "xmax", cstring "option"]
  , Virtual void_ "FixParameter" [int "ipar", double "value"] 
  , NonVirtual double_ "GetChisquare" [] 
  , NonVirtual (cppclass_ "TH1")  "GetHistogram" [] 
  , AliasVirtual double_ "GetMaximum" [double "xmin", double "xmax", double "epsilon", double "maxiter", bool "logx"] "getMaximumTF1"
  , AliasVirtual double_ "GetMinimum" [double "xmin", double "xmax", double "epsilon", double "maxiter", bool "logx"] "getMinimumTF1"
  , Virtual double_ "GetMaximumX" [double "xmin", double "xmax", double "epsilon", double "maxiter", bool "logx"]
  , Virtual double_ "GetMinimumX" [double "xmin", double "xmax", double "epsilon", double "maxiter", bool "logx"]
  , Virtual int_ "GetNDF" [] 
  , Virtual int_ "GetNpx" []
  -- GetMethodCall
  , Virtual int_ "GetNumberFreeParameters" [] 
  , Virtual int_ "GetNumberFitPoints" []
  , NonVirtual (cppclass_ "TObject") "GetParent" [] 
  , Virtual double_ "GetParError" [int "ipar"] 
  -- GetParErrors 
  -- GetParLiits
  , Virtual double_ "GetProb" [] 
  , AliasVirtual int_ "GetQuantiles" [int "nprobSum", doublep "q", doublep "probSum"] "getQuantilesTF1"
  , AliasVirtual double_ "GetRandom" [double "xmin", double "xmax"] "getRandomTF1"
  -- GetRange
  , Virtual double_ "GetSave" [doublep "x"] 
  , Virtual double_ "GetX" [double "y", double "xmin", double "xmax", double "epsilon", int "maxiter"] 
  , Virtual double_ "GetXmin" []
  , Virtual double_ "GetXmax" [] 
  , NonVirtual (cppclass_ "TAxis") "GetXaxis" [] 
  , NonVirtual (cppclass_ "TAxis") "GetYaxis" [] 
  , NonVirtual (cppclass_ "TAxis") "GetZaxis" [] 
  , Virtual double_ "GradientPar" [int "ipar", doublep "x", double "eps"] 
  , Virtual void_ "InitArgs" [doublep "x", doublep "params"] 
  -- InitStandardFunctions
  , AliasVirtual double_ "Integral" [double "a", double "b", doublep "params", double "epsilon"] "IntegralTF1"
  , Virtual double_ "IntegralError" [double "a", double "b", doublep "params", doublep "covmat", double "epsilon"]
  , Virtual double_ "IntegralFast" [int "num", doublep "x", doublep "w", double "a", double "b", doublep "params", double "epsilon"] 
  -- IntegralMultiple
  , Virtual bool_ "IsInside" [doublep "x"]  
  , Virtual void_ "ReleaseParameter" [int "ipar"] 
  , Virtual void_ "SetChisquare" [double "chi2"] 
  -- SetFitResult
  , AliasVirtual void_ "SetMaximum" [double "maximum"] "setMaximumTF1"
  , AliasVirtual void_ "SetMinimum" [double "minimum"] "setMinimumTF1" 
  , Virtual void_ "SetNDF" [int "ndf"] 
  , Virtual void_ "SetNumberFitPoints" [int "npfits"] 
  , Virtual void_ "SetNpx" [int "npx"]
  , Virtual void_ "SetParError" [int "ipar", double "error"] 
  , Virtual void_ "SetParErrors" [doublep "errors"] 
  , Virtual void_ "SetParLimits" [int "ipar", double "parmin", double "parmax"] 
  , Virtual void_ "SetParent" [cppclass "TObject" "parent"] 
  , AliasVirtual void_ "SetRange" [double "xmin", double "xmax"] "setRange1"
  , AliasVirtual void_ "SetRange" [double "xmin", double "xmax", double "ymin", double "ymax"] "setRange2"
  , AliasVirtual void_ "SetRange" [double "xmin", double "xmax", double "ymin", double "ymax", double "zmin", double "zmax"] "setRange3"
  , Virtual void_ "SetSavedPoint" [int "point", double "value"] 

  -- GetCurrent
  -- AbsValue
  -- RejectPoint 
  -- RejectedPoint 
  -- SetCurrent 
  , Virtual double_ "Moment" [double "n", double "a", double "b", doublep "params", double "epsilon"] 
  , Virtual double_ "CentralMoment" [double "n", double "a", double "b", doublep "params", double "epsilon"] 
  , Virtual double_ "Mean" [double "a", double "b", doublep "params", double "epsilon"]
  , Virtual double_ "Variance" [double "a", double "b", doublep "params", double "epsilon"] 
  -- CalcGaussLegendreSamplingPoints
  ]

tGraph :: Class
tGraph = 
  Class "TGraph" [tNamed, tAttLine, tAttFill, tAttMarker] 
  [ Constructor [int "n", doublep "x", doublep "y"] 
  , Virtual void_ "Apply" [cppclass "TF1" "f"] 
  , Virtual double_ "Chisquare" [cppclass "TF1" "f1"]
  -- CompareArg
  -- CompareX
  -- CompareY 
  -- CompareRadius
  -- ComputeRange
  , Virtual void_ "DrawGraph" [int "n", doublep "x", doublep "y", cstring "option"] 
  , AliasVirtual void_ "DrawPanel" [] "drawPanelTGraph"
  -- Eval 
  , Virtual void_ "Expand" [int "newsize", int "step"]
  -- Fit
  , AliasVirtual void_ "FitPanel" [] "FitPanelTGraph"
  , NonVirtual bool_ "GetEditable" [] 
  , NonVirtual (cppclass_ "TF1") "GetFunction" [cstring "name"]
  , NonVirtual (cppclass_ "TH1F") "GetHistogram" [] 
  , NonVirtual (cppclass_ "TList") "GetListOfFunctions" [] 
  , AliasVirtual double_ "GetCorrelationFactor" [] "getCorrelationFactorTGraph"
  , AliasVirtual double_ "GetCovariance" [] "getCovarianceTGraph"
  , AliasVirtual double_ "GetMean" [int "axis"] "getMeanTGraph"
  , AliasVirtual double_ "GetRMS" [int "axis"] "getRMSTGraph"
  , NonVirtual int_ "GetMaxSize" [] 
  , NonVirtual int_ "GetN" []
  , Virtual double_ "GetErrorX" [int "bin"] 
  , Virtual double_ "GetErrorY" [int "bin"]
  , Virtual double_ "GetErrorXhigh" [int "bin"]
  , Virtual double_ "GetErrorXlow" [int "bin"]
  , Virtual double_ "GetErrorYhigh" [int "bin"]
  , Virtual double_ "GetErrorYlow" [int "bin"]
  -- GetX
  -- GetY
  -- GetEX
  -- GetEY
  -- omit.. 
  , NonVirtual double_ "GetMaximum" [] 
  , NonVirtual double_ "GetMinimum" []
  , NonVirtual (cppclass_ "TAxis") "GetXaxis" []
  , NonVirtual (cppclass_ "TAxis") "GetYaxis" [] 
  -- GetPoint
  , Virtual void_ "InitExpo" [double "xmin", double "xmax"]
  , Virtual void_ "InitGaus" [double "xmin", double "xmax"]
  , Virtual void_ "InitPolynom" [double "xmin", double "xmax"]
  , Virtual int_ "InsertPoint" [] 
  , AliasVirtual double_ "Integral" [int "first", int "last"] "integralTGraph"
  , Virtual bool_ "IsEditable" [] 
  , AliasVirtual int_ "IsInside" [double "x", double "y"] "isInsideTGraph"
  , Virtual void_ "LeastSquareFit" [int "m", doublep "a", double "xmin", double "xmax"] 
  -- LeastSquareLinearFit
  , NonVirtual void_ "PaintGraph" [int "npoints", doublep "x", doublep "y", cstring "chopt"] 
  , NonVirtual void_ "PaintGrapHist" [int "npoints", doublep "x", doublep "y", cstring "chopt"] 
  , Virtual void_ "PaintStats" [cppclass "TF1" "fit"] 
  , Virtual int_ "RemovePoint" [int "ipoint"]
  , Virtual void_ "SetEditable" [bool "editable"] 
  , Virtual void_ "SetHistogram" [cppclass "TH1F" "h"] 
  , AliasVirtual void_ "SetMaximum" [double "maximum"] "setMaximumTGraph"
  , AliasVirtual void_ "SetMinimum" [double "minimum"] "setMinimumTGraph"
  , Virtual void_ "Set" [int "n"]
  , Virtual void_ "SetPoint" [int "i", double "x", double "y"] 
  -- Zero
  ]

tGraphAsymmErrors :: Class
tGraphAsymmErrors = 
  Class "TGraphAsymmErrors" [tGraph]
  [ Constructor [int "n", doublep "x", doublep "y", doublep "exl", doublep "exh", doublep "eyl", doublep "eyh" ]  
  ]

tCutG :: Class
tCutG = 
  Class "TCutG" [tGraph]
  [ Constructor [cstring "name", int "n", doublep "x", doublep "y"] 
  ]

tGraphBentErrors :: Class
tGraphBentErrors = 
  Class "TGraphBentErrors" [tGraph]
  [ Constructor [int "n", doublep "x", doublep "y", doublep "exl", doublep "exh", doublep "eyl", doublep "eyh", doublep "exld", doublep "exhd", doublep "eyld", doublep "eyhd"] 
  ]

tGraphErrors :: Class
tGraphErrors = 
  Class "TGraphErrors" [tGraph]
  [ Constructor [int "n", doublep "x", doublep "y", doublep "ex", doublep "ey"] 
  ]

tGraphPolar :: Class
tGraphPolar = 
  Class "TGraphPolar" [tGraphErrors]
  [ Constructor [int "n", doublep "x", doublep "y", doublep "ex", doublep "ey"] 
  ]

tGraphQQ :: Class
tGraphQQ = 
  Class "TGraphQQ" [tGraph]
  [ Constructor [int "nx", doublep "x", int "ny", doublep "y"] 
  ]

tEllipse :: Class
tEllipse = 
  Class "TEllipse" [tObject, tAttLine, tAttFill]
  [ Constructor [double "x1", double "y1", double "r1", double "r2", double "phimin", double "phimax", double "theta" ] 
  ]

tArc :: Class 
tArc = 
  Class "TArc" [tEllipse]
  [ Constructor [double "x1", double "y1", double "radius", double "phimin", double "phimax" ] 
  ]

tCrown :: Class
tCrown = 
  Class "TCrown" [tEllipse]
  [ Constructor [double "x1", double "y1", double "radin", double "radout", double "phimin", double "phimax"] 
  ]

tLine :: Class
tLine = 
  Class "TLine" [tObject, tAttLine] 
  [ Constructor [double "x1", double "y1", double "x2", double "y2" ] 
  , Virtual (cppclass_ "TLine") "DrawLine" [double "x1", double "y1", double "x2", double "y2"]
  , Virtual (cppclass_ "TLine") "DrawLineNDC" [double "x1", double "y1", double "x2", double "y2"]
  , NonVirtual double_ "GetX1" [] 
  , NonVirtual double_ "GetX2" [] 
  , NonVirtual double_ "GetY1" [] 
  , NonVirtual double_ "GetY2" [] 
  , NonVirtual bool_ "IsHorizontal" [] 
  , NonVirtual bool_ "IsVertical" [] 
  , Virtual void_ "PaintLine" [double "x1", double "y1", double "x2", double "y2"] 
  , Virtual void_ "PaintLineNDC" [double "u1", double "v1", double "u2", double "v2"] 
  , NonVirtual void_ "SetHorizontal" [bool "set"] 
  , NonVirtual void_ "SetVertical" [bool "set"] 
  , Virtual void_ "SetX1" [double "x1"]
  , Virtual void_ "SetX2" [double "x2"]
  , Virtual void_ "SetY1" [double "y1"]
  , Virtual void_ "SetY2" [double "y2"]
  ]            

tArrow :: Class
tArrow = 
  Class "TArrow" [tLine, tAttFill]
  [ Constructor [double "x1", double "y1", double "x2", double "y2", float "arrowsize", cstring "option" ] 
  ]

tGaxis :: Class 
tGaxis = 
  Class "TGaxis" [tLine, tAttText]
  [ Constructor [double "xmin", double "ymin", double "xmax", double "ymax", double "wmin", double "wmax", int "ndiv", cstring "chopt", double "gridlength" ] 
  ]

tShape :: Class 
tShape = 
  Class "TShape" [tNamed, tAttLine, tAttFill, tAtt3D]
  [ Constructor [cstring "name", cstring "title", cstring "material" ]  
  ]

tBRIK :: Class
tBRIK = 
  Class "TBRIK" [tShape]
  [ Constructor [cstring "name", cstring "title", cstring "material", float "dx", float "dy", float "dz" ] 
  ]
 
tTUBE :: Class
tTUBE = 
  Class "TTUBE" [tShape]
  [ Constructor [cstring "name", cstring "title", cstring "material", float "rmin", float "rmax", float "dz", float "aspect"] 
  ]

tPCON :: Class
tPCON = 
  Class "TPCON" [tShape]
  [ Constructor [cstring "name", cstring "title", cstring "material", float "phi1", float "dphi1", int "nz"]
  ]

{-
tPolyLineShape :: Class
tPolyLineShape = Class "TPolyLineShape" [tShape, tAttMarker]
                 []
-}

tSPHE :: Class
tSPHE = 
  Class "TSPHE" [tShape]
  [ Constructor [cstring "name", cstring "title", cstring "material", float "rmin", float "rmax", float "themin", float "themax", float "phimin", float "phimax" ]
  ]

tXTRU :: Class
tXTRU = 
  Class "TXTRU" [tShape]
  [ Constructor [cstring "name", cstring "title", cstring "material", int "nyx", int "nz" ]
  ]

tBox :: Class 
tBox = 
  Class "TBox" [tObject, tAttLine, tAttFill] 
  [ Constructor [double "x1", double "y1", double "x2", double "y2"] 
  ] 

tPave :: Class
tPave = 
  Class "TPave" [tBox]
  [ Constructor [double "x1", double "y1", double "x2", double "y2", int "bordersize", cstring "br"]
  ]

tPaveText :: Class
tPaveText = 
  Class "TPaveText" [tPave, tAttText]
  [ Constructor [double "x1", double "y1", double "x2", double "y2", cstring "br"] 
  ]

tDiamond :: Class
tDiamond = 
  Class "TDiamond" [tPaveText] 
  [ Constructor [double "x1", double "y1", double "x2", double "y2"] 
  ]

tPaveStats :: Class 
tPaveStats = 
  Class "TPaveStats" [tPaveText]
  [ Constructor [double "x1", double "y1", double "x2", double "y2", cstring "br"] 
  ]

tPavesText :: Class
tPavesText = 
  Class "TPavesText" [tPaveText]
  [ Constructor [double "x1", double "y1", double "x2", double "y2", int "npaves", cstring "br"]
  ]

tLegend :: Class 
tLegend = 
  Class "TLegend" [tPave, tAttText]
  [ Constructor [double "x1", double "y1", double "x2", double "y2", cstring "header", cstring "option"] 
  , Virtual (cppclass_ "TLegendEntry") "AddEntry" [cppclass "TObject" "obj", cstring "label", cstring "option"] 
  ]


tLegendEntry :: Class
tLegendEntry = 
  Class "TLegendEntry" [tObject, tAttText, tAttLine, tAttFill, tAttMarker] 
  []


tPaletteAxis :: Class
tPaletteAxis = 
  Class "TPaletteAxis" [tPave]
  [ Constructor [double "x1", double "y1", double "x2", double "y2", cppclass "TH1" "h" ] 
  ]

tPaveLabel :: Class
tPaveLabel = 
  Class "TPaveLabel" [tPave, tAttText]
  [ Constructor [double "x1", double "y1", double "x2", double "y2", cstring "label", cstring "option"]  
  ]

tPaveClass :: Class
tPaveClass = 
  Class "TPaveClass" [tPaveLabel]
  [ ]

tWbox :: Class
tWbox = 
  Class "TWbox" [tBox] 
  [ Constructor [double "x1", double "y1", double "x2", double "y2", short "color", short "bordersize", short "bordermode"]  
  , Virtual void_ "SetBorderMode"   [short "bordermode" ] 
  ] 

tFrame :: Class 
tFrame = 
  Class "TFrame" [tWbox]
  [ Constructor [double "x1", double "y1", double "x2", double "y2"] 
  ]

tSliderBox :: Class
tSliderBox = 
  Class "TSliderBox" [tWbox]
  [ Constructor [double "x1", double "y1", double "x2", double "y2", short "color", short "bordersize", short "bordermode"] 
  ]


tTree :: Class 
tTree = 
  Class "TTree" [tNamed, tAttLine, tAttFill, tAttMarker]
  [ Constructor [cstring "name", cstring "title", int "splitlevel"] 
  ]

tChain :: Class
tChain = 
  Class "TChain" [tTree]
  [ Constructor [cstring "name", cstring "title"] 
  ]

tProofChain :: Class
tProofChain = 
  Class "TProofChain" [tChain]
  [ Constructor [ cppclass "TChain" "chain", bool "gettreeheader"] 
  ]

tHbookTree :: Class 
tHbookTree = 
  Class "THbookTree" [tTree]
  [ Constructor [cstring "name", int "id"] 
  ]

tNtuple :: Class
tNtuple = 
  Class "TNtuple" [tTree]
  [ Constructor [cstring "name", cstring "title", cstring "varlist", int "bufsize"] 
  ]

tNtupleD :: Class
tNtupleD = 
  Class "TNtupleD" [tTree]
  [ Constructor [cstring "name", cstring "title", cstring "varlist", int "bufsize"] 
  ]

tTreeSQL :: Class
tTreeSQL = 
  Class "TTreeSQL" [tTree]
  [
  ]

tPolyLine :: Class 
tPolyLine = 
  Class "TPolyLine" [tObject, tAttLine, tAttFill]
  [ Constructor [int "n", doublep "x", doublep "y", cstring "option"] 
  ]

tCurlyLine :: Class 
tCurlyLine = 
  Class "TCurlyLine" [tPolyLine]
  [ Constructor [double "x1", double "y1", double "x2", double "y2", double "wl", double "amp"] 
  ]

tCurlyArc :: Class 
tCurlyArc = 
  Class "TCurlyArc" [tCurlyLine]
  [ Constructor [double "x1", double "y1", double "rad", double "phimin", double "phimax", double "wl", double "amp"] 
  ]

tEfficiency :: Class 
tEfficiency = Class "TEfficiency" [tNamed, tAttLine, tAttFill, tAttMarker]
              []




tAxis :: Class
tAxis = 
  Class "TAxis" [tNamed, tAttAxis] 
  [ Constructor [int "nbins", double "xmin", double "xmax"] 
  , AliasVirtual int_ "FindBin" [double "x"] "findBinTAxis"
  , AliasVirtual int_ "FindFixBin" [double "x"] "findFixBinTAxis"
  , AliasVirtual double_ "GetBinCenter" [int "bin"]  "getBinCenterTAxis"
  , Virtual double_ "GetBinCenterLog" [int "bin"]  
  -- , Virtual double_ "GetBinLabel" [int "bin"]
  , Virtual double_ "GetBinUpEdge" [int "bin"]
  -- GetCenter
  , NonVirtual bool_ "GetCenterLabels" []
  , NonVirtual bool_ "GetCenterTitle" [] 
  -- GetLowEdge
  , Virtual void_ "SetTimeDisplay" [ int "value" ] 
  , Virtual void_ "SetTimeFormat" [ cstring "format" ] 
  , Virtual void_ "SetTimeOffset" [double "toffset", cstring "option"]
  ]
 
           
tText :: Class
tText = 
  Class "TText" [tNamed, tAttText]
  [ Constructor [double "x", double "y", cstring "text"]
  , Virtual (cppclass_ "TText") "DrawText" [double "x", double "y", cstring "text"]
  , Virtual (cppclass_ "TText") "DrawTextNDC" [double "x", double "y", cstring "text"]
  , Virtual void_ "GetControlBox" [int "x", int "y", int "theta", intp "cBoxX", intp "cBoxY" ]
  -- omit..
  , Virtual void_ "SetText" [double "x", double "y", cstring "text"] 
  ] 

tLatex :: Class
tLatex = Class "TLatex" [tText, tAttLine] 
         [ Constructor       [double "x", double "y", cstring "text"] 
         , NonVirtual self_ "DrawLatex" [double "x", double "y", cstring "text"]
         ]


tDirectory :: Class
tDirectory = Class "TDirectory" [tNamed] 
             [ Virtual void_ "Append" [cppclass "TObject" "obj", bool "replace"]
             , AliasVirtual void_ "Add" [cppclass "TObject" "obj", bool "replace"] "addD"
             , Virtual int_ "AppendKey" [cppclass "TKey" "key" ]
             , Virtual void_ "Close"    [ cstring "option" ] 
             , Virtual (cppclass_ "TObject") "Get" [ cstring "namecycle" ] 
             ]

tKey :: Class
tKey = Class "TKey" [tNamed]
       [ 
-- Constructor [cstring "name", cstring "title", cppclass "TClass" "cl", int "nbytes", cppclass "TDirectory" "motherDir"]
       ] 




tDirectoryFile :: Class
tDirectoryFile = 
  Class "TDirectoryFile" [tDirectory] 
  [ Virtual (cppclass_ "TList") "GetListOfKeys" [] 

  ]

tList :: Class
tList = 
  Class "TList" [tSeqCollection] 
  [ 
  ] 

tFile :: Class
tFile = Class "TFile" [tDirectoryFile] 
        [ Constructor [cstring "fname", cstring "option", cstring "ftitle", int "compress" ] 
        ]


tBranch :: Class
tBranch = Class "TBranch" [tNamed, tAttFill]
          []

tVirtualTreePlayer :: Class
tVirtualTreePlayer = Class "TVirtualTreePlayer" [tObject]
                     []

tTreePlayer :: Class
tTreePlayer = Class "TTreePlayer" [tVirtualTreePlayer] 
              []
                 



tArray :: Class
tArray = Class "TArray" [deletable] 
         []

tArrayC :: Class 
tArrayC = Class "TArrayC" [tArray]
          []

tArrayD :: Class
tArrayD = Class "TArrayD" [tArray]
          []

tArrayF :: Class 
tArrayF = Class "TArrayF" [tArray]
          []

tArrayI :: Class
tArrayI = Class "TArrayI" [tArray]
          []

tArrayL :: Class
tArrayL = Class "TArrayL" [tArray]
          []

tArrayL64 :: Class
tArrayL64 = Class "TArrayL64" [tArray]
            []

tArrayS :: Class
tArrayS = Class "TArrayS" [tArray]
          []

tH1 :: Class
tH1 = 
  Class "TH1" [tNamed, tAttLine, tAttFill, tAttMarker] 
  [ Virtual void_ "Add" [cppclass "TH1" "h1", double "c1"]  
  , Virtual void_ "AddBinContent" [int "bin", double "w"] 
  , Virtual double_ "Chi2Test" [cppclass "TH1" "h2", cstring "option", doublep "res"] 
  , Virtual double_ "ComputeIntegral" []
  , Virtual void_ "DirectoryAutoAdd" [cppclass "TDirectory" "dir"]

  , Virtual void_ "Divide" [cppclass "TH1" "h1", cppclass "TH1" "h2", double "c1", double "c2", cstring "option"]
  , AliasVirtual self_ "DrawCopy" [cstring "option"] "drawCopyTH1"
  , Virtual (cppclass_ "TH1") "DrawNormalized" [cstring "option", double "norm"]
  , AliasVirtual void_ "DrawPanel" [] "drawPanelTH1"
  , Virtual int_ "BufferEmpty" [int "action"]
  , AliasVirtual void_ "Eval" [cppclass "TF1" "f1", cstring "option"] "evalF"
  , Virtual (cppclass_ "TH1") "FFT" [cppclass "TH1" "h_output", cstring "option"] 

  , AliasVirtual int_  "Fill" [double "x"] "fill1"
  , AliasVirtual int_  "Fill" [double "x", double "w"] "fill1w"
  , AliasVirtual void_ "FillN" [int "ntimes", doublep "x", doublep "w", int "stride"] "fillN1"
  , Virtual void_ "FillRandom" [cppclass "TH1" "h", int "ntimes"] 
  , Virtual int_ "FindBin" [double "x", double "y", double "z"] 
  , Virtual int_ "FindFixBin" [double "x", double "y", double "z"]
  , Virtual int_ "FindFirstBinAbove" [double "threshold", int "axis"] 
  , Virtual int_ "FindLastBinAbove" [double "threshold", int "axis"]  
  -- Fit
  , AliasVirtual void_ "FitPanel" [] "FitPanelTH1"
  , NonVirtual self_ "GetAsymmetry" [cppclass "TH1" "h2", double "c2", double "dc2"]
  , NonVirtual int_ "GetBufferLength" [] 
  , NonVirtual int_ "GetBufferSize" [] 
  -- GetBuffer
  , Static int_ "GetDefaultBufferSize" []
  -- GetIntegral
  -- GetListOfFunctions
  , AliasVirtual int_ "GetNdivisions" [cstring "axis"] "getNdivisionA"
  , AliasVirtual short_ "GetAxisColor" [cstring "axis"] "getAxisColorA"
  , AliasVirtual short_ "GetLabelColor" [cstring "axis"] "getLabelColorA"
  , AliasVirtual short_ "GetLabelFont" [cstring "axis"] "getLabelFontA"
  , AliasVirtual float_ "GetLabelOffset" [cstring "axis"] "getLabelOffsetA"
  , AliasVirtual float_ "GetLabelSize" [cstring "axis"] "getLabelSizeA"
  , AliasVirtual short_ "GetTitleFont" [cstring "axis"] "getTitleFontA"
  , AliasVirtual float_ "GetTitleOffset" [cstring "axis"] "getTitleOffsetA"
  , AliasVirtual float_ "GetTitleSize" [cstring "axis"] "getTitleSizeA"
  , AliasVirtual float_ "GetTickLength" [cstring "axis"] "getTickLengthA"
  , Virtual float_ "GetBarOffset" []
  , Virtual float_ "GetBarWidth" [] 
  , Virtual int_ "GetContour" [doublep "levels"] 
  , Virtual double_ "GetContourLevel" [int "level"] 
  , Virtual double_ "GetContourLevelPad" [int "level"] 
  , Virtual int_ "GetBin" [int "binx", int "biny", int "binz"]
  -- GetBinXYZ
  , Virtual double_ "GetBinCenter" [int "bin"]
  , AliasVirtual double_ "GetBinContent" [int "binx"] "GetBinContent1"
  , AliasVirtual double_ "GetBinContent" [int "binx", int "biny"] "GetBinContent2"
  , AliasVirtual double_ "GetBinContent" [int "binx", int "biny", int "binz"] "GetBinContent3" 
  , AliasVirtual double_ "GetBinError" [int "binx"] "GetBinError1"
  , AliasVirtual double_ "GetBinError" [int "binx", int "biny"] "GetBinError2"
  , AliasVirtual double_ "GetBinError" [int "binx", int "biny", int "binz"] "GetBinError3" 
  , Virtual double_ "GetBinLowEdge" [int "bin"] 
  , Virtual double_ "GetBinWidth" [int "bin"]
  -- GetBinWidthContent
  , Virtual double_ "GetCellContent" [int "binx", int "biny"] 
  , Virtual double_ "GetCellError" [int "binx", int "biny"]
  -- GetCenter
  , Static  bool_ "GetDefaultSumw2" []
  , NonVirtual (cppclass_ "TDirectory") "GetDirectory" [] 
  , Virtual double_ "GetEntries" []
  , Virtual double_ "GetEffectiveEntries" [] 
  , Virtual (cppclass_ "TF1") "GetFunction" [cstring "name"]
  , Virtual int_ "GetDimension" []
  , Virtual double_ "GetKurtosis" [int "axis"]
  , Virtual void_ "GetLowEdge" [doublep "edge"] 
  , AliasVirtual double_ "GetMaximum" [double "maxval"] "getMaximumTH1"
  , Virtual int_ "GetMaximumBin" []
  , Virtual double_ "GetMaximumStored" []
  , AliasVirtual double_ "GetMinimum" [double "minval"] "getMinimumTH1"
  , Virtual int_ "GetMinimumBin" []
  , Virtual double_ "GetMinimumStored" [] 
  , Virtual double_ "GetMean" [int "axis"]
  , Virtual double_ "GetMeanError" [int "axis"]
  , Virtual double_ "GetNbinsX" [] 
  , Virtual double_ "GetNbinsY" [] 
  , Virtual double_ "GetNbinsZ" [] 
   -- GetObjectInfo
   -- GetOption
  -- , Virtual (cppclass_ "TVirtualHistPainter") "GetPainter" [cstring "option"]
  , AliasVirtual int_ "GetQuantiles" [int "nprobSum", doublep "q", doublep "pbSum"] "getQuantilesTH1"
  , Virtual double_ "GetRandom" []
  , Virtual void_ "GetStats" [doublep "stats"]
  , Virtual double_ "GetSumOfWeights" [] 
  , Virtual (cppclass_ "TArrayD") "GetSumw2" []
  , Virtual int_ "GetSumw2N" [] 
  , Virtual double_ "GetRMS" [int "axis"]
  , Virtual double_ "GetRMSError" [int "axis"] 
  , Virtual double_ "GetSkewness" [int "axis"]
  , NonVirtual (cppclass_ "TAxis") "GetXaxis" [] 
  , NonVirtual (cppclass_ "TAxis") "GetYaxis" [] 
  , NonVirtual (cppclass_ "TAxis") "GetZaxis" []
  , AliasVirtual double_ "Integral" [int "binx1", int "binx2", cstring "option"] "integral1"
  -- IntegralAndError
  , AliasVirtual double_ "Interpolate" [double "x"] "interpolate1"
  , AliasVirtual double_ "Interpolate" [double "x", double "y"] "interpolate2"
  , AliasVirtual double_ "Interpolate" [double "x", double "y", double "z"] "interpolate3"
  , NonVirtual bool_ "IsBinOverflow" [int "bin"]
  , NonVirtual bool_ "IsBinUnderflow" [int "bin"]
  , Virtual double_ "KolmogorovTest" [cppclass "TH1" "h2", cstring "option"]
  , Virtual void_ "LabelsDeflate" [cstring "axis"]
  , Virtual void_ "LabelsInflate" [cstring "axis"]
  , Virtual void_ "LabelsOption" [cstring "option", cstring "axis"]
  , AliasVirtual void_ "Multiply" [cppclass "TF1" "h1", double "c1"] "multiflyF"
  , Virtual void_ "Multiply" [cppclass "TH1" "h1", cppclass "TH1" "h2", double "c1", double "c2", cstring "option"] 
  , Virtual void_ "PutStats" [doublep "stats"]
  , Virtual (cppclass_ "TH1") "Rebin" [int "ngroup", cstring "newname", doublep "xbins"]
  , Virtual void_ "RebinAxis" [double "x", cppclass "TAxis" "axis"]
  , Virtual void_ "Rebuild" [cstring "option"]
  -- , Virtual void_ "RecursiveRemove" [cppclass "TObject" "obj"]
  , Virtual void_ "Reset" [cstring "option"]
  , Virtual void_ "ResetStats" [] 
  , Virtual void_ "Scale" [double "c1", cstring "option"]
  , AliasVirtual void_ "SetAxisColor" [short "color", cstring "axis"] "setAxisColorA"
  , Virtual void_ "SetAxisRange" [double "xmin", double "xmax", cstring "axis"]
  , Virtual void_ "SetBarOffset" [float "offset"]
  , Virtual void_ "SetBarWidth" [float "width"]
  , AliasVirtual void_ "SetBinContent" [int "bin", double "content"] "setBinContent1"
  , AliasVirtual void_ "SetBinContent" [int "binx", int "biny", double "content"] "setBinContent2"
  , AliasVirtual void_ "SetBinContent" [int "binx", int "biny", int "binz", double "content"] "setBinContent3"
  , AliasVirtual void_ "SetBinError" [int "bin", double "error"] "setBinError1"
  , AliasVirtual void_ "SetBinError" [int "binx", int "biny", double "error"] "setBinError2"
  , AliasVirtual void_ "SetBinError" [int "binx", int "biny", int "binz", double "error"] "setBinError3"
  , AliasVirtual void_ "SetBins" [int "nx", doublep "xBins"] "setBins1"
  , AliasVirtual void_ "SetBins" [int "nx", doublep "xBins", int "ny", doublep "yBins"] "setBins2"
  , AliasVirtual void_ "SetBins" [int "nx", doublep "xBins", int "ny", doublep "yBins", int "nz", doublep "zBins"] "setBins3"
  , Virtual void_ "SetBinsLength" [int "bin"]
  , Virtual void_ "SetBuffer" [int "buffersize", cstring "option"]
  , Virtual void_ "SetCellContent" [int "binx", int "biny", double "content"]
  , Virtual void_ "SetContent" [doublep "content"] 
  , Virtual void_ "SetContour" [int "nlevels", doublep "levels"] 
  , Virtual void_ "SetContourLevel" [int "level", double "value"]
  , Static  void_ "SetDefaultBufferSize" [int "buffersize"]
  , Static  void_ "SetDefaultSumw2" [bool "sumw2"]
  -- SetDefaultSumw2
  , Virtual void_ "SetDirectory" [cppclass "TDirectory" "dir"]
  , Virtual void_ "SetEntries" [double "n"]
  , Virtual void_ "SetError" [doublep "error"]
  , AliasVirtual void_ "SetLabelColor" [short "color", cstring "axis"] "setLabelColorA"
  , AliasVirtual void_ "SetLabelSize" [float "size", cstring "axis"] "setLabelSizeA"
  , AliasVirtual void_   "SetLabelFont"    [short "font", cstring "axis"] "setLabelFontA"
  , AliasVirtual void_   "SetLabelOffset"  [float "offset", cstring "axis"] "setLabelOffsetA"
  , Virtual void_ "SetMaximum" [double "maximum"]
  , Virtual void_ "SetMinimum" [double "minimum"]
  , Virtual void_ "SetNormFactor" [double "factor"] 
  , Virtual void_ "SetStats" [bool "stats"] 
  , Virtual void_ "SetOption" [cstring "option"] 
  , Virtual void_ "SetXTitle" [cstring "title"] 
  , Virtual void_ "SetYTitle" [cstring "title"]
  , Virtual void_ "SetZTitle" [cstring "title"]
  , Virtual (cppclass_ "TH1") "ShowBackground" [int "niter", cstring "option"]
  , Virtual int_  "ShowPeaks" [double "sigma", cstring "option", double "threshold" ]
  , Virtual void_ "Smooth" [int "ntimes", cstring "option"] 
  , Static  void_ "SmoothArray" [int "NN", doublep "XX", int "ntimes"]
  , Static  void_ "StatOverflows" [bool "flag"]
  , Virtual void_ "Sumw2" [] 
  , NonVirtual void_ "UseCurrentStyle" [] 
  -- TransformHisto
  ] 

tH2 :: Class 
tH2 = 
  Class "TH2" [tH1] 
  [ AliasVirtual int_ "Fill" [double "x", double "y"] "fill2"
  , AliasVirtual int_ "Fill" [double "x", double "y", double "w"] "fill2w"
  , AliasVirtual void_ "FillN" [int "ntimes", doublep "x",  doublep "y", doublep "w", int "stride"] "fillN2"
  , AliasVirtual void_ "FillRandom" [cppclass "TH1" "h", int "ntimes"] "fillRandom2"
  , AliasVirtual int_  "FindFirstBinAbove" [double "threshold", int "axis"] "findFirstBinAbove2"
  , AliasVirtual int_  "FindLastBinAbove"  [double "threshold", int "axis"] "findLastBinAbove2"
  , Virtual void_ "FitSlicesX" [cppclass "TF1" "f1", int "firstybin", int "lastybin", int "cut", cstring "option", cppclass "TObjArray" "arr"]
  , Virtual void_ "FitSlicesY" [cppclass "TF1" "f1", int "firstxbin", int "lastxbin", int "cut", cstring "option", cppclass "TObjArray" "arr"]
  -- GetBinWithContent2
  , AliasVirtual double_ "GetCorrelationFactor" [int "axis1", int "axis2"] "getCorrelationFactor2"
  , AliasVirtual double_ "GetCovariance" [int "axis1", int "axis2"] "getCovariance2"
  -- GetRandom2
  , AliasVirtual double_ "Integral" [int "binx1", int "binx2", int "biny1", int "biny2", cstring "option"] "integral2"
  , NonVirtual (cppclass_ "TH1D") "ProjectionX" [cstring "name", int "firstybin", int "lastybin", cstring "option" ]
  , NonVirtual (cppclass_ "TH1D") "ProjectionY" [cstring "name", int "firstxbin", int "lastxbin", cstring "option" ] 
  , AliasVirtual (cppclass_ "TH2") "RebinX" [int "ngroup", cstring "newname"] "rebinX2"
  , AliasVirtual (cppclass_ "TH2") "RebinY" [int "ngroup", cstring "newname"] "rebinY2"
  , Virtual (cppclass_ "TH2") "Rebin2D" [int "nxgroup", int "nygroup", cstring "newname"]
  , Virtual void_ "SetShowProjectionX" [int "nbins"]
  , Virtual void_ "SetShowProjectionY" [int "nbins"]
  ]


tH3 :: Class
tH3 = 
  Class "TH3" [tH1, tAtt3D]
  [ AliasVirtual int_ "Fill" [double "x", double "y", double "z"] "fill3"
  , AliasVirtual int_ "Fill" [double "x", double "y", double "z", double "w"] "fill3w"
  , Virtual void_ "FitSlicesZ" [cppclass "TF1" "f1", int "binminx", int "binmaxx", int "binminy", int "binmaxy", int "cut", cstring "option" ] 
  -- GetBinWithContent3
  , AliasVirtual double_ "GetCorrelationFactor" [int "axis1", int "axis2"] "getCorrelationFactor3"
  , AliasVirtual double_ "GetCovariance" [int "axis1", int "axis2"] "getCovariance3"
  -- GetRandom3
  , NonVirtual (cppclass_ "TH1D") "ProjectionX" [cstring "name", int "firstybin", int "lastybin", int "firstzbin", int "lastzbin", cstring "option" ]
  , NonVirtual (cppclass_ "TH1D") "ProjectionY" [cstring "name", int "firstxbin", int "lastxbin", int "firstzbin", int "lastzbin", cstring "option" ]
  , NonVirtual (cppclass_ "TH1D") "ProjectionZ" [cstring "name", int "firstxbin", int "lastxbin", int "firstybin", int "lastybin", cstring "option" ] 
  , NonVirtual (cppclass_ "TH1") "Project3D" [cstring "option"]
  -- Project3DProfile
  , AliasVirtual (cppclass_ "TH3") "RebinX" [int "ngroup", cstring "newname"] "rebinX3"
  , AliasVirtual (cppclass_ "TH3") "RebinY" [int "ngroup", cstring "newname"] "rebinY3"
  , AliasVirtual (cppclass_ "TH3") "RebinZ" [int "ngroup", cstring "newname"] "rebinZ3"
  , Virtual (cppclass_ "TH3") "Rebin3D" [int "nxgroup", int "nygroup", int "nzgroup", cstring "newname"]
  ]


tH1C :: Class 
tH1C = Class "TH1C" [tH1, tArrayC]
       []
 
tH1D :: Class
tH1D = Class "TH1D" [tH1, tArrayD]
       [ Constructor [cstring "name",cstring "title",int "nbinsx",double "xlow",double "xup"]
       ]

tH1F :: Class
tH1F = Class "TH1F" [tH1, tArrayF] 
       [ Constructor [cstring "name",cstring "title",int "nbinsx",double "xlow",double "xup"]
       ] 
           
tH1I :: Class 
tH1I = Class "TH1I" [tH1, tArrayI]
       []

tH1K :: Class
tH1K = Class "TH1K" [tH1, tArrayF]
       []

tH1S :: Class
tH1S = Class "TH1S" [tH1, tArrayS]
       []

tH2C :: Class
tH2C = Class "TH2C" [tH2, tArrayC]
       []

tH2D :: Class 
tH2D = Class "TH2D" [tH2, tArrayD] 
       [ Constructor [ cstring "name",cstring "title",int "nbinsx",double "xlow",double "xup"
                     , int "nbinsy", double "ylow", double "yup"]
       ]

tH2F :: Class
tH2F = Class "TH2F" [tH2, tArrayF] 
       [ Constructor [cstring "name",cstring "title",int "nbinsx",double "xlow",double "xup"
                              ,int "nbinsy", double "ylow", double "yup"] 
       ]

tH2I :: Class
tH2I = Class "TH2I" [tH2, tArrayI]
       []

tH2Poly :: Class 
tH2Poly = Class "TH2Poly" [tH2]
          []

tH2S :: Class
tH2S = Class "TH2S" [tH2, tArrayS]
       []



tH3C :: Class 
tH3C = Class "TH3C" [tH3, tArrayC]
       []

tH3D :: Class
tH3D = Class "TH3D" [tH3, tArrayD] 
       []

tH3F :: Class
tH3F = Class "TH3F" [tH3, tArrayF]
       []

tH3I :: Class
tH3I = Class "TH3I" [tH3, tArrayI]
       []

tH3S :: Class
tH3S = Class "TH3S" [tH3, tArrayS]
       []


tQObject :: Class
tQObject = Class "TQObject" [deletable] 
           []

tVirtualPad :: Class
tVirtualPad = Class "TVirtualPad" [tObject, tAttLine, tAttFill, tAttPad, tQObject]
              [ Virtual (cppclass_ "TVirtualPad") "cd" [int "subpadnumber"]  
              , Virtual (cppclass_ "TFrame") "GetFrame" [] 
              , Virtual void_ "Modified" [bool "flag"]
              , Virtual void_ "Range" [double "x1", double "y1", double "x2", double "y2"] 
              ] 

tPad :: Class 
tPad = Class "TPad" [tVirtualPad] 
       []
 
tButton :: Class
tButton = Class "TButton" [tPad, tAttText]
          []

tGroupButton :: Class
tGroupButton = Class "TGroupButton" [tButton]
               []

tCanvas :: Class
tCanvas = Class "TCanvas" [tPad] 
          [ Constructor [cstring "name",cstring "title",int "ww",int "wh"] 
          ] 

tDialogCanvas :: Class
tDialogCanvas = Class "TDialogCanvas" [tCanvas, tAttText]
                 []

tInspectCanvas :: Class
tInspectCanvas = Class "TInspectCanvas" [tCanvas, tAttText]
                 [] 

tEvePad :: Class
tEvePad = Class "TEvePad" [tPad]
          []

tSlider :: Class 
tSlider = Class "TSlider" [tPad]
          []


tApplication :: Class
tApplication = Class "TApplication" [tObject, tQObject] 
               [ Constructor    [ cstring "appClassName", intp "argc", charpp "argv"  ] 
               , Virtual void_ "Run"    [ bool "retrn"]   
               ]

tRint :: Class
tRint = Class "TRint" [tApplication]
        [ Constructor    [ cstring "appClassName", intp "argc", charpp "argv" ]
        ] 


tRandom :: Class 
tRandom = 
  Class "TRandom" [tNamed]
  [ Constructor [ int "seed" ] 
  , Virtual double_ "Gaus" [double "mean", double "sigma"]
  , Virtual double_ "Uniform" [double "x1", double "x2"]
  ]       

tCollection :: Class
tCollection = 
  Class "TCollection" [tObject] []

tSeqCollection :: Class
tSeqCollection = 
  Class "TSeqCollection" [tCollection] []

tObjArray :: Class 
tObjArray = 
  Class "TObjArray" [tSeqCollection] []

tDatime :: Class 
tDatime = 
  Class "TDatime"  [deletable]
  [ Constructor [int "year", int "month", int "day", int "hour", int "min", int "sec"] 
  , Virtual uint_ "Convert" [bool "toGMT"]  
  , NonVirtual int_ "GetDay" [] 
  , NonVirtual int_ "GetHour" []
  , NonVirtual int_ "GetMinute" [] 
  , NonVirtual int_ "GetSecond" []
  , NonVirtual int_ "GetYear" []
  , NonVirtual int_ "GetMonth" [] 
  , AliasVirtual void_ "Set" [uint "tloc" ] "setTDatime"
  ] 

tVirtualHistPainter :: Class 
tVirtualHistPainter = 
  AbstractClass "TVirtualHistPainter" [tObject] 
  [ ] 


{-
tProfile = 
  Class "TProfile" [tH1D] 
  [ ] 
-}

{-
root_all_classes = [ deletable, tObject, tNamed, tClass, tDictionary 
                   , tCanvas, tAttCanvas, tH1, tH1F, tPad, tAttLine
                   , tAttFill, tAttMarker, tArrayF, tVirtualPad, tArray
                   , tFrame, tAttPad, tQObject, tWbox, tBox, tRandom
                   , tGraph]
-}

root_all_classes :: [Class]
root_all_classes = 
  [ deletable
  , tObject, tNamed, tDictionary, tClass
  , tFormula
  , tAtt3D, tAttAxis, tAttBBox, tAttCanvas, tAttFill, tAttImage, tAttLine
  , tAttMarker, tAttPad, tAttParticle, tAttText

  , tHStack, tF1
  , tGraph, tGraphAsymmErrors, tCutG, tGraphBentErrors, tGraphErrors, tGraphPolar
  , tGraphQQ
  , tEllipse, tArc, tCrown
  , tLine, tArrow, tGaxis
  , tShape, tBRIK, tTUBE, tPCON, tSPHE, tXTRU
  -- , tPolyLineShape
  , tBox, tPave, tPaveText, tDiamond, tPaveStats, tPavesText, tLegend, tLegendEntry
  -- , tPaletteAxis
  , tPaveLabel, tPaveClass, tWbox, tFrame, tSliderBox
  , tTree, tChain
  -- , tProofChain
  -- , tHbookTree
  , tNtuple, tNtupleD, tTreeSQL
  , tPolyLine, tCurlyLine, tCurlyArc, tEfficiency
  , tAxis, tLatex, tText
  , tDirectory, tDirectoryFile, tFile
  , tBranch, tVirtualTreePlayer, tTreePlayer
  , tArray, tArrayC, tArrayD, tArrayF, tArrayI, tArrayL, tArrayL64
  , tArrayS
  , tH1, tH2, tH3
  , tH1C, tH1D, tH1F, tH1I, tH1S
  , tH2C, tH2D, tH2F, tH2I, tH2Poly, tH2S 
  , tH3C, tH3D, tH3F, tH3I, tH3S  
  , tQObject
  , tVirtualPad, tPad, tButton, tGroupButton, tCanvas
  , tDialogCanvas, tInspectCanvas
  , tEvePad, tSlider
  , tApplication, tRint
  , tRandom
  --  , tProfile
  , tCollection, tSeqCollection, tObjArray, tList
  , tKey
  , tDatime 
  , tVirtualHistPainter
  ]

