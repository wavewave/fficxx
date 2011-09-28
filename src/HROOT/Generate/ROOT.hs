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

module HROOT.Generate.ROOT where

import HROOT.Generate.Type.CType
import HROOT.Generate.Type.Method
import HROOT.Generate.Type.Class
import HROOT.Generate.Type.Module


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
tObject = Class "TObject" [deletable] 
          [ Constructor [] 
          , Virtual  cstring_ "GetName" [] 
          , Virtual void_    "Draw"    [cstring "option"] 
          , Virtual (cppclass_ "TObject") "FindObject" [cstring "name"]
          , Virtual void_    "SaveAs"  [cstring "filename", cstring "option"] 
          , Virtual int_     "Write"   [cstring "name", int "option", int "bufsize" ]
          , Virtual (cppclass_ "TClass") "IsA" [] 
          ]


tNamed :: Class
tNamed = Class "TNamed" [tObject] 
         [ Constructor [cstring "name", cstring "title"] 
         , Virtual void_  "SetTitle"        [cstring "name"]  
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
           , Virtual double_ "GetParameter"    [int "idx" ] 
           , Virtual void_   "SetParameter"    [int "idx" , double "value"] 
           ]

tAtt3D :: Class
tAtt3D = Class "TAtt3D" [deletable]
         []

tAttAxis :: Class
tAttAxis = Class "TAttAxis" [deletable] 
                 [ Constructor [] 
                 , Virtual void_   "SetLabelColor"   [int    "color" ] 
                 , Virtual void_   "SetLabelSize"    [double "size"  ] 
                 , Virtual void_   "SetTickLength"   [double "length" ] 
                 , Virtual void_   "SetTitleOffset"  [double "offset" ] 
                 , Virtual void_   "SetNdivisions"   [int "n", bool "optim" ]
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
tAttLine = Class "TAttLine" [deletable] 
           [ Constructor [short "lcolor", short "lstyle", short "lwidth"] 
           , Virtual void_   "SetLineColor"    [int "color" ] 
           ] 

tAttMarker :: Class
tAttMarker = Class "TAttMarker" [deletable] 
             [ Constructor [short "color", short "style", short "msize"] 
             ]  

tAttPad :: Class
tAttPad = Class "TAttPad" [deletable]
          [ Constructor [] 
          ]

tAttParticle :: Class
tAttParticle = Class "TAttParticle" [tNamed]
               [] 

tAttText :: Class
tAttText = Class "TAttText" [deletable] 
           [ Constructor [int "align", float "angle", short "color", short "font", float "tsize" ] 
           , Virtual void_ "SetTextColor" [int "tcolor"]  
           , Virtual void_ "SetTextAlign" [int "align"]  
           , Virtual void_ "SetTextSize"  [double "tsize"]  
           ]  


tHStack :: Class
tHStack = Class "THStack" [tNamed] 
          [ Constructor [cstring "name",cstring "title"]  
          ] 



tF1 :: Class
tF1 = Class "TF1" [tFormula, tAttLine, tAttFill] 
      [ Constructor [cstring "name",cstring "formula",double "xmin",double "xmax"] 
      ]

tGraph :: Class
tGraph = Class "TGraph" [tNamed, tAttLine, tAttFill, tAttMarker] 
         [ Constructor [int "n", doublep "x", doublep "y"] 
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
  ]

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
  -- , Virtual double_ "GetBinCenter" [int "bin"] 

  ]
 
           
tText :: Class
tText = Class "TText" [tNamed, tAttText]
        [] 


tLatex :: Class
tLatex = Class "TLatex" [tText, tAttLine] 
         [ Constructor       [double "x", double "y", cstring "text"] 
         , NonVirtual self_ "DrawLatex" [double "x", double "y", cstring "text"]
         ]


tDirectory :: Class
tDirectory = Class "TDirectory" [tNamed] 
             [ Virtual void_ "Close"    [ cstring "option" ]  
             , Virtual (cppclass_ "TObject") "Get" [ cstring "namecycle" ] 
             ]

tDirectoryFile :: Class
tDirectoryFile = Class "TDirectoryFile" [tDirectory] 
                 []

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
  , Virtual int_ "DistancetoPrimitive" [int "px", int "py"]
  , Virtual void_ "Divide" [cppclass "TH1" "h1", cppclass "TH2" "h2", double "c1", double "c2", cstring "option"]
  , Virtual self_ "DrawCopy" [cstring "option"]
  , Virtual (cppclass_ "TH1") "DrawNormalized" [cstring "option", double "norm"]
  , Virtual void_ "DrawPanel" []
  , Virtual int_ "BufferEmpty" [int "action"]
  , Virtual void_ "Eval" [cppclass "TF1" "f1", cstring "option"]
  , Virtual void_ "ExecuteEvent" [int "event", int "px", int "py"]
  , Virtual (cppclass_ "TH1") "FFT" [cppclass "TH1" "h_output", cstring "option"] 

  , AliasVirtual int_  "Fill" [double "x"] "fill1"
  , Virtual void_ "FillN" [int "ntimes", doublep "x", doublep "w", int "stride"]
  , Virtual void_ "FillRandom" [cppclass "TH1" "h", int "ntimes"] 
  , Virtual int_ "FindBin" [double "x", double "y", double "z"] 
  , Virtual int_ "FindFixBin" [double "x", double "y", double "z"]
  , Virtual int_ "FindFirstBinAbove" [double "threshold", int "axis"] 
  , Virtual int_ "FindLastBinAbove" [double "threshold", int "axis"]  
  -- Fit
  , Virtual void_ "FitPanel" [] 
  , NonVirtual self_ "GetAsymmetry" [cppclass "TH1" "h2", double "c2", double "dc2"]
  , NonVirtual int_ "GetBufferLength" [] 
  , NonVirtual int_ "GetBufferSize" [] 
  -- GetBuffer
  -- GetDefaultBufferSize (static)
  -- GetIntegral
  -- GetListOfFunctions
  , Virtual int_ "GetNdivisions" [cstring "axis"]
  , Virtual short_ "GetAxisColor" [cstring "axis"]
  , Virtual short_ "GetLabelColor" [cstring "axis"]
  , Virtual short_ "GetLabelFont" [cstring "axis"]
  , Virtual float_ "GetLabelOffset" [cstring "axis"]
  , Virtual float_ "GetLabelSize" [cstring "axis"]
  , Virtual short_ "GetTitleFont" [cstring "axis"]
  , Virtual float_ "GetTitleOffset" [cstring "axis"]
  , Virtual float_ "GetTitleSize" [cstring "axis"]
  , Virtual float_ "GetTickLength" [cstring "axis"]
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
  -- GetDefaultSumw2
  , NonVirtual (cppclass_ "TDirectory") "GetDirectory" [] 
  -- omit...
  , NonVirtual (cppclass_ "TAxis") "GetXaxis" [] 
  , NonVirtual (cppclass_ "TAxis") "GetYaxis" [] 
  , NonVirtual (cppclass_ "TAxis") "GetZaxis" []

  ] 

tH2 :: Class 
tH2 = 
  Class "TH2" [tH1] 
  [ --  NonVirtual (cppclass_ "TProfile") "ProfileX" [cstring "name", int "firstybin", int "lastybin", cstring "option" ] 
   -- , NonVirtual (cppclass_ "TProfile") "ProfileY" [cstring "name", int "firstxbin", int "lastxbin", cstring "option" ] 
    NonVirtual (cppclass_ "TH1D") "ProjectionX" [cstring "name", int "firstybin", int "lastybin", cstring "option" ]
  , NonVirtual (cppclass_ "TH1D") "ProjectionY" [cstring "name", int "firstxbin", int "lastxbin", cstring "option" ] 
  , AliasVirtual int_ "Fill" [double "x", double "y"] "fill2"
  ]


tH3 :: Class
tH3 = Class "TH3" [tH1, tAtt3D]
      []


tH1C :: Class 
tH1C = Class "TH1C" [tH1, tArrayC]
       []
 
tH1D :: Class
tH1D = Class "TH1D" [tH1, tArrayD]
       []

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
              [ Virtual (cppclass_ "TFrame") "GetFrame" [] 
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
  , tBox, tPave, tPaveText, tDiamond, tPaveStats, tPavesText, tLegend
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
  ]

