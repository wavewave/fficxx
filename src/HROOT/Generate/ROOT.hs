module HROOT.Generate.ROOT where

import HROOT.Generate.CType
import HROOT.Generate.Function
import HROOT.Generate.Class

tObject :: Class
tObject = Class "TObject" [] 
          [ Constructor [] 
          , Virtual  cstring_ "GetName" [] 
          , Virtual void_    "Draw"    [cstring "option"] 
          , Virtual void_    "SaveAs"  [cstring "filename", cstring "option"] 
          , Virtual int_     "Write"   [cstring "name", int "option", int "bufsize" ] 
          ]


tNamed :: Class
tNamed = Class "TNamed" [tObject] 
         [ Constructor [cstring "name", cstring "title"] 
         , Virtual void_  "SetTitle"        [cstring "name"]  
         ]


tFormula :: Class
tFormula = Class "TFormula" [] 
           [ Constructor [cstring "name", cstring "formula"] 
           , Virtual double_ "GetParameter"    [int "idx" ] 
           , Virtual void_   "SetParameter"    [int "idx" , double "value"] 
           ]

tAtt3D :: Class
tAtt3D = Class "TAtt3D" []
         []

tAttAxis :: Class
tAttAxis = Class "TAttAxis" [] 
                 [ Constructor [] 
                 , Virtual void_   "SetLabelColor"   [int    "color" ] 
                 , Virtual void_   "SetLabelSize"    [double "size"  ] 
                 , Virtual void_   "SetTickLength"   [double "length" ] 
                 , Virtual void_   "SetTitleOffset"  [double "offset" ] 
                 , Virtual void_   "SetNdivisions"   [int "n", bool "optim" ] 
                 ] 

tAttBBox :: Class 
tAttBBox = Class "TAttBBox" [] 
           []

tAttCanvas :: Class
tAttCanvas = Class "TAttCanvas" [] 
             [ Constructor []  
             ]

tAttFill :: Class
tAttFill = Class "TAttFill" [] 
           [ Constructor [short "fcolor", short "fstyle"] 
           , Virtual void_   "SetFillColor"    [int "color" ] 
           , Virtual void_   "SetFillStyle"    [int "style" ]  
           ]
tAttImage :: Class
tAttImage = Class "TAttImage" [] 
            []


tAttLine :: Class
tAttLine = Class "TAttLine" [] 
           [ Constructor [short "lcolor", short "lstyle", short "lwidth"] 
           , Virtual void_   "SetLineColor"    [int "color" ] 
           ] 

tAttMarker :: Class
tAttMarker = Class "TAttMarker" [] 
             [ Constructor [short "color", short "style", short "msize"] 
             ]  

tAttPad :: Class
tAttPad = Class "TAttPad" []
          [ Constructor [] 
          ]

tAttParticle :: Class
tAttParticle = Class "TAttParticle" [tNamed]
               [] 

tAttText :: Class
tAttText = Class "TAttText" [] 
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

tPolyLineShape :: Class
tPolyLineShape = Class "TPolyLineShape" [tShape, tAttMarker]
                 []

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
  [ Constructor [double "x1", double "y1", double "x2", double "y2", (CPT (CPTClass "TH1") NoConst, "h") ] 
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
  [ Constructor [ (CPT (CPTClass "TChain") NoConst, "chain"), bool "gettreeheader"] 
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
tAxis = Class "TAxis" [tNamed, tAttAxis] 
        []
 
           
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
             [ Virtual void_ "Close"    [ cstring "option" ]  ]

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
tArray = Class "TArray" [] 
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
tH1 = Class "TH1" [tNamed, tAttLine, tAttFill, tAttMarker] 
      [ Virtual (cppclass "TAxis") "GetXaxis" [] 
      , Virtual (cppclass "TAxis") "GetYaxis" [] 
      , Virtual (cppclass "TAxis") "GetZaxis" [] 
      , Virtual void_ "Add" [ (CPT (CPTClass "TH1") NoConst, "h1"), double "c1" ]  
      , AliasVirtual int_  "Fill" [double "x"] "fill1"
      ] 

tH2 :: Class 
tH2 = Class "TH2" [tH1] 
      [ AliasVirtual int_ "Fill" [double "x", double "y"] "fill2"
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
       []

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
tQObject = Class "TQObject" [] 
           []

tVirtualPad :: Class
tVirtualPad = Class "TVirtualPad" [tObject, tAttLine, tAttFill, tAttPad, tQObject]
              [ Virtual (cppclass "TFrame") "GetFrame" [] 
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



root_all_classes :: [Class]
root_all_classes = 
  [ tObject, tNamed, tFormula
  , tAtt3D, tAttAxis, tAttBBox, tAttCanvas, tAttFill, tAttImage, tAttLine
  , tAttMarker, tAttPad, tAttParticle, tAttText

  , tHStack, tF1
  , tGraph, tGraphAsymmErrors, tCutG, tGraphBentErrors, tGraphErrors, tGraphPolar
  , tGraphQQ
  , tEllipse, tArc, tCrown
  , tLine, tArrow, tGaxis
  , tShape, tBRIK, tTUBE, tPCON, tPolyLineShape, tSPHE, tXTRU
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
  ]










