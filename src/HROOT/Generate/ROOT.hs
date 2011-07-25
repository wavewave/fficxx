module HROOT.Generate.ROOT where

import HROOT.Generate.CType
import HROOT.Generate.Function
import HROOT.Generate.Class

tObject :: Class
tObject = Class "TObject" [] 
          [ Function self_    "New" [] Constructor 
          , Function cstring_ "GetName" [] Ordinary
          , Function void_    "Draw"    [cstring "option"] Ordinary
          , Function void_    "SaveAs"  [cstring "filename", cstring "option"] Ordinary
          , Function int_     "Write"   [cstring "name", int "option", int "bufsize" ] Ordinary
          ]


tNamed :: Class
tNamed = Class "TNamed" [tObject] 
         [ Function self_  "New" [cstring "name", cstring "title"] Constructor
         , Function void_  "SetTitle"        [cstring "name"] Ordinary 
         ]


tFormula :: Class
tFormula = Class "TFormula" [] 
           [ Function self_ "New" [cstring "name", cstring "formula"] Constructor
           , Function double_ "GetParameter"    [int "idx" ] Ordinary
           , Function void_   "SetParameter"    [int "idx" , double "value"] Ordinary
           ]

tAtt3D :: Class
tAtt3D = Class "TAtt3D" []
         []

tAttAxis :: Class
tAttAxis = Class "TAttAxis" [] 
                 [ Function self_ "New" [] Constructor
                 , Function void_   "SetLabelColor"   [int    "color" ] Ordinary
                 , Function void_   "SetLabelSize"    [double "size"  ] Ordinary
                 , Function void_   "SetTickLength"   [double "length" ] Ordinary
                 , Function void_   "SetTitleOffset"  [double "offset" ] Ordinary
                 , Function void_   "SetNdivisions"   [int "n", bool "optim" ] Ordinary
                 ] 

tAttBBox :: Class 
tAttBBox = Class "TAttBBox" [] 
           []

tAttCanvas :: Class
tAttCanvas = Class "TAttCanvas" [] 
             [ Function self_ "New" [] Constructor 
             ]

tAttFill :: Class
tAttFill = Class "TAttFill" [] 
           [ Function self_ "New" [short "fcolor", short "fstyle"] Constructor
           , Function void_   "SetFillColor"    [int "color" ] Ordinary
           , Function void_   "SetFillStyle"    [int "style" ] Ordinary 
           ]
tAttImage :: Class
tAttImage = Class "TAttImage" [] 
            []


tAttLine :: Class
tAttLine = Class "TAttLine" [] 
           [ Function self_ "New" [short "lcolor", short "lstyle", short "lwidth"] Constructor
           , Function void_   "SetLineColor"    [int "color" ] Ordinary
           ] 

tAttMarker :: Class
tAttMarker = Class "TAttMarker" [] 
             [ Function self_ "New" [short "color", short "style", short "msize"] Constructor
             ]  

tAttPad :: Class
tAttPad = Class "TAttPad" []
          [ Function self_ "New" [] Constructor
          ]

tAttParticle :: Class
tAttParticle = Class "TAttParticle" [tNamed]
               [] 

tAttText :: Class
tAttText = Class "TAttText" [] 
           [ Function self_ "New" [int "align", float "angle", short "color", short "font", float "tsize" ] Constructor
           , Function void_ "SetTextColor" [int "tcolor"] Ordinary 
           , Function void_ "SetTextAlign" [int "align"] Ordinary 
           , Function void_ "SetTextSize"  [double "tsize"] Ordinary 
           ]  


tHStack :: Class
tHStack = Class "THStack" [tNamed] 
          [ Function self_ "New" [cstring "name",cstring "title"]  Constructor
          ] 



tF1 :: Class
tF1 = Class "TF1" [tFormula, tAttLine, tAttFill] 
      [ Function self_ "New" [cstring "name",cstring "formula",double "xmin",double "xmax"] Constructor
      ]

tGraph :: Class
tGraph = Class "TGraph" [tNamed, tAttLine, tAttFill, tAttMarker] 
         [ Function self_ "New" [int "n", doublep "x", doublep "y"] Constructor
         ]

tGraphAsymmErrors :: Class
tGraphAsymmErrors = 
  Class "TGraphAsymmErrors" [tGraph]
  [ Function self_ "New" [int "n", doublep "x", doublep "y", doublep "exl", doublep "exh", doublep "eyl", doublep "eyh" ] Constructor 
  ]

tCutG :: Class
tCutG = 
  Class "TCutG" [tGraph]
  [ Function self_ "New" [cstring "name", int "n", doublep "x", doublep "y"] Constructor
  ]

tGraphBentErrors :: Class
tGraphBentErrors = 
  Class "TGraphBentErrors" [tGraph]
  [ Function self_ "New" [int "n", doublep "x", doublep "y", doublep "exl", doublep "exh", doublep "eyl", doublep "eyh", doublep "exld", doublep "exhd", doublep "eyld", doublep "eyhd"] Constructor
  ]

tGraphErrors :: Class
tGraphErrors = 
  Class "TGraphErrors" [tGraph]
  [ Function self_ "New" [int "n", doublep "x", doublep "y", doublep "ex", doublep "ey"] Constructor
  ]

tGraphPolar :: Class
tGraphPolar = 
  Class "TGraphPolar" [tGraphErrors]
  [ Function self_ "New" [int "n", doublep "x", doublep "y", doublep "ex", doublep "ey"] Constructor
  ]

tGraphQQ :: Class
tGraphQQ = 
  Class "TGraphQQ" [tGraph]
  [ Function self_ "New" [int "nx", doublep "x", int "ny", doublep "y"] Constructor
  ]

tEllipse :: Class
tEllipse = 
  Class "TEllipse" [tObject, tAttLine, tAttFill]
  [ Function self_ "New" [double "x1", double "y1", double "r1", double "r2", double "phimin", double "phimax", double "theta" ] Constructor
  ]

tArc :: Class 
tArc = 
  Class "TArc" [tEllipse]
  [ Function self_ "New" [double "x1", double "y1", double "radius", double "phimin", double "phimax" ] Constructor
  ]

tCrown :: Class
tCrown = 
  Class "TCrown" [tEllipse]
  [ Function self_ "New" [double "x1", double "y1", double "radin", double "radout", double "phimin", double "phimax"] Constructor
  ]

tLine :: Class
tLine = 
  Class "TLine" [tObject, tAttLine] 
  [ Function self_ "New" [double "x1", double "y1", double "x2", double "y2" ] Constructor
  ]            

tArrow :: Class
tArrow = 
  Class "TArrow" [tLine, tAttFill]
  [ Function self_ "New" [double "x1", double "y1", double "x2", double "y2", float "arrowsize", cstring "option" ] Constructor
  ]

tGaxis :: Class 
tGaxis = 
  Class "TGaxis" [tLine, tAttText]
  [ Function self_ "New" [double "xmin", double "ymin", double "xmax", double "ymax", double "wmin", double "wmax", int "ndiv", cstring "chopt", double "gridlength" ] Constructor
  ]

tShape :: Class 
tShape = 
  Class "TShape" [tNamed, tAttLine, tAttFill, tAtt3D]
  [ Function self_ "New" [cstring "name", cstring "title", cstring "material" ] Constructor 
  ]

tBRIK :: Class
tBRIK = 
  Class "TBRIK" [tShape]
  [ Function self_ "New" [cstring "name", cstring "title", cstring "material", float "dx", float "dy", float "dz" ] Constructor
  ]
 
tTUBE :: Class
tTUBE = 
  Class "TTUBE" [tShape]
  [ Function self_ "New" [cstring "name", cstring "title", cstring "material", float "rmin", float "rmax", float "dz", float "aspect"]  Constructor
  ]

tPCON :: Class
tPCON = 
  Class "TPCON" [tShape]
  [ Function self_ "New" [cstring "name", cstring "title", cstring "material", float "phi1", float "dphi1", int "nz"] Constructor
  ]

tPolyLineShape :: Class
tPolyLineShape = Class "TPolyLineShape" [tShape, tAttMarker]
                 []

tSPHE :: Class
tSPHE = 
  Class "TSPHE" [tShape]
  [ Function self_ "New" [cstring "name", cstring "title", cstring "material", float "rmin", float "rmax", float "themin", float "themax", float "phimin", float "phimax" ] Constructor
  ]

tXTRU :: Class
tXTRU = 
  Class "TXTRU" [tShape]
  [ Function self_ "New" [cstring "name", cstring "title", cstring "material", int "nyx", int "nz" ] Constructor
  ]

tBox :: Class 
tBox = 
  Class "TBox" [tObject, tAttLine, tAttFill] 
  [ Function self_ "New" [double "x1", double "y1", double "x2", double "y2"] Constructor 
  ] 

tPave :: Class
tPave = 
  Class "TPave" [tBox]
  [ Function self_ "New" [double "x1", double "y1", double "x2", double "y2", int "bordersize", cstring "br"] Constructor
  ]

tPaveText :: Class
tPaveText = 
  Class "TPaveText" [tPave, tAttText]
  [ Function self_ "New" [double "x1", double "y1", double "x2", double "y2", cstring "br"] Constructor
  ]

tDiamond :: Class
tDiamond = 
  Class "TDiamond" [tPaveText] 
  [ Function self_ "New" [double "x1", double "y1", double "x2", double "y2"] Constructor
  ]

tPaveStats :: Class 
tPaveStats = 
  Class "TPaveStats" [tPaveText]
  [ Function self_ "New" [double "x1", double "y1", double "x2", double "y2", cstring "br"] Constructor
  ]

tPavesText :: Class
tPavesText = 
  Class "TPavesText" [tPaveText]
  [ Function self_ "New" [double "x1", double "y1", double "x2", double "y2", int "npaves", cstring "br"] Constructor
  ]

tLegend :: Class 
tLegend = 
  Class "TLegend" [tPave, tAttText]
  [ Function self_ "New" [double "x1", double "y1", double "x2", double "y2", cstring "header", cstring "option"] Constructor
  ]

tPaletteAxis :: Class
tPaletteAxis = 
  Class "TPaletteAxis" [tPave]
  [ Function self_ "New" [double "x1", double "y1", double "x2", double "y2", (CPT (CPTClass "TH1") NoConst, "h") ] Constructor
  ]

tPaveLabel :: Class
tPaveLabel = 
  Class "TPaveLabel" [tPave, tAttText]
  [ Function self_ "New" [double "x1", double "y1", double "x2", double "y2", cstring "label", cstring "option"] Constructor 
  ]

tPaveClass :: Class
tPaveClass = 
  Class "TPaveClass" [tPaveLabel]
  [ ]

tWbox :: Class
tWbox = 
  Class "TWbox" [tBox] 
  [ Function self_ "New" [double "x1", double "y1", double "x2", double "y2", short "color", short "bordersize", short "bordermode"] Constructor 
  , Function void_ "SetBorderMode"   [short "bordermode" ] Ordinary
  ] 

tFrame :: Class 
tFrame = 
  Class "TFrame" [tWbox]
  [ Function self_ "New" [double "x1", double "y1", double "x2", double "y2"] Constructor
  ]

tSliderBox :: Class
tSliderBox = 
  Class "TSliderBox" [tWbox]
  [ Function self_ "New" [double "x1", double "y1", double "x2", double "y2", short "color", short "bordersize", short "bordermode"] Constructor
  ]


tTree :: Class 
tTree = 
  Class "TTree" [tNamed, tAttLine, tAttFill, tAttMarker]
  [ Function self_ "New" [cstring "name", cstring "title", int "splitlevel"] Constructor
  ]

tChain :: Class
tChain = 
  Class "TChain" [tTree]
  [ Function self_ "New" [cstring "name", cstring "title"] Constructor
  ]

tProofChain :: Class
tProofChain = 
  Class "TProofChain" [tChain]
  [ Function self_ "New" [ (CPT (CPTClass "TChain") NoConst, "chain"), bool "gettreeheader"] Constructor
  ]

tHbookTree :: Class 
tHbookTree = 
  Class "THbookTree" [tTree]
  [ Function self_ "New" [cstring "name", int "id"] Constructor
  ]

tNtuple :: Class
tNtuple = 
  Class "TNtuple" [tTree]
  [ Function self_ "New" [cstring "name", cstring "title", cstring "varlist", int "bufsize"] Constructor
  ]

tNtupleD :: Class
tNtupleD = 
  Class "TNtupleD" [tTree]
  [ Function self_ "New" [cstring "name", cstring "title", cstring "varlist", int "bufsize"] Constructor
  ]

tTreeSQL :: Class
tTreeSQL = 
  Class "TTreeSQL" [tTree]
  [
  ]

tPolyLine :: Class 
tPolyLine = 
  Class "TPolyLine" [tObject, tAttLine, tAttFill]
  [ Function self_ "New" [int "n", doublep "x", doublep "y", cstring "option"] Constructor
  ]

tCurlyLine :: Class 
tCurlyLine = 
  Class "TCurlyLine" [tPolyLine]
  [ Function self_ "New" [double "x1", double "y1", double "x2", double "y2", double "wl", double "amp"] Constructor
  ]

tCurlyArc :: Class 
tCurlyArc = 
  Class "TCurlyArc" [tCurlyLine]
  [ Function self_ "New" [double "x1", double "y1", double "rad", double "phimin", double "phimax", double "wl", double "amp"] Constructor
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
         [ Function self_ "New"       [double "x", double "y", cstring "text"] Constructor
         , Function self_ "DrawLatex" [double "x", double "y", cstring "text"] (NonVirtual "drawLatex")
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
      [ Function (cppclass "TAxis") "GetXaxis" [] Ordinary
      , Function (cppclass "TAxis") "GetYaxis" [] Ordinary
      , Function (cppclass "TAxis") "GetZaxis" [] Ordinary
      , Function void_ "Add" [ (CPT (CPTClass "TH1") NoConst, "h1"), double "c1" ] Ordinary 
      , Function int_  "Fill" [double "x"] (Alias "fill1")
      ] 

tH2 :: Class 
tH2 = Class "TH2" [tH1] 
      [ Function int_ "Fill" [double "x", double "y"] (Alias "fill2") 
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
       [ Function self_ "New" [cstring "name",cstring "title",int "nbinsx",double "xlow",double "xup"] Constructor
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
       [ Function self_ "New" [cstring "name",cstring "title",int "nbinsx",double "xlow",double "xup"
                              ,int "nbinsy", double "ylow", double "yup"] Constructor
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
              [ Function (cppclass "TFrame") "GetFrame" [] Ordinary
              , Function void_ "Range" [double "x1", double "y1", double "x2", double "y2"] Ordinary
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
          [ Function self_ "New" [cstring "name",cstring "title",int "ww",int "wh"] Constructor
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
               [ Function self_ "New"    [ cstring "appClassName", intp "argc", charpp "argv"  ] Constructor
               , Function void_ "Run"    [ bool "retrn"] Ordinary  
               ]

tRint :: Class
tRint = Class "TRint" [tApplication]
        [ Function self_ "New"    [ cstring "appClassName", intp "argc", charpp "argv"  ] Constructor ] 






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










