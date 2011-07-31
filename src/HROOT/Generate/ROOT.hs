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
          , Destructor
          ]


tNamed :: Class
tNamed = Class "TNamed" [tObject] 
         [ Constructor [cstring "name", cstring "title"] 
         , Virtual void_  "SetTitle"        [cstring "name"]  
         , Destructor
         ]


tFormula :: Class
tFormula = Class "TFormula" [] 
           [ Constructor [cstring "name", cstring "formula"] 
           , Virtual double_ "GetParameter"    [int "idx" ] 
           , Virtual void_   "SetParameter"    [int "idx" , double "value"] 
           , Destructor
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
                 , Destructor 
                 ] 

tAttBBox :: Class 
tAttBBox = Class "TAttBBox" [] 
           []

tAttCanvas :: Class
tAttCanvas = Class "TAttCanvas" [] 
             [ Constructor []  
             , Destructor
             ]

tAttFill :: Class
tAttFill = Class "TAttFill" [] 
           [ Constructor [short "fcolor", short "fstyle"] 
           , Virtual void_   "SetFillColor"    [int "color" ] 
           , Virtual void_   "SetFillStyle"    [int "style" ]  
           , Destructor
           ]
tAttImage :: Class
tAttImage = Class "TAttImage" [] 
            []


tAttLine :: Class
tAttLine = Class "TAttLine" [] 
           [ Constructor [short "lcolor", short "lstyle", short "lwidth"] 
           , Virtual void_   "SetLineColor"    [int "color" ] 
           , Destructor
           ] 

tAttMarker :: Class
tAttMarker = Class "TAttMarker" [] 
             [ Constructor [short "color", short "style", short "msize"] 
             , Destructor
             ]  

tAttPad :: Class
tAttPad = Class "TAttPad" []
          [ Constructor [] 
          , Destructor
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
           , Destructor
           ]  


tHStack :: Class
tHStack = Class "THStack" [tNamed] 
          [ Constructor [cstring "name",cstring "title"]  
          , Destructor
          ] 



tF1 :: Class
tF1 = Class "TF1" [tFormula, tAttLine, tAttFill] 
      [ Constructor [cstring "name",cstring "formula",double "xmin",double "xmax"] 
      , Destructor
      ]

tGraph :: Class
tGraph = Class "TGraph" [tNamed, tAttLine, tAttFill, tAttMarker] 
         [ Constructor [int "n", doublep "x", doublep "y"] 
         , Destructor
         ]

tGraphAsymmErrors :: Class
tGraphAsymmErrors = 
  Class "TGraphAsymmErrors" [tGraph]
  [ Constructor [int "n", doublep "x", doublep "y", doublep "exl", doublep "exh", doublep "eyl", doublep "eyh" ]  
  , Destructor
  ]

tCutG :: Class
tCutG = 
  Class "TCutG" [tGraph]
  [ Constructor [cstring "name", int "n", doublep "x", doublep "y"] 
  , Destructor
  ]

tGraphBentErrors :: Class
tGraphBentErrors = 
  Class "TGraphBentErrors" [tGraph]
  [ Constructor [int "n", doublep "x", doublep "y", doublep "exl", doublep "exh", doublep "eyl", doublep "eyh", doublep "exld", doublep "exhd", doublep "eyld", doublep "eyhd"] 
  , Destructor
  ]

tGraphErrors :: Class
tGraphErrors = 
  Class "TGraphErrors" [tGraph]
  [ Constructor [int "n", doublep "x", doublep "y", doublep "ex", doublep "ey"] 
  , Destructor
  ]

tGraphPolar :: Class
tGraphPolar = 
  Class "TGraphPolar" [tGraphErrors]
  [ Constructor [int "n", doublep "x", doublep "y", doublep "ex", doublep "ey"] 
  , Destructor
  ]

tGraphQQ :: Class
tGraphQQ = 
  Class "TGraphQQ" [tGraph]
  [ Constructor [int "nx", doublep "x", int "ny", doublep "y"] 
  , Destructor
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
  , Destructor
  ]

tCrown :: Class
tCrown = 
  Class "TCrown" [tEllipse]
  [ Constructor [double "x1", double "y1", double "radin", double "radout", double "phimin", double "phimax"] 
  , Destructor
  ]

tLine :: Class
tLine = 
  Class "TLine" [tObject, tAttLine] 
  [ Constructor [double "x1", double "y1", double "x2", double "y2" ] 
  , Destructor
  ]            

tArrow :: Class
tArrow = 
  Class "TArrow" [tLine, tAttFill]
  [ Constructor [double "x1", double "y1", double "x2", double "y2", float "arrowsize", cstring "option" ] 
  , Destructor
  ]

tGaxis :: Class 
tGaxis = 
  Class "TGaxis" [tLine, tAttText]
  [ Constructor [double "xmin", double "ymin", double "xmax", double "ymax", double "wmin", double "wmax", int "ndiv", cstring "chopt", double "gridlength" ] 
  , Destructor
  ]

tShape :: Class 
tShape = 
  Class "TShape" [tNamed, tAttLine, tAttFill, tAtt3D]
  [ Constructor [cstring "name", cstring "title", cstring "material" ]  
  , Destructor
  ]

tBRIK :: Class
tBRIK = 
  Class "TBRIK" [tShape]
  [ Constructor [cstring "name", cstring "title", cstring "material", float "dx", float "dy", float "dz" ] 
  , Destructor
  ]
 
tTUBE :: Class
tTUBE = 
  Class "TTUBE" [tShape]
  [ Constructor [cstring "name", cstring "title", cstring "material", float "rmin", float "rmax", float "dz", float "aspect"] 
  , Destructor 
  ]

tPCON :: Class
tPCON = 
  Class "TPCON" [tShape]
  [ Constructor [cstring "name", cstring "title", cstring "material", float "phi1", float "dphi1", int "nz"]
  , Destructor 
  ]

tPolyLineShape :: Class
tPolyLineShape = Class "TPolyLineShape" [tShape, tAttMarker]
                 []

tSPHE :: Class
tSPHE = 
  Class "TSPHE" [tShape]
  [ Constructor [cstring "name", cstring "title", cstring "material", float "rmin", float "rmax", float "themin", float "themax", float "phimin", float "phimax" ]
  , Destructor 
  ]

tXTRU :: Class
tXTRU = 
  Class "TXTRU" [tShape]
  [ Constructor [cstring "name", cstring "title", cstring "material", int "nyx", int "nz" ]
  , Destructor 
  ]

tBox :: Class 
tBox = 
  Class "TBox" [tObject, tAttLine, tAttFill] 
  [ Constructor [double "x1", double "y1", double "x2", double "y2"] 
  , Destructor 
  ] 

tPave :: Class
tPave = 
  Class "TPave" [tBox]
  [ Constructor [double "x1", double "y1", double "x2", double "y2", int "bordersize", cstring "br"]
  , Destructor 
  ]

tPaveText :: Class
tPaveText = 
  Class "TPaveText" [tPave, tAttText]
  [ Constructor [double "x1", double "y1", double "x2", double "y2", cstring "br"] 
  , Destructor
  ]

tDiamond :: Class
tDiamond = 
  Class "TDiamond" [tPaveText] 
  [ Constructor [double "x1", double "y1", double "x2", double "y2"] 
  , Destructor
  ]

tPaveStats :: Class 
tPaveStats = 
  Class "TPaveStats" [tPaveText]
  [ Constructor [double "x1", double "y1", double "x2", double "y2", cstring "br"] 
  , Destructor
  ]

tPavesText :: Class
tPavesText = 
  Class "TPavesText" [tPaveText]
  [ Constructor [double "x1", double "y1", double "x2", double "y2", int "npaves", cstring "br"]
  , Destructor 
  ]

tLegend :: Class 
tLegend = 
  Class "TLegend" [tPave, tAttText]
  [ Constructor [double "x1", double "y1", double "x2", double "y2", cstring "header", cstring "option"] 
  , Destructor
  ]

tPaletteAxis :: Class
tPaletteAxis = 
  Class "TPaletteAxis" [tPave]
  [ Constructor [double "x1", double "y1", double "x2", double "y2", (CPT (CPTClass "TH1") NoConst, "h") ] 
  , Destructor
  ]

tPaveLabel :: Class
tPaveLabel = 
  Class "TPaveLabel" [tPave, tAttText]
  [ Constructor [double "x1", double "y1", double "x2", double "y2", cstring "label", cstring "option"]  
  , Destructor
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
  , Destructor
  ] 

tFrame :: Class 
tFrame = 
  Class "TFrame" [tWbox]
  [ Constructor [double "x1", double "y1", double "x2", double "y2"] 
  , Destructor
  ]

tSliderBox :: Class
tSliderBox = 
  Class "TSliderBox" [tWbox]
  [ Constructor [double "x1", double "y1", double "x2", double "y2", short "color", short "bordersize", short "bordermode"] 
  , Destructor
  ]


tTree :: Class 
tTree = 
  Class "TTree" [tNamed, tAttLine, tAttFill, tAttMarker]
  [ Constructor [cstring "name", cstring "title", int "splitlevel"] 
  , Destructor
  ]

tChain :: Class
tChain = 
  Class "TChain" [tTree]
  [ Constructor [cstring "name", cstring "title"] 
  , Destructor
  ]

tProofChain :: Class
tProofChain = 
  Class "TProofChain" [tChain]
  [ Constructor [ (CPT (CPTClass "TChain") NoConst, "chain"), bool "gettreeheader"] 
  , Destructor
  ]

tHbookTree :: Class 
tHbookTree = 
  Class "THbookTree" [tTree]
  [ Constructor [cstring "name", int "id"] 
  , Destructor
  ]

tNtuple :: Class
tNtuple = 
  Class "TNtuple" [tTree]
  [ Constructor [cstring "name", cstring "title", cstring "varlist", int "bufsize"] 
  , Destructor
  ]

tNtupleD :: Class
tNtupleD = 
  Class "TNtupleD" [tTree]
  [ Constructor [cstring "name", cstring "title", cstring "varlist", int "bufsize"] 
  , Destructor
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
  , Destructor
  ]

tCurlyLine :: Class 
tCurlyLine = 
  Class "TCurlyLine" [tPolyLine]
  [ Constructor [double "x1", double "y1", double "x2", double "y2", double "wl", double "amp"] 
  , Destructor
  ]

tCurlyArc :: Class 
tCurlyArc = 
  Class "TCurlyArc" [tCurlyLine]
  [ Constructor [double "x1", double "y1", double "rad", double "phimin", double "phimax", double "wl", double "amp"] 
  , Destructor
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
         , Destructor
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
        , Destructor
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
      , Destructor
      ] 

tH2 :: Class 
tH2 = Class "TH2" [tH1] 
      [ AliasVirtual int_ "Fill" [double "x", double "y"] "fill2"
      , Destructor
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
       , Destructor 
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
       , Destructor
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
          , Destructor
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
        , Destructor
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










