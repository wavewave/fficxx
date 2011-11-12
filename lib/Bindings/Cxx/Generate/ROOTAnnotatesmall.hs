{-# LANGUAGE QuasiQuotes #-}

module Bindings.Cxx.Generate.ROOTAnnotatesmall where

import Bindings.Cxx.Generate.Type.Annotate 
import Bindings.Cxx.Generate.QQ.Verbatim


import qualified Data.Map as M

{-
annotateMap :: AnnotateMap 
annotateMap = M.empty 
-}

annotateMap :: AnnotateMap 
annotateMap = 
  M.fromList 
  [ tNamedAnn, tNamedNewAnn, tNamedSetTitleAnn
  , tObjectGetNameAnn, tObjectDrawAnn, tObjectFindObjectAnn ]
{-
  , tH1Ann, tH1AddAnn, tH1AddBinContentAnn, tH1Chi2TestAnn
  , tH1ComputeIntegralAnn, tH1DirectoryAutoAddAnn, tH1DistancetoPrimitiveAnn
  , tH1DivideAnn, tH1DrawCopyAnn, tH1DrawNormalizedAnn
  , tH1DrawPanelAnn
  , tH2FillAnn, tH2FillRandomAnn, tH2FindFirstBinAboveAnn, tH2FindLastBinAboveAnn
  , tH2FitSlicesXAnn, tH2FitSlicesYAnn, tH2GetCorrelationFactorAnn, tH2GetCovarianceAnn
  , tH2GetStatsAnn, tH2IntegralAnn, tH2InterpolateAnn, tH2KolmogorovTestAnn
  , tH2ProjectionXAnn, tH2ProjectionYAnn, tH2RebinXAnn, tH2RebinYAnn, tH2Rebin2DAnn
  , tH2PutStats, tH2Reset, tH2SetShowProjectionX, tH2SetShowProjectionY, tH2ShowBackgroundAnn
  , tH2ShowPeaksAnn, tH2SmoothAnn
  , ann_interpolate1, ann_interpolate2, ann_interpolate3
  , ann_tH1IsBinOverflow, ann_tH1IsBinUnderflow, ann_kolmogorovTest, ann_labelsDeflate, ann_labelsInflate
  , ann_labelsOption, ann_multiplyF, ann_multiply, ann_paint, ann_putStats, ann_rebin, ann_rebinAxis
  , ann_rebuild, ann_recursiveRemove, ann_reset, ann_resetStats, ann_scale, ann_setAxisColorA
  , ann_setAxisRange, ann_setBarOffset, ann_setBarWidth, ann_setBinContent1, ann_setBinContent2
  , ann_setBinContent3                  
  ]  -}

tNamedAnn = ((HROOTClass,"TNamed"),[verbatim|
Class TNamed
reference : http://root.cern.ch
|])

tNamedNewAnn = ((HROOTMethod,"newTNamed"),[verbatim|constructor : 

> TNamed( char* name, char* title) 

|] )

tNamedSetTitleAnn = ((HROOTMethod, "setTitle"), [verbatim|SetTitle method

> SetTitle( char* name, char* title ) 

|] )


tObjectGetNameAnn = ((HROOTMethod, "getName"), [verbatim|
> char* TObject::GetName()

|])

tObjectDrawAnn = ((HROOTMethod, "draw"), [verbatim|
> void TObject::Draw( char* option )

|])

tObjectFindObjectAnn = ((HROOTMethod, "findObject"), [verbatim|
> TObject* TObject::FindObject( char* name )

|])

tH1Ann = ((HROOTClass, "TH1"), [verbatim| the TH1 class : the mother class of all histogram classes 

> class TH1 : TNamed, TAttLine, TAttFill, TAttMarker

|])

tH1AddAnn = ((HROOTMethod, "add"), [verbatim| 
> void TH1::Add( TH1* h1, Double_t c1 ) 

|])

tH1AddBinContentAnn = ((HROOTMethod, "addBinContent"), [verbatim|
> void TH1::AddBinContent( Int_t bin, Double_t w )

|])

tH1Chi2TestAnn = ((HROOTMethod, "chi2Test"), [verbatim|
> Double_t TH1::Chi2Test( const TH1* h2, Option_t* option="UU", Double_t* res=0 ) const

|])


tH1ComputeIntegralAnn = ((HROOTMethod, "computeIntegral"), [verbatim|
> Double_t TH1::ComputeIntegral ()

|])

tH1DirectoryAutoAddAnn = ((HROOTMethod, "directoryAutoAdd"), [verbatim|
> void TH1::DirectoryAutoAdd(TDirectory* )

|])

tH1DistancetoPrimitiveAnn = ((HROOTMethod, "distancetoPrimitive"), [verbatim|
> Int_t TH1::DistancetoPrimitive(Int_t px, Int_t py)

|])

tH1DivideAnn = ((HROOTMethod, "divide"), [verbatim|
> void TH1::Divide(const TH1* h1, const TH1* h2, Double_t c1=1, Double_t c2=1, Option_t* option="")

|])

tH1DrawCopyAnn = ((HROOTMethod, "drawCopy"), [verbatim|
> TH1* TH1::DrawCopy (Option_t* option="") const 

|])

tH1DrawNormalizedAnn = ((HROOTMethod, "drawNormalized"), [verbatim|
> TH1* TH1::DrawNormalized (Option_t* option="", Double_t norm=1) const

|])

tH1DrawPanelAnn = ((HROOTMethod, "drawPanel"), [verbatim|
> void TH1::DrawPanel()

|])

tH2FillAnn = ((HROOTMethod, "fill2"), [verbatim|
> Int_t    Fill(Double_t x, Double_t y);
|])

tH2FillRandomAnn = ((HROOTMethod, "fillRandom2"), [verbatim|
> void     FillRandom(TH1 *h, Int_t ntimes=5000);

|])

tH2FindFirstBinAboveAnn = ((HROOTMethod, "findFirstBinAbove2"), [verbatim|
> Int_t    FindFirstBinAbove(Double_t threshold=0, Int_t axis=1) const;

|])

tH2FindLastBinAboveAnn = ((HROOTMethod, "findLastBinAbove2"), [verbatim|
> Int_t    FindLastBinAbove (Double_t threshold=0, Int_t axis=1) const;

|])

tH2FitSlicesXAnn = ((HROOTMethod, "fitSlicesX"), [verbatim|
> void     FitSlicesX(TF1 *f1=0,Int_t firstybin=0, Int_t lastybin=-1, Int_t cut=0, Option_t *option="QNR", TObjArray* arr = 0); // *MENU*

|])

tH2FitSlicesYAnn = ((HROOTMethod, "fitSlicesY"), [verbatim|
> void     FitSlicesY(TF1 *f1=0,Int_t firstxbin=0, Int_t lastxbin=-1, Int_t cut=0, Option_t *option="QNR", TObjArray* arr = 0); // *MENU*

|])

tH2GetCorrelationFactorAnn = ((HROOTMethod, "getCorrelationFactor"), [verbatim|
> Double_t GetCorrelationFactor(Int_t axis1=1,Int_t axis2=2) const;

|])

tH2GetCovarianceAnn = ((HROOTMethod, "getCovariance"), [verbatim|
> Double_t GetCovariance(Int_t axis1=1,Int_t axis2=2) const;

|])

tH2GetStatsAnn = ((HROOTMethod, "getStats"), [verbatim|
> void     GetStats(Double_t *stats) const;

|])

tH2IntegralAnn = ((HROOTMethod, "integral"), [verbatim|
> Double_t Integral(Int_t binx1, Int_t binx2, Int_t biny1, Int_t biny2, Option_t *option="") const;

|])

tH2InterpolateAnn = ((HROOTMethod, "interpolate"), [verbatim|
> Double_t Interpolate(Double_t x, Double_t y, Double_t z);

|])

tH2KolmogorovTestAnn = ((HROOTMethod, "kolmogorovTest"), [verbatim|
> Double_t KolmogorovTest(const TH1 *h2, Option_t *option="") const;

|])

tH2ProjectionXAnn = ((HROOTMethod, "projectionX"), [verbatim|
> TH1D      *ProjectionX(const char *name="_px", Int_t firstybin=0, Int_t lastybin=-1, Option_t *option="") const; // *MENU*

|])

tH2ProjectionYAnn = ((HROOTMethod, "projectionY"), [verbatim|
> TH1D      *ProjectionY(const char *name="_py", Int_t firstxbin=0, Int_t lastxbin=-1, Option_t *option="") const; // *MENU*

|])

tH2RebinXAnn = ((HROOTMethod, "rebinX"), [verbatim|
> TH2     *RebinX(Int_t ngroup=2, const char *newname="");

|])

tH2RebinYAnn = ((HROOTMethod, "rebinY"), [verbatim|
> TH2     *RebinY(Int_t ngroup=2, const char *newname="");  

|])

tH2Rebin2DAnn = ((HROOTMethod, "rebin2D"), [verbatim|
> TH2     *Rebin2D(Int_t nxgroup=2, Int_t nygroup=2, const char *newname="");     

|])

tH2PutStats = ((HROOTMethod, "putStats"), [verbatim|
> void     PutStats(Double_t *stats);

|])

tH2Reset = ((HROOTMethod, "reset"), [verbatim|
> void     Reset(Option_t *option="");

|])

tH2SetShowProjectionX = ((HROOTMethod, "setShowProjectionX"), [verbatim|
> void     SetShowProjectionX(Int_t nbins);  // *MENU*

|])

tH2SetShowProjectionY = ((HROOTMethod, "setShowProjectionY"), [verbatim|
> void     SetShowProjectionY(Int_t nbins);  // *MENU*

|])

tH2ShowBackgroundAnn = ((HROOTMethod, "showBackground"), [verbatim|
> TH1     *ShowBackground(Int_t niter=20, Option_t *option="same");

|])

tH2ShowPeaksAnn = ((HROOTMethod, "showPeaks"), [verbatim|
> Int_t    ShowPeaks(Double_t sigma=2, Option_t *option="", Double_t threshold=0.05); // *MENU*

|])

tH2SmoothAnn = ((HROOTMethod, "smooth"), [verbatim|
> void     Smooth(Int_t ntimes=1, Option_t *option=""); // *MENU*

|])


---

ann_interpolate1 = ((HROOTMethod, "interpolate1"), [verbatim|
> Double_t Interpolate(Double_t x)

|])

ann_interpolate2 = ((HROOTMethod, "interpolate2"), [verbatim|
> Double_t Interpolate(Double_t x, Double_t y)

|])

ann_interpolate3 = ((HROOTMethod, "interpolate3"), [verbatim|
> Double_t Interpolate(Double_t x, Double_t y, Double_t z)

|])

ann_tH1IsBinOverflow = ((HROOTMethod, "tH1IsBinOverflow"), [verbatim|
> Bool_t   TH1::IsBinOverflow(Int_t bin) const

|])

ann_tH1IsBinUnderflow = ((HROOTMethod, "tH1IsBinUnderflow"), [verbatim|
> Bool_t   IsBinUnderflow(Int_t bin) const

|])

ann_kolmogorovTest = ((HROOTMethod, "kolmogorovTest"), [verbatim|
> Double_t KolmogorovTest(const TH1 *h2, Option_t *option="") const

|])

ann_labelsDeflate = ((HROOTMethod, "labelsDeflate"), [verbatim|
> void     LabelsDeflate(Option_t *axis="X")

|])

ann_labelsInflate = ((HROOTMethod, "labelsInflate"), [verbatim|
> void     LabelsInflate(Option_t *axis="X")

|])

ann_labelsOption = ((HROOTMethod, "labelsOption"), [verbatim|
> void     LabelsOption(Option_t *option="h", Option_t *axis="X")

|])

ann_multiplyF = ((HROOTMethod, "multiplyF"), [verbatim|
> void     Multiply(TF1 *h1, Double_t c1=1)

|])

ann_multiply = ((HROOTMethod, "multiply"), [verbatim|
> void     Multiply(const TH1 *h1, const TH1 *h2, Double_t c1=1, Double_t c2=1, Option_t *option=""); // *MENU*

|])


ann_paint = ((HROOTMethod, "paint"), [verbatim|
> void     Paint(Option_t *option="")

|])

ann_putStats = ((HROOTMethod, "putStats"), [verbatim|
> void     PutStats(Double_t *stats)

|])

ann_rebin = ((HROOTMethod, "rebin"), [verbatim|
> TH1     *Rebin(Int_t ngroup=2, const char*newname="", const Double_t *xbins=0);  // *MENU*

|])

ann_rebinAxis = ((HROOTMethod, "rebinAxis"), [verbatim|
> void     RebinAxis(Double_t x, TAxis *axis)

|])

ann_rebuild = ((HROOTMethod, "rebuild"), [verbatim|
> void     Rebuild(Option_t *option="")

|])

ann_recursiveRemove = ((HROOTMethod, "recursiveRemove"), [verbatim|
> void     RecursiveRemove(TObject *obj)

|])

ann_reset = ((HROOTMethod, "reset"), [verbatim|
> void     Reset(Option_t *option="")

|])

ann_resetStats = ((HROOTMethod, "resetStats"), [verbatim|
> void     ResetStats()

|])


ann_scale = ((HROOTMethod, "scale"), [verbatim|
> void     Scale(Double_t c1=1, Option_t *option="")

|])

ann_setAxisColorA = ((HROOTMethod, "setAxisColorA"), [verbatim|
> void     SetAxisColor(Color_t color=1, Option_t *axis="X")

|])

ann_setAxisRange = ((HROOTMethod, "setAxisRange"), [verbatim|
> void     SetAxisRange(Double_t xmin, Double_t xmax, Option_t *axis="X")

|])

ann_setBarOffset = ((HROOTMethod, "setBarOffset"), [verbatim|
> void     SetBarOffset(Float_t offset=0.25)

|])

ann_setBarWidth = ((HROOTMethod, "setBarWidth"), [verbatim|
> void     SetBarWidth(Float_t width=0.5) 

|])

ann_setBinContent1 = ((HROOTMethod, "setBinContent1"), [verbatim|
> void     SetBinContent(Int_t bin, Double_t content)

|])

ann_setBinContent2 = ((HROOTMethod, "setBinContent2"), [verbatim|
> void     SetBinContent(Int_t binx, Int_t biny, Double_t content)

|])

ann_setBinContent3 = ((HROOTMethod, "setBinContent3"), [verbatim|
> void     SetBinContent(Int_t binx, Int_t biny, Int_t binz, Double_t content)

|])

