{-# LANGUAGE QuasiQuotes #-}

module HROOT.Generate.ROOTAnnotate where

import HROOT.Generate.Type.Annotate 
import HROOT.Generate.QQ.Verbatim


import qualified Data.Map as M

{-
annotateMap :: AnnotateMap 
annotateMap = M.empty 
-}

annotateMap :: AnnotateMap 
annotateMap = M.fromList [ tNamedAnn, tNamedNewAnn, tNamedSetTitleAnn
                         , tObjectGetNameAnn, tObjectDrawAnn, tObjectFindObjectAnn
                         , tH1Ann, tH1AddAnn, tH1AddBinContentAnn, tH1Chi2TestAnn
                         , tH1ComputeIntegralAnn, tH1DirectoryAutoAddAnn, tH1DistancetoPrimitiveAnn
                         , tH1DivideAnn, tH1DrawCopyAnn, tH1DrawNormalizedAnn
                         , tH1DrawPanelAnn

                         ] 

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