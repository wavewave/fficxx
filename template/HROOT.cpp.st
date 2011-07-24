#include <iostream>

#include "$headerfilename$"

#include "TMath.h"
#include "TFile.h"
#include "TStyle.h"
#include "TLatex.h"
#include "TTree.h"
#include "TChain.h"
#include "THStack.h"

#include "TNamed.h"

#include "TAtt3D.h"
#include "TAttAxis.h"
#include "TAttBBox.h"
#include "TAttCanvas.h"
#include "TAttFill.h"
#include "TAttImage.h"
#include "TAttLine.h"
#include "TAttMarker.h"
#include "TAttPad.h"
#include "TAttParticle.h"
#include "TAttText.h"

#include "TGraph.h"
#include "TGraphAsymmErrors.h"
#include "TCutG.h"
#include "TGraphBentErrors.h"
#include "TGraphErrors.h"
#include "TGraphPolar.h"
#include "TGraphQQ.h"

#include "TEllipse.h"
#include "TArc.h"
#include "TCrown.h"
#include "TLine.h"
#include "TArrow.h"
#include "TGaxis.h"
#include "TShape.h"
#include "TBRIK.h"
#include "TTUBE.h"
#include "TPCON.h"
#include "TPolyLineShape.h"
#include "TSPHE.h"
#include "TXTRU.h"
#include "TBox.h"
#include "TPave.h"
#include "TPaveText.h"
#include "TDiamond.h"
#include "TPaveStats.h"
#include "TPavesText.h"
#include "TLegend.h"
#include "TPaletteAxis.h"
#include "TPaveLabel.h"
#include "TPaveClass.h"
#include "TWbox.h"
#include "TFrame.h"
#include "TSliderBox.h"




#include "TLorentzVector.h"
#include "TStyle.h" 
#include "TF1.h"
#include "TAxis.h"

#include "TLine.h"
#include "TWbox.h"

#include "TArray.h"
#include "TArrayC.h"
#include "TArrayD.h"
#include "TArrayF.h"
#include "TArrayI.h"
#include "TArrayL.h"
#include "TArrayL64.h"

#include "TH1.h"
#include "TH2.h"
#include "TH3.h"

#include "TH1C.h"
#include "TH1D.h"
#include "TH1F.h"
#include "TH1I.h"
#include "TH1S.h"

#include "TH2C.h"
#include "TH2D.h"
#include "TH2F.h"
#include "TH2I.h"
#include "TH2Poly.h"
//#include "TH2S"

#include "TH3C.h"
#include "TH3D.h"
#include "TH3F.h"
#include "TH3I.h"
#include "TH3S.h"

#include "TQObject.h"
#include "TVirtualPad.h"
#include "TPad.h"
#include "TButton.h"
#include "TGroupButton.h"
#include "TCanvas.h"
#include "TDialogCanvas.h"
#include "TInspectCanvas.h"
#include "TEvePad.h"
#include "TSlider.h"



#include "TTreePlayer.h"
#include "TApplication.h"
#include "TRint.h"

using namespace std;


template<class ToType, class FromType>
const ToType* to_const(const FromType* x) {
  return reinterpret_cast<const ToType*>(x);
}

template<class ToType, class FromType>
ToType* to_nonconst(FromType* x) {
  return reinterpret_cast<ToType*>(x);
}



$cppbody$