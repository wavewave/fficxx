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
#include "TAttLine.h"
#include "TAttAxis.h"


#include "TGraph.h"
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