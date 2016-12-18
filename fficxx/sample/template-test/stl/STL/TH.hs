{-# LANGUAGE TemplateHaskell #-}

module STL.TH where

import Data.Char
import Foreign.C.Types
import Foreign.Ptr
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
--
import FFICXX.Generate.Util.TH
--
import STL
  

std_namefun str nty = "w_" ++ str ++ "_" ++ nty

vector_printout :: Name -> String -> Q Exp
vector_printout nty ctyp = mkTFunc (nty,ctyp,nf,tyf)
  where nf = std_namefun "printout" 
        tyf n = [t|Ptr (RawSTLVector $(return (ConT n))) -> IO ()|]

vector_new :: Name -> String -> Q Exp
vector_new nty ctyp = mkTFunc (nty,ctyp, nf,tyf)
  where nf = std_namefun "new"
        tyf n = [t|IO (Ptr (RawSTLVector $(return (ConT n)))) |]

vector_push_back :: Name -> String -> Q Exp
vector_push_back nty ctyp = mkTFunc (nty,ctyp,nf,tyf)
  where nf = std_namefun "push_back"
        tyf n = let ty = return (ConT n) in [t|Ptr (RawSTLVector $(ty)) -> $(ty) -> IO () |]

vector_at :: Name -> String -> Q Exp
vector_at nty ctyp = mkTFunc (nty,ctyp,std_namefun "at",tyf)
  where tyf n = let ty = return (ConT n)
                in [t|Ptr (RawSTLVector $(ty)) -> CInt -> IO (Ptr $(ty)) |] 

vector_delete :: Name -> String -> Q Exp
vector_delete nty ctyp = mkTFunc (nty,ctyp,nf,tyf)
  where nf = std_namefun "delete"
        tyf n = [t|Ptr (RawSTLVector $(return (ConT n))) -> IO ()|]


genVectorInstanceFor :: Name -> String -> Q [Dec]
genVectorInstanceFor n ctyp = do
  f1 <- mkMember "STLVector" "printout"  vector_printout  n ctyp
  f2 <- mkNew    "STLVector" "new"       vector_new       n ctyp
  f3 <- mkMember "STLVector" "push_back" vector_push_back n ctyp 
  f4 <- mkMember "STLVector" "at"        vector_at        n ctyp 
  f5 <- mkMember "STLVector" "delete"    vector_delete    n ctyp

  return [ mkInstance [] (AppT (con "ISTLVector") (ConT n)) [ f1, f2, f3, f4, f5 ] ]

