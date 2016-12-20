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
        tyf n = [t|STLVector $(return (ConT n)) -> IO ()|]

vector_new :: Name -> String -> Q Exp
vector_new nty ctyp = mkTFunc (nty,ctyp, nf,tyf)
  where nf = std_namefun "new"
        tyf n = [t|IO (STLVector $(return (ConT n))) |]

vector_push_back :: Name -> String -> Q Exp
vector_push_back nty ctyp = mkTFunc (nty,ctyp,nf,tyf)
  where nf = std_namefun "push_back"
        tyf n = let ty = return (ConT n) in [t|STLVector $(ty) -> $(ty) -> IO () |]

vector_at :: Name -> String -> Q Exp
vector_at nty ctyp = mkTFunc (nty,ctyp,std_namefun "at",tyf)
  where tyf n = let ty = return (ConT n)
                in [t|STLVector $(ty) -> CInt -> IO $(ty) |] 

vector_delete :: Name -> String -> Q Exp
vector_delete nty ctyp = mkTFunc (nty,ctyp,nf,tyf)
  where nf = std_namefun "delete"
        tyf n = [t|STLVector $(return (ConT n)) -> IO ()|]


genVectorInstanceFor :: Name -> String -> Q [Dec]
genVectorInstanceFor n ctyp = do
  f1 <- mkMember "printout"  vector_printout  n ctyp
  f2 <- mkNew    "new"       vector_new       n ctyp
  f3 <- mkMember "push_back" vector_push_back n ctyp 
  f4 <- mkMember "at"        vector_at        n ctyp 
  f5 <- mkMember "delete"    vector_delete    n ctyp

  return [ mkInstance [] (AppT (con "ISTLVector") (ConT n)) [ f1, f2, f3, f4, f5 ] ]

