{-# LANGUAGE TemplateHaskell #-}

module STL.TH where

import Data.Char
import Foreign.C.Types
import Foreign.Ptr
import Language.Haskell.TH
import Language.Haskell.TH.Syntax



import STL

mkTFunc :: (Name,Name->String,Name-> Q Type) -> Q Exp  
mkTFunc (nty,nf,tyf) = do
  let fn = nf nty
  n <- newName fn
  d <- forImpD CCall unsafe fn n (tyf nty)
  addTopDecls [d]
  [|$(varE n)|]

std_namefun str nty =
  let n:ns = nameBase nty
  in "w_" ++ str ++ "_" ++ (map toLower ns)

vector_printout :: Name -> Q Exp
vector_printout nty = mkTFunc (nty,nf,tyf)
  where nf = std_namefun "printout" 
        tyf n = [t|Ptr (RawSTLVector $(return (ConT n))) -> IO ()|]

vector_new :: Name -> Q Exp
vector_new nty = mkTFunc (nty,nf,tyf)
  where nf = std_namefun "new"
        tyf n = [t|IO (Ptr (RawSTLVector $(return (ConT n)))) |]

vector_push_back :: Name -> Q Exp
vector_push_back nty = mkTFunc (nty,nf,tyf)
  where nf = std_namefun "push_back"
        tyf n = let ty = return (ConT n) in [t|Ptr (RawSTLVector $(ty)) -> $(ty) -> IO () |]

vector_at :: Name -> Q Exp
vector_at nty = mkTFunc (nty,std_namefun "at",tyf)
  where tyf n = let ty = return (ConT n)
                in [t|Ptr (RawSTLVector $(ty)) -> CInt -> IO (Ptr $(ty)) |] 

vector_delete :: Name -> Q Exp
vector_delete nty = mkTFunc (nty,nf,tyf)
  where nf = std_namefun "delete"
        tyf n = [t|Ptr (RawSTLVector $(return (ConT n))) -> IO ()|]


-------

con = ConT . mkNameS

mkInstance = InstanceD Nothing


mkMember :: String -> String -> (Name -> Q Exp) -> Name -> Q Dec
mkMember tname fname f n = do
  let x = mkNameS "x"
  e <- f n
  return $
    FunD (mkNameS fname) [ Clause [ConP (mkNameS tname) [VarP x]] (NormalB (AppE e (VarE x))) [] ]

mkNew :: String -> String -> (Name -> Q Exp) -> Name -> Q Dec
mkNew tname fname f n = do
  e <- f n
  return $
    FunD (mkNameS fname)
      [ Clause [] (NormalB (VarE (mkNameS "fmap") `AppE` ConE (mkNameS tname) `AppE` e)) [] ]


createInstancesFor :: Name -> Q [Dec]
createInstancesFor n = do
  f1 <- mkMember "STLVector" "printout"  vector_printout  n
  f2 <- mkNew    "STLVector" "new"       vector_new       n
  f3 <- mkMember "STLVector" "push_back" vector_push_back n
  f4 <- mkMember "STLVector" "at"        vector_at        n
  f5 <- mkMember "STLVector" "delete"    vector_delete    n

  return [ mkInstance [] (AppT (con "ISTLVector") (ConT n)) [ f1, f2, f3, f4, f5 ] ]

