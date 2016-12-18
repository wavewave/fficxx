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


myTest :: Q [Dec]
myTest = do
  e1 <- vector_printout  (mkNameS "CInt")
  e2 <- vector_new       (mkNameS "CInt")
  e3 <- vector_push_back (mkNameS "CInt")
  e4 <- vector_at        (mkNameS "CInt")
  e5 <- vector_delete    (mkNameS "CInt")

  return
    [
      mkInstance [] (AppT (con "ISTLVector") (con "CInt"))
      [ FunD (mkNameS "printout")
          [ Clause [ConP (mkNameS "STLVector") [VarP (mkNameS "x")]] (NormalB (AppE e1 (VarE (mkNameS "x")))) [] ]
      , FunD (mkNameS "new")
          [ Clause [] (NormalB (VarE (mkNameS "fmap") `AppE` ConE (mkNameS "STLVector") `AppE` e2)) [] ]
      , FunD (mkNameS "push_back")
          [ Clause [ConP (mkNameS "STLVector") [VarP (mkNameS "x")]] (NormalB (AppE e3 (VarE (mkNameS "x")))) [] ]
      , FunD (mkNameS "at")
          [ Clause [ConP (mkNameS "STLVector") [VarP (mkNameS "x")]] (NormalB (AppE e4 (VarE (mkNameS "x")))) [] ]
      , FunD (mkNameS "delete")
          [ Clause [ConP (mkNameS "STLVector") [VarP (mkNameS "x")]] (NormalB (AppE e5 (VarE (mkNameS "x")))) [] ]

        
      ]
    ]

-- STLVector
