{-# LANGUAGE TemplateHaskell #-}
module TH where
import Data.Char
import Data.Monoid
import Foreign.C.Types
import Foreign.Ptr
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import FFICXX.Runtime.TH
import Template


mkTFunc' :: (Type, String, String -> String, Type -> Q Type) -> ExpQ
mkTFunc' (typ, ncty, nf, tyf)
  = do let fn = nf ncty
       let fn' = "c_" <> fn
       n <- newName fn'
       -- let n = mkNameU fn'
       d <- forImpD CCall safe fn n (tyf typ)
       addTopDecls [d]
       [| $( varE n ) |]

mkNew' :: String -> (Type -> String -> Q Exp) -> Type -> String -> Q Dec
mkNew' fname f typ suffix = do
  e <- f typ suffix
  return $
    FunD (mkNameS fname)
      [ Clause [] (NormalB e) [] ]

mkMember' :: String -> (Type -> String -> Q Exp) -> Type -> String -> Q Dec
mkMember' fname f typ suffix = do
  let x = mkNameS "x"
  e <- f typ suffix
  return $
    FunD (mkNameS fname) [ Clause [VarP x] (NormalB (AppE e (VarE x))) [] ]


t_newFunction :: Type -> String -> ExpQ
t_newFunction typ suffix
  = mkTFunc' (typ, suffix, \ n -> "Function_new_" <> n, tyf)
  where tyf n =
          let t = pure typ
          in [t| FunPtr $( t ) -> IO (Function $( t )) |]

t_call :: Type -> String -> ExpQ
t_call typ suffix
  = mkTFunc' (typ, suffix, \ n -> "Function_call_" <> n, tyf)
  where tyf n =
          let t = pure typ
          in [t| Function $( t ) -> $( t ) |]

genFunctionInstanceFor :: Q Type -> String -> Q [Dec]
genFunctionInstanceFor qtyp suffix
  = do typ <- qtyp
       f1 <- mkNew' "newFunction" t_newFunction typ suffix
       f2 <- mkMember' "call" t_call typ suffix
       let lst = [f1,f2]
       return [mkInstance [] (AppT (con "IFunction") typ) lst]
