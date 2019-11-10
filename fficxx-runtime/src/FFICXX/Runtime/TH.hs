{-# LANGUAGE TemplateHaskell #-}

module FFICXX.Runtime.TH where

import Data.Monoid                ( (<>) )
import Language.Haskell.TH
import Language.Haskell.TH.Syntax


-- | Primitive C type like int, double should be treated differently than
--   Non-primitive type. The primitive type detection is not yet automatic.
--   So we manually mark template instantiation with this boolean parameter.
data IsCPrimitive = CPrim | NonCPrim


con :: String -> Type
con = ConT . mkNameS


mkInstance :: Cxt -> Type -> [Dec] -> Dec
mkInstance = InstanceD Nothing


mkTFunc :: (Type, String, String -> String, Type -> Q Type) -> Q Exp
mkTFunc (typ, suffix, nf, tyf)
  = do let fn = nf suffix
       let fn' = "c_" <> fn
       n <- newName fn'
       d <- forImpD CCall safe fn n (tyf typ)
       addTopDecls [d]
       [| $( varE n ) |]


mkMember :: String -> (Type -> String -> Q Exp) -> Type -> String -> Q Dec
mkMember fname f typ suffix = do
  let x = mkNameS "x"
  e <- f typ suffix
  pure $
    FunD (mkNameS fname) [ Clause [VarP x] (NormalB (AppE e (VarE x))) [] ]


mkNew :: String -> (Type -> String -> Q Exp) -> Type -> String -> Q Dec
mkNew fname f typ suffix = do
  e <- f typ suffix
  pure $
    FunD (mkNameS fname)
      [ Clause [] (NormalB e) [] ]


mkDelete :: String -> (Type -> String -> Q Exp) -> Type -> String -> Q Dec
mkDelete = mkMember
