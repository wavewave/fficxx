{-# LANGUAGE TemplateHaskell #-}

-----------------------------------------------------------------------------
-- |
-- Module      : FFICXX.Runtime.TH
-- Copyright   : (c) 2011-2016 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module FFICXX.Runtime.TH where

import Data.Monoid                ( (<>) )
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-------

con :: String -> Type
con = ConT . mkNameS


mkInstance :: Cxt -> Type -> [Dec] -> Dec
mkInstance = InstanceD Nothing


mkTFunc :: (Type, String, String -> String, Type -> Q Type) -> Q Exp
mkTFunc (typ, suffix, nf, tyf)
  = do let fn = nf suffix
       let fn' = "c_" <> fn
       n <- newName fn'
       let fn'' = "wrap_" <> fn
       n_wrap <- newName fn''
       d <- forImpD CCall safe fn n (tyf typ)
       d_wrap <- forImpD CCall safe "wrapper" n_wrap (pure typ)
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
