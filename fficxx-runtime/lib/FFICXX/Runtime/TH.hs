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

mkTFunc :: (Name, String, String -> String, Name -> Q Type) -> ExpQ
mkTFunc (nty, ncty, nf, tyf)
  = do let fn = nf ncty
       let fn' = "c_" <> fn
       n <- newName fn'
       d <- forImpD CCall unsafe fn n (tyf nty)
       addTopDecls [d]
       [| $( varE n ) |]



mkMember :: String -> (Name -> String -> Q Exp) -> Name -> String -> Q Dec
mkMember fname f n ctyp = do
  let x = mkNameS "x"
  e <- f n ctyp
  return $
    FunD (mkNameS fname) [ Clause [VarP x] (NormalB (AppE e (VarE x))) [] ]

mkNew :: String -> (Name -> String -> Q Exp) -> Name -> String -> Q Dec
mkNew fname f n ctyp = do
  e <- f n ctyp
  return $
    FunD (mkNameS fname)
      [ Clause [] (NormalB e) [] ]

mkDelete :: String -> (Name -> String -> Q Exp) -> Name -> String -> Q Dec
mkDelete = mkMember
