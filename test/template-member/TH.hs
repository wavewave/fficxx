{-# LANGUAGE TemplateHaskell #-}
module TH where

import TestPkg.A

import Data.Monoid
import FFICXX.Runtime.TH
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

instance_a_method :: Q Type -> String -> Q [Dec]
instance_a_method qtyp suffix
  = do typ <- qtyp
       f1 <- mkMember ("a_method_" <> suffix) t_method typ suffix
       pure [f1]


t_method :: Type -> String -> Q Exp
t_method typ suffix
  = mkTFunc (typ, suffix, \ n -> "A_method_" <> n, tyf)
  where tyf n
          = let t = pure typ in [t| A -> $( t ) -> IO () |]
