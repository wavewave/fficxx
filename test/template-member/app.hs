{-# LANGUAGE TemplateHaskell #-}
module Main where

import TestPkg.A
import TestPkg.A.Implementation
import TestPkg.T1
import TestPkg.T2

$(genInstanceFor_a_method [t|T1|] "T1")
$(genInstanceFor_a_method [t|T2|] "T2")

main :: IO ()
main = do
  a <- newA
  t1 <- newT1
  t1_print t1
  t2 <- newT2
  a_method_T1 a t1
  a_method_T2 a t2
  pure ()
