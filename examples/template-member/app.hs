{-# LANGUAGE TemplateHaskell #-}
module Main where

import STD.UniquePtr.Template
import STD.UniquePtr.TH

import TestPkg.A
import TestPkg.A.Implementation
import TestPkg.T1
import TestPkg.T2

$(genUniquePtrInstanceFor [t|T1|] "T1")

$(genInstanceFor_a_method [t|T1|] "T1")
$(genInstanceFor_a_method [t|T2|] "T2")

$(genInstanceFor_a_method2 [t|T1|] "T1")

main :: IO ()
main = do
  a <- newA
  t1 <- newT1
  t1_print t1
  t2 <- newT2
  a_method_T1 a t1
  a_method_T2 a t2

  ptr <- newUniquePtr t1
  a_method2_T1 a ptr
  pure ()
