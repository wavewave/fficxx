{-# LANGUAGE TemplateHaskell #-}
module Main where

import TestPkg.A
import TestPkg.A.Implementation
import TestPkg.T1

import TH

$(instance_a_method [t|T1|] "T1")

main :: IO ()
main = do
  a <- newA
  t1 <- newT1
  t1_print t1
  a_method_T1 a t1
  pure ()
