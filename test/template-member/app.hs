module Main where

import TestPkg.A
import TestPkg.T1

main :: IO ()
main = do
  a <- newA
  t1 <- newT1
  t1_print t1
  a_methodT1 a t1
  pure ()
