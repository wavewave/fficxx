module Main where

import TestPkg.A
import TestPkg.T1

main :: IO ()
main = do
  a <- newA
  t1 <- newT1

  pure ()
