{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Attoparsec.Text (parseOnly)
import Data.Text (Text)
import FFICXX.Generate.IDL.Parser
  ( class_,
    function,
  )
import Text.Pretty.Simple (pPrint)

sampleIdl1 :: Text
sampleIdl1 = "void func (int a, char b);"

sampleIdl2 :: Text
sampleIdl2 =
  "class A : B {\n\
  \  int method1( double x, double y );\n\
  \};"

main :: IO ()
main = do
  let e1 = parseOnly function sampleIdl1
  pPrint e1
  let e2 = parseOnly class_ sampleIdl2
  pPrint e2
