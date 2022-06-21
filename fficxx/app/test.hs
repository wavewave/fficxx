{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Attoparsec.Text (parseOnly)
import Data.Text (Text)
import FFICXX.Generate.IDL.Parser
  ( class_,
    function,
    package,
  )
import Text.Pretty.Simple (pPrint)

sample1 :: Text
sample1 = "void func (int a, char b);"

sample2 :: Text
sample2 =
  "class A : B {\n\
  \  int method1( double x, double y );\n\
  \};"

sample3 :: Text
sample3 =
  "void func (int a, char b); \n\
  \\n\
  \class A : B {\n\
  \  int method1( double x, double y );\n\
  \};\n"

main :: IO ()
main = do
  let e1 = parseOnly function sample1
  pPrint e1
  let e2 = parseOnly class_ sample2
  pPrint e2
  let e3 = parseOnly package sample3
  pPrint e3
