{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Attoparsec.Text (parseOnly)
import Data.Text (Text)
import FFICXX.Generate.IDL.Parser (idl)

sampleIdl :: Text
sampleIdl = "void func (int a, char b);"

main :: IO ()
main = do
  let e = parseOnly idl sampleIdl
  print e
