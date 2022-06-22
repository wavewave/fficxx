{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -w #-}

module Test.FFICXX.IDLSpec (spec) where

import Data.Attoparsec.Text (parseOnly)
import Data.Either (isRight)
import Data.Text (Text)
import FFICXX.Generate.IDL.Parser
  ( class_,
    function,
    package,
  )
import Test.Hspec
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

spec :: Spec
spec = do
  describe "parse test" $ do
    it "function" $ do
      let e = parseOnly function sample1
      isRight e `shouldBe` True
    it "class" $ do
      let e = parseOnly class_ sample2
      isRight e `shouldBe` True
    it "package" $ do
      let e = parseOnly package sample3
      isRight e `shouldBe` True
