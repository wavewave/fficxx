{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -w #-}

module TemplateTopLevelSpec ( spec ) where

import System.IO.Silently ( capture_ )
--
import FFICXX.Runtime.TH  ( IsCPrimitive(..), TemplateParamInfo(..) )
--
import TmplTopLevelTest
--
import Test.Hspec     ( Spec, afterAll, around, beforeAll, describe, it, shouldBe )


spec :: Spec
spec =
  describe "call top-level function" $ do
    it "should call ordinary top-level function" $ do
      let action = do
            ordinary :: IO ()
      s <- capture_ action
      s `shouldBe` "ordinary function is called\n"

