{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -w #-}

module TemplateTopLevelSpec ( spec ) where

import Foreign.C.Types    ( CInt(..) )
import System.IO.Silently ( capture_ )
--
import FFICXX.Runtime.TH  ( IsCPrimitive(..), TemplateParamInfo(..) )
import STD.Vector.Template
--
import TmplTopLevelTest
import TmplTopLevelTest.TH (genReturn_vectorInstanceFor)
--
import Test.Hspec     ( Spec, afterAll, around, beforeAll, describe, it, shouldBe )

genReturn_vectorInstanceFor
  CPrim
  ( [t|CInt|], TPInfo { tpinfoCxxType       = "int"
                      , tpinfoCxxHeaders    = []
                      , tpinfoCxxNamespaces = []
                      , tpinfoSuffix        = "int"
                      }
  )

spec :: Spec
spec =
  describe "call top-level function" $ do
    it "should call ordinary top-level function" $ do
      let action = do
            ordinary :: IO ()
      s <- capture_ action
      s `shouldBe` "ordinary function is called\n"
    it "should call template top-level function" $ do
      let action = do
            return_vector @CInt (12 :: CInt) -- :: IO ()
      s <- capture_ action
      s `shouldBe` "template function is called\n"

