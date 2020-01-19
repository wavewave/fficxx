{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module TemplateDepSpec ( spec ) where

import Foreign.C.Types    ( CInt )
import System.IO.Silently ( capture_ )
--
import FFICXX.Runtime.TH  ( IsCPrimitive(..), TemplateParamInfo(..) )
--
import TmplDepTest.T2.Template
import TmplDepTest.T2.TH
--
import Test.Hspec     ( Spec, afterAll, around, beforeAll, describe, it, shouldBe )


genT2InstanceFor
  CPrim
  ( [t|CInt|], TPInfo { tpinfoCxxType       = "int"
                      , tpinfoCxxHeaders    = []
                      , tpinfoCxxNamespaces = []
                      , tpinfoSuffix        = "int"
                      }
  )

spec :: Spec
spec =
  describe "import template class dependency" $
    it "should call template function that depends on another template class" $ do
      let action = do
            t2 <- newT2 :: IO (T2 CInt)
            callT1 t2
      s <- capture_ action
      s `shouldBe` "I am T1.\nin A::method\nI am T1.\nin A::method\nI am T2.\nin A::method2\nI am T1.\n"
