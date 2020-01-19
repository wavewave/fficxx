{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module TemplateDepSpec ( spec ) where

import Foreign.C.Types    ( CInt )
import System.IO.Silently ( capture_ )
--
import FFICXX.Runtime.TH  ( IsCPrimitive(..), TemplateParamInfo(..) )
--
import TmplDepTest.T1.Template
import TmplDepTest.T1.TH
-- import TmplDepTest.A.Implementation
-- import TMFTest.T1
-- import TMFTest.T2
--
import Test.Hspec     ( Spec, afterAll, around, beforeAll, describe, it, shouldBe )


genT1InstanceFor
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
            t1 <- newT1 :: IO (T1 CInt)
            method t1
      s <- capture_ action
      s `shouldBe` "I am T1.\nin A::method\nI am T1.\nin A::method\nI am T2.\nin A::method2\nI am T1.\n"
