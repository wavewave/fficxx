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
import TmplDepTest.T2.Template
import TmplDepTest.T2.TH
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
            t1 <- newT1 :: IO (T1 CInt)
            t2 <- newT2 :: IO (T2 CInt)
            callT1 t2 t1
      s <- capture_ action
      s `shouldBe` "In T2::callT1(), calling T1::method: \nIn T1::method(), typeid(P) = i\n"
