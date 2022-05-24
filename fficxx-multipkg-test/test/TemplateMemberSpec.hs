{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module TemplateMemberSpec
  ( spec,
  )
where

--
import FFICXX.Runtime.TH (IsCPrimitive (..), TemplateParamInfo (..))
import STD.UniquePtr.TH
import STD.UniquePtr.Template
import System.IO.Silently (capture_)
--
import TMFTest.A
import TMFTest.A.Implementation
import TMFTest.T1
import TMFTest.T2
--
import Test.Hspec (Spec, afterAll, around, beforeAll, describe, it, shouldBe)

genUniquePtrInstanceFor
  NonCPrim
  ( [t|T1|],
    TPInfo
      { tpinfoCxxType = "T1",
        tpinfoCxxHeaders = ["TMFTestT1.h", "tmftest.h"],
        tpinfoCxxNamespaces = [],
        tpinfoSuffix = "T1"
      }
  )

genInstanceFor_a_method
  NonCPrim
  ( [t|T1|],
    TPInfo
      { tpinfoCxxType = "T1",
        tpinfoCxxHeaders = ["TMFTestT1.h", "tmftest.h"],
        tpinfoCxxNamespaces = [],
        tpinfoSuffix = "T1"
      }
  )

genInstanceFor_a_method
  NonCPrim
  ( [t|T2|],
    TPInfo
      { tpinfoCxxType = "T2",
        tpinfoCxxHeaders = ["TMFTestT2.h", "tmftest.h"],
        tpinfoCxxNamespaces = [],
        tpinfoSuffix = "T2"
      }
  )

genInstanceFor_a_method2
  NonCPrim
  ( [t|T1|],
    TPInfo
      { tpinfoCxxType = "T1",
        tpinfoCxxHeaders = ["TMFTestT1.h", "tmftest.h"],
        tpinfoCxxNamespaces = [],
        tpinfoSuffix = "T1"
      }
  )

spec :: Spec
spec =
  describe "template member function generation" $
    it "should call template member function" $
      do
        let action = do
              a <- newA
              t1 <- newT1
              t1_print t1
              t2 <- newT2
              a_method_T1 a t1
              a_method_T2 a t2
              ptr <- newUniquePtr t1
              a_method2_T1 a ptr
        s <- capture_ action
        s `shouldBe` "I am T1.\nin A::method\nI am T1.\nin A::method\nI am T2.\nin A::method2\nI am T1.\n"
