{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module TemplateDepSpec
  ( spec,
  )
where

--
import FFICXX.Runtime.TH (IsCPrimitive (..), TemplateParamInfo (..))
import Foreign.C.Types (CInt)
import STD.CppString
import System.IO.Silently (capture_)
--

--
import Test.Hspec (Spec, afterAll, around, beforeAll, describe, it, shouldBe)
import TmplDepTest.T1.TH
import TmplDepTest.T1.Template
import TmplDepTest.T2.TH
import TmplDepTest.T2.Template

genT1InstanceFor
  CPrim
  ( [t|CInt|],
    TPInfo
      { tpinfoCxxType = "int",
        tpinfoCxxHeaders = [],
        tpinfoCxxNamespaces = [],
        tpinfoSuffix = "int"
      }
  )

genT1InstanceFor
  NonCPrim
  ( [t|CppString|],
    TPInfo
      { tpinfoCxxType = "std::string",
        tpinfoCxxHeaders = ["string", "stdcxxType.h"],
        tpinfoCxxNamespaces = ["std"],
        tpinfoSuffix = "string"
      }
  )

genT2InstanceFor
  CPrim
  ( [t|CInt|],
    TPInfo
      { tpinfoCxxType = "int",
        tpinfoCxxHeaders = [],
        tpinfoCxxNamespaces = [],
        tpinfoSuffix = "int"
      }
  )

genT2InstanceFor
  NonCPrim
  ( [t|CppString|],
    TPInfo
      { tpinfoCxxType = "std::string",
        tpinfoCxxHeaders = ["string", "stdcxxType.h"],
        tpinfoCxxNamespaces = ["std"],
        tpinfoSuffix = "string"
      }
  )

spec :: Spec
spec =
  describe "import template class dependency" $ do
    it "should call template function that depends on another template class, for primitive type param" $ do
      let action = do
            t1 <- newT1 :: IO (T1 CInt)
            t2 <- newT2 :: IO (T2 CInt)
            callT1 t2 t1
      s <- capture_ action
      s `shouldBe` "In T2::callT1(), calling T1::method: \nIn T1::method(), typeid(P) = i\n"
    it "should call template function that depends on another template class, for non-primitive type param" $ do
      let action = do
            t1 <- newT1 :: IO (T1 CppString)
            t2 <- newT2 :: IO (T2 CppString)
            callT1 t2 t1
      s <- capture_ action
      s `shouldBe` "In T2::callT1(), calling T1::method: \nIn T1::method(), typeid(P) = NSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEE\n"
