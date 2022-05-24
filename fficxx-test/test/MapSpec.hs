{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module MapSpec
  ( spec,
  )
where

import Control.Exception (bracket)
import qualified Data.ByteString.Char8 as B
--
import FFICXX.Runtime.CodeGen.Cxx (HeaderName (..), Namespace (..))
import FFICXX.Runtime.TH (IsCPrimitive (..), TemplateParamInfo (..))
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import STD.Map.TH
import STD.Map.Template
import STD.MapIterator.TH
import STD.MapIterator.Template
import STD.Pair.TH
import STD.Pair.Template
--
import Test.Hspec (Spec, afterAll, beforeAll, describe, it, shouldBe)

genPairInstanceFor
  CPrim
  ( [t|CInt|],
    TPInfo
      { tpinfoCxxType = "int",
        tpinfoCxxHeaders = [],
        tpinfoCxxNamespaces = [],
        tpinfoSuffix = "int"
      }
  )
  ( [t|CDouble|],
    TPInfo
      { tpinfoCxxType = "double",
        tpinfoCxxHeaders = [],
        tpinfoCxxNamespaces = [],
        tpinfoSuffix = "double"
      }
  )

genMapInstanceFor
  CPrim
  ( [t|CInt|],
    TPInfo
      { tpinfoCxxType = "int",
        tpinfoCxxHeaders = [],
        tpinfoCxxNamespaces = [],
        tpinfoSuffix = "int"
      }
  )
  ( [t|CDouble|],
    TPInfo
      { tpinfoCxxType = "double",
        tpinfoCxxHeaders = [],
        tpinfoCxxNamespaces = [],
        tpinfoSuffix = "double"
      }
  )

genMapIteratorInstanceFor
  CPrim
  ( [t|CInt|],
    TPInfo
      { tpinfoCxxType = "int",
        tpinfoCxxHeaders = [],
        tpinfoCxxNamespaces = [],
        tpinfoSuffix = "int"
      }
  )
  ( [t|CDouble|],
    TPInfo
      { tpinfoCxxType = "double",
        tpinfoCxxHeaders = [],
        tpinfoCxxNamespaces = [],
        tpinfoSuffix = "double"
      }
  )

spec :: Spec
spec =
  describe "FFI to map" $ do
    beforeAll (newMap :: IO (Map CInt CDouble)) . afterAll deleteMap $
      describe "map<int,double>" $
        do
          it "should have no elements at first" $ \m -> do
            n <- size m
            n `shouldBe` 0
          it "should have 1 elem after insertion" $ \m -> do
            kv <- newPair 1 123.0
            insert m kv
            n <- size m
            n `shouldBe` 1
          it "should retrieve value via iterator" $ \m -> do
            iter <- begin m
            p <- deRef iter
            k <- first_get p
            v <- second_get p
            (k, v) `shouldBe` (1, 123.0)
          it "should retrieve multiple values via iterator" $ \m -> do
            kv <- newPair 2 246.0
            insert m kv
            iter <- increment =<< begin m
            p <- deRef iter
            k <- first_get p
            v <- second_get p
            (k, v) `shouldBe` (2, 246.0)
