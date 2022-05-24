{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module PairSpec
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
  ( [t|CInt|],
    TPInfo
      { tpinfoCxxType = "int",
        tpinfoCxxHeaders = [],
        tpinfoCxxNamespaces = [],
        tpinfoSuffix = "int"
      }
  )

{-
-- TODO: resolve this.
-- this caused conflict with MapSpec.

genPairInstanceFor
  CPrim
  ( [t|CInt|], TPInfo { tpinfoCxxType       = "int"
                      , tpinfoCxxHeaders    = []
                      , tpinfoCxxNamespaces = []
                      , tpinfoSuffix        = "int"
                      }
  )
  ( [t|CDouble|], TPInfo { tpinfoCxxType       = "double"
                         , tpinfoCxxHeaders    = []
                         , tpinfoCxxNamespaces = []
                         , tpinfoSuffix        = "double"
                         }
  )
-}
spec :: Spec
spec =
  describe "FFI to pair" $ do
    beforeAll (newPair 1 123 :: IO (Pair CInt CInt)) . afterAll deletePair $
      describe "pair<int,int>" $
        do
          it "should get first element" $ \kv -> do
            k <- first_get kv
            k `shouldBe` 1
          it "should get second element" $ \kv -> do
            v <- second_get kv
            v `shouldBe` 123
          it "should change first element" $ \kv -> do
            first_set kv 2
            k <- first_get kv
            k `shouldBe` 2
          it "should change second element" $ \kv -> do
            second_set kv 246
            v <- second_get kv
            v `shouldBe` 246
