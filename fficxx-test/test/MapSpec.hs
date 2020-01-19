{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TemplateHaskell          #-}

module MapSpec ( spec ) where

import Control.Exception          ( bracket )
import qualified Data.ByteString.Char8 as B
import Foreign.C.Types
import Foreign.Ptr
import Foreign.C.String
--
import FFICXX.Runtime.CodeGen.Cxx ( HeaderName(..), Namespace(..) )
import FFICXX.Runtime.TH          ( IsCPrimitive(..), TemplateParamInfo(..) )
import STD.Map.Template
import STD.Map.TH
import STD.Pair.Template
import STD.Pair.TH
--
import Test.Hspec     ( Spec, afterAll, around, beforeAll, describe, it, shouldBe )


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

genMapInstanceFor
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


spec :: Spec
spec =
  describe "FFI to map" $ do
    beforeAll (newMap :: IO (Map CInt CDouble)) . afterAll deleteMap $
      describe "map<int,double>" $ do
        it "should have no elements at first" $ \m -> do
          n <- size m
          n `shouldBe` 0
        it "should have 1 elem after insertion" $ \m -> do
          kv <- newPair 1 123.0 :: IO (Pair CInt CDouble)
          insert m kv
          n <- size m
          n `shouldBe` 1
