{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TemplateHaskell          #-}

module VectorSpec ( spec ) where

import           Control.Exception           (bracket)
import qualified Data.ByteString.Char8       as B
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Ptr
--
import           FFICXX.Runtime.CodeGen.Cxx  (HeaderName (..), Namespace (..))
import           FFICXX.Runtime.TH           (IsCPrimitive (..),
                                              TemplateParamInfo (..))
import           STD.CppString
import           STD.Vector.Template
import qualified STD.Vector.TH               as TH
import           STD.VectorIterator.Template
import qualified STD.VectorIterator.TH       as TH
--
import           Test.Hspec                  (Spec, afterAll, around, beforeAll,
                                              describe, it, shouldBe)


TH.genVectorIteratorInstanceFor
  CPrim
  ( [t|CInt|], TPInfo { tpinfoCxxType       = "int"
                      , tpinfoCxxHeaders    = []
                      , tpinfoCxxNamespaces = []
                      , tpinfoSuffix        = "int"
                      }
  )

TH.genVectorInstanceFor
  CPrim
  ( [t|CInt|], TPInfo { tpinfoCxxType       = "int"
                      , tpinfoCxxHeaders    = []
                      , tpinfoCxxNamespaces = []
                      , tpinfoSuffix        = "int"
                      }
  )

TH.genVectorInstanceFor
  NonCPrim
  ( [t|CppString|], TPInfo { tpinfoCxxType       = "std::string"
                           , tpinfoCxxHeaders    = [ "string", "stdcxxType.h"]
                           , tpinfoCxxNamespaces = [ "std" ]
                           , tpinfoSuffix        = "string"
                           }
  )

spec :: Spec
spec =
  describe "FFI to vector" $ do
    beforeAll (newVector :: IO (Vector CInt)) . afterAll deleteVector $
      describe "vector<int>" $ do
        it "should be initialized" $ \v -> do
          n₀ <- size v
          n₀ `shouldBe` 0
        it "should add one item" $ \v -> do
          push_back v 1
          n₁ <- size v
          n₁ `shouldBe` 1
        it "should add 100 items" $ \v -> do
          mapM_ (push_back v) [1..100]
          n₂ <- size v
          n₂ `shouldBe` 101
        it "should remove one item" $ \v -> do
          pop_back v
          n₃ <- size v
          n₃ `shouldBe` 100
        it "should be able to retrieve an item" $ \v -> do
          item <- at v 5
          item `shouldBe` 5
        it "should retrieve an item by iterator" $ \v -> do
          iter <- increment =<< increment =<< begin v
          p <- deRef iter
          p `shouldBe` 2
    --
    beforeAll (newVector :: IO (Vector CppString)) . afterAll deleteVector $
      describe "vector<std::string>" $ do
        it "should create a C++-string and store it" $ \v -> do
          cppstr <- newCppString ("hello" :: B.ByteString)
          push_back v cppstr
          n₁ <- size v
          n₁ `shouldBe` 1
        it "should retrieve the matched string" $ \v -> do
          cppstr' <- at v 0
          cstr' <- cppString_c_str cppstr'
          bstr <- B.packCString cstr'
          bstr `shouldBe` "hello"
        it "should remove the string" $ \v -> do
          pop_back v
          n₂ <- size v
          n₂ `shouldBe` 0
