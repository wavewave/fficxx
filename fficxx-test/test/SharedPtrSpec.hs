{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module SharedPtrSpec
  ( spec,
  )
where

import Control.Concurrent (forkIO, forkOS, threadDelay)
import Control.Concurrent.Async (async, wait)
import qualified Data.ByteString.Char8 as B
--
import FFICXX.Runtime.CodeGen.Cxx (HeaderName (..), Namespace (..))
import FFICXX.Runtime.TH (IsCPrimitive (..), TemplateParamInfo (..))
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import STD.CppString
import qualified STD.SharedPtr.TH as TH
import STD.SharedPtr.Template
--
import Test.Hspec (Spec, afterAll, around, beforeAll, describe, it, shouldBe)

TH.genSharedPtrInstanceFor
  NonCPrim
  ( [t|CppString|],
    TPInfo
      { tpinfoCxxType = "std::string",
        tpinfoCxxHeaders = ["string", "stdcxxType.h"],
        tpinfoCxxNamespaces = ["std"],
        tpinfoSuffix = "string"
      }
  )

worker :: SharedPtr CppString -> IO (CInt, B.ByteString)
worker ptr = do
  c <- use_count ptr
  threadDelay 1000000
  cppstr <- get ptr
  cstr <- cppString_c_str cppstr
  bstr <- B.packCString cstr
  pure (c, bstr)

spec :: Spec
spec =
  describe "FFI to shared_ptr" $
    describe "shared_ptr<std::string>" $
      it "should pass content via shared_ptr" $
        do
          cppstr <- newCppString ("hello" :: B.ByteString)
          ptr <- newSharedPtr cppstr
          a1 <- async $ worker ptr
          a2 <- async $ worker ptr
          (_, bstr1) <- wait a1
          (_, bstr2) <- wait a2
          bstr1 `shouldBe` "hello"
          bstr2 `shouldBe` "hello"
          deleteSharedPtr ptr
