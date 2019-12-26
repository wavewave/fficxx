{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TemplateHaskell          #-}

module UniquePtrSpec where

import qualified Data.ByteString.Char8 as B
import Foreign.C.Types
import Foreign.Ptr
import Foreign.C.String
--
import FFICXX.Runtime.CodeGen.Cxx ( HeaderName(..), Namespace(..) )
import FFICXX.Runtime.TH          ( IsCPrimitive(..), TemplateParamInfo(..) )
import STD.CppString
import STD.UniquePtr.Template
import qualified STD.UniquePtr.TH as TH
--
import Test.Hspec     ( Spec, afterAll, around, beforeAll, describe, it, shouldBe )


TH.genUniquePtrInstanceFor
  NonCPrim
  ( [t|CppString|], TPInfo { tpinfoCxxType       = "std::string"
                           , tpinfoCxxHeaders    = [ "string", "stdcxxType.h" ]
                           , tpinfoCxxNamespaces = [ "std" ]
                           , tpinfoSuffix        = "string"
                           }
  )

main :: IO ()
main = do
  putStrLn "test"
  withCString "hello" $ \cstr -> do
    cppstr <- newCppString cstr
    ptr <- newUniquePtr cppstr
    cppstr' <- get ptr
    cstr' <- cppString_c_str cppstr'
    bstr <- B.packCString cstr'
    print bstr
    deleteUniquePtr ptr

spec :: Spec
spec =
  describe "FFI to unique_ptr" $ do
    describe "unique_ptr<std::string>" $ do
      it "should pass content via unique_ptr" $ do
        cppstr <- newCppString ("hello" :: B.ByteString)
        ptr <- newUniquePtr cppstr
        cppstr' <- get ptr
        cstr' <- cppString_c_str cppstr'
        bstr <- B.packCString cstr'
        bstr `shouldBe` "hello"
        deleteUniquePtr ptr
    -- print bstr
    --
