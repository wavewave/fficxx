{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

import qualified Data.ByteString.Char8 as B

import Foreign.C.Types
import Foreign.Ptr
import Foreign.C.String

import FFICXX.Runtime.TH (IsCPrimitive(..), TemplateParamInfo(..))
import STD.CppString
import STD.UniquePtr.Template
import qualified STD.UniquePtr.TH as TH

TH.genUniquePtrInstanceFor
  NonCPrim
  [t|CppString|]
  (TPInfo { tpinfoCxxType       = "std::string"
          , tpinfoCxxHeaders    = ["string","stdcxxType.h"]
          , tpinfoCxxNamespaces = ["std"]
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
