{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

import qualified Data.ByteString.Char8 as B

import Foreign.C.Types
import Foreign.Ptr
import Foreign.C.String

import FFICXX.Runtime.CodeGen.Cxx ( HeaderName(..), Namespace(..) )
import FFICXX.Runtime.TH          ( IsCPrimitive(..), TemplateParamInfo(..) )
import STD.CppString
import STD.Vector.Template
import qualified STD.Vector.TH as TH


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
                           , tpinfoCxxHeaders    = [ HdrName "string", HdrName "stdcxxType.h"]
                           , tpinfoCxxNamespaces = [ NS "std" ]
                           , tpinfoSuffix        = "string"
                           }
  )

test1 :: IO ()
test1 = do
  v :: Vector CInt <- newVector
  n <- size v
  print =<< size v

  push_back v 1
  print =<< size v
  mapM_ (push_back v) [1..100]
  print =<< size v
  pop_back v
  print =<< size v

  print =<< at v 5
  deleteVector v


test2 = do
  withCString "hello" $ \cstr -> do
    v :: Vector CppString <- newVector
    cppstr <- newCppString cstr
    push_back v cppstr
    print =<< size v
    cppstr' <- at v 0
    cstr' <- cppString_c_str cppstr'
    bstr <- B.packCString cstr'
    print bstr
    print =<< size v
    pop_back v
    print =<< size v
    deleteVector v


main :: IO ()
main = do
  test1
  test2
