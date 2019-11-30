{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Concurrent (forkIO,forkOS,threadDelay)
import qualified Data.ByteString.Char8 as B

import Foreign.C.Types
import Foreign.Ptr
import Foreign.C.String

import FFICXX.Runtime.CodeGen.Cxx ( HeaderName(..), Namespace(..) )
import FFICXX.Runtime.TH (IsCPrimitive(..), TemplateParamInfo(..))
import STD.CppString
import STD.SharedPtr.Template
import qualified STD.SharedPtr.TH as TH

TH.genSharedPtrInstanceFor
  NonCPrim
  [t|CppString|]
  (TPInfo { tpinfoCxxType       = "std::string"
          , tpinfoCxxHeaders    = [ HdrName "string", HdrName "stdcxxType.h" ]
          , tpinfoCxxNamespaces = [ NS "std" ]
          , tpinfoSuffix        = "string"
          }
  )


printString :: CppString -> IO ()
printString cppstr = do
  cstr <- cppString_c_str cppstr
  bstr <- B.packCString cstr
  print bstr

worker :: SharedPtr CppString -> IO ()
worker ptr = do
  threadDelay 1000000
  c <- use_count ptr
  print c
  cppstr' <- get ptr
  printString cppstr'

main = do
  putStrLn "test"
  withCString "hello" $ \cstr -> do
    cppstr <- newCppString cstr
    ptr <- newSharedPtr cppstr

    forkIO $ worker ptr
    forkOS $ worker ptr

    threadDelay 3000000

    deleteSharedPtr ptr
