{-# LANGUAGE TemplateHaskell #-}
module Main where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.C.String

import           STD.CppString
import           STD.Function.Template
import qualified STD.Function.TH as TH
import           STD.UniquePtr.Template
import qualified STD.UniquePtr.TH as TH


$(TH.genFunctionInstanceFor ''CppString "string")
--  $(TH.genUniquePtrInstanceFor ''CppString "string")

main = do
  putStrLn "test"
  withCString "hello" $ \cstr -> do
    cppstr <- newCppString cstr
    fptr <- newFunction cppstr
    deleteFunction fptr
    -- deleteUniquePtr fptr
