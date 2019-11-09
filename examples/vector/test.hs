{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

import qualified Data.ByteString.Char8 as B

import Foreign.C.Types
import Foreign.Ptr
import Foreign.C.String

import           STD.CppString
import           STD.Vector.Template
import qualified STD.Vector.TH as TH

-- import qualified TestTH (test)



$(TH.genVectorInstanceFor [t|CInt|] "int")
$(TH.genVectorInstanceFor [t|CppString|] "string")
--  $(TestTH.test)

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
