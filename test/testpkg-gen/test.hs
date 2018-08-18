{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

import qualified Data.ByteString.Char8 as B

import           Foreign.C.Types
import           Foreign.Ptr
import           Foreign.C.String

import           STD.CppString
import           STD.Vector.Template
import qualified STD.Vector.TH as TH

import           TestPkg (test)

$(TH.genVectorInstanceFor ''CFloat "float")

main = do
  v :: Vector CFloat <- newVector
  mapM_ (push_back v) [1.0,1.1,1.2,1.3]
  test v
  deleteVector v



