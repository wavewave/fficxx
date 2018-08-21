{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

import qualified Data.ByteString.Char8 as B
import           Data.Foldable (for_)
import           Foreign.C.Types
import           Foreign.Ptr
import           Foreign.C.String

import           STD.CppString
import           STD.Deletable
import           STD.UniquePtr.Template
import           STD.UniquePtr.TH
import           STD.Vector.Template
import           STD.Vector.TH

import           TestPkg (test)
import           TestPkg.A
import           TestPkg.A.Implementation
import           TestPkg.B

$(genVectorInstanceFor ''CFloat "float")
$(genUniquePtrInstanceFor ''A "A")

main = do
  v :: Vector CFloat <- newVector
  mapM_ (push_back v) [1.0,1.1,1.2,1.3]
  test v

  a <- newA
  a_member_set a 3772
  m <- a_member_get a
  print m
  ptr <- newUniquePtr a
  deleteUniquePtr ptr
  -- delete a

  b <- newB
  call b v

  v' <- call2 b
  for_ [0,1,2] $ \i -> do
    x <- at v' i
    print x

  deleteVector v'


  delete b

  deleteVector v
