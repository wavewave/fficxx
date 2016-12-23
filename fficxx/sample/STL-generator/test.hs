{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

import Foreign.C.Types
import Foreign.Ptr

import STL.Vector.Template
import qualified STL.Vector.TH as TH

 
import STL.Foo
import STL.Foo.RawType


$(TH.genVectorInstanceFor ''CInt "int")
$(TH.genVectorInstanceFor ''Foo  "Foo")

main = do
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


  --
  f <- newFoo 9
  showme f

  g <- newFoo 10
  w <- newVector
  push_back w g

  -- pop_back w

  print =<< size w

  x <- at w 0
  showme x
  deleteVector w
  
  --

  v2 <- getVector f
  print =<< size v2
  print =<< v2 `at` 0
  print =<< v2 `at` 2

  v3 <- newVector
  mapM_ (push_back v3) [1..1000]

  addContents f v3
  print =<< size v2


